structure Compiler  =
struct

  open TpAbSyn

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)
  exception Error2 of string

  type Pos = int * int (* (line, column) *)

  (* ... rest will all be filled/copied in successively (top-down)... *)
  val unimplemented = Error ("not implemented", (0,0))

  (* Name generator.  Call with, e.g., t1 = "tmp"^newName () *)
  val counter = ref 0
  fun newName () = (counter := !counter + 1;
                  "_" ^ Int.toString (!counter)^ "_")

  (* Number to text with Mars-compatible sign symbol *)
  fun makeConst n = if n>=0 then Int.toString n
                    else "-" ^ Int.toString (~n)

  (* link register *)
  val RA = "31"
  (* Register for stack pointer *)
  val SP = "29"
  (* Register for heap pointer *)
  val HP = "28"
  (* Register for frame pointer *)
  val FP = "25"

  (* Suggested register division *)
  val minReg = 2       (* lowest allocatable register *)
  val maxCaller = 15   (* highest caller-saves register *)
  val maxReg = 24      (* highest allocatable register *)



  (***********************************************************************)
  (*** `VTab' associates the name of a variable with with the name of  ***)
  (***        the register that records its value in the generated code***)
  (***********************************************************************)
  type VTab = (string * string) list

  fun extend  ((Dec ((n,_),_)) , vtable) =
      SymTab.bind n (n ^ "_local_" ^ newName()) vtable

  (***********************************************************************)
  (*** for LValues: `Reg r' denotes that register `r' holds a value    ***)
  (***              `Mem r' denotes that register `r' holds the        ***)
  (***                 memory location which is to be read/written,    ***)
  (***                 e.g., compiling both ...      := a[i1,i2]       ***)
  (***                       and            a[i1,i2] := ...            ***)
  (***                       requires the memory location of a[i1,i2]  ***)
  (***********************************************************************)
  datatype Location = Reg of string (* in given register *)
                    | Mem of string (* at memory address held in register *)

  (*****************************************************************)
  (*** representing constant string as arrays                    ***)
  (*****************************************************************)
  (* Table storing all string literals, with labels given to them *)
  val stringTable = ref []

  (* Building a string in the heap, including initialisation code *)
  fun buildString (label, str)
    = let val data = [Mips.ALIGN "2"   (* means: word-align *)
                     ,Mips.LABEL label (* pointer *)
                     ,Mips.SPACE "8"   (* size(int) and data pointer (word) *)
                     ,Mips.ASCII str]
          val initR = label^ "_init"
          val addrR = label^ "_addr"
          val initcode = [ Mips.LA(addrR, label)
                         , Mips.LI(initR, makeConst (String.size str))
                         , Mips.SW(initR, addrR, makeConst 0 )
                         , Mips.ADDI(initR, addrR, makeConst 8)
                         , Mips.SW(initR, addrR, makeConst 4 )]
      in (initcode, data)
      end

  (********************************************************************)
  (*** compileExp(vtab : VTab, e : Exp, place : string) : mips list ***)
  (*** Input: 1. `vtab' associates a `var' with `loc : Location',   ***)
  (***           i.e., the register that either stores the value in ***)
  (***           case `loc' is `Reg of r', or the memory location   ***)
  (***           from which the value is to be read/written in case ***)
  (***           `loc' is `Mem of r'                                ***)
  (***        2. `e' is the expression for which we generate code   ***)
  (***        3. `place' is the register that will store the result ***)
  (***            of the current expression `e'                     ***)
  (*** Output: generated code as a list of mips instructions        ***)
  (********************************************************************)

  and compileExp( vtable, Literal(BVal (Num n), _), place ) =
        if n < 32768
        then [ Mips.LI  (place, makeConst n) ]
        else [ Mips.LUI (place, makeConst (n div 65536)),
               Mips.ORI (place, place, makeConst (n mod 65536)) ]

    | compileExp( vtable, Literal(BVal (Log b), _), place ) =
        [ Mips.LI (place,makeConst (if b then 1 else 0)) ]

    | compileExp( vtable, Literal(BVal (Chr c), _), place ) =
        [ Mips.LI (place,makeConst (ord c)) ]

    | compileExp( vtable, Literal(Arr _, pos), place ) =
        raise Error("Array Values are Currently Unimplemented, at ", pos)

    | compileExp( vtable, StrLit(strlit, pos), place ) =
        let val normalChars =
                    (List.filter Char.isAlpha (String.explode(strlit)))
                  @ String.explode "__str__"
            val label = String.implode(List.take (normalChars,7)) ^ newName()
            val ()    = stringTable := (label,strlit)::(!stringTable)
        in  [ Mips.LA (place, label),
              Mips.COMMENT (label^": string \""^ String.toCString strlit ^ "\"") ]
        end

  (***************************************************************************)
  (*** Array layout is as follows:                                         ***)
  (***         [ d_1, d_2, ..., d_r, s_1, ..., s_{r-1}, ptr ]              ***)
  (***   where `r' is the rank of the array, i.e., number of dimensions    ***)
  (***         `d_i' is the size of dimension `i', with i in {1,..,r}      ***)
  (***         `s_i' is the stride of dimension `i', where                 ***)
  (***               s_r = 1 not recorded, and s_i = s_{i+1} * d{i+1}      ***)
  (***         `N = d_1*d_2*...*d_r' is the total number of elements       ***)
  (***              of the (flat) array and is adjusted to be a multiple   ***)
  (***              of 4, i.e., alligned, when array's type is Bool/Char   ***)
  (***         `ptr' points to an alligned chunk of memory of size         ***)
  (***            `N*sizeOf(element Type)' rounded up to a multiple of 4,  ***)
  (***            which contains the elements of the flat array, i.e.,     ***)
  (***            [e_1, ..., e_N]                                          ***)
  (***************************************************************************)
    | compileExp( vtable, ArrLit(elms, arrtp, pos), place ) =
        let val shape   = mkShape ( ArrLit (elms, arrtp, pos) )
            val strides = List.take( mkStrides shape, (length shape) - 1 )
            val explst  = flattenArrRep ( ArrLit (elms, arrtp, pos) )
            val arr_rank= length shape

            val tot_size= foldl (fn (x,p) => x*p) 1 shape
            val one_byte= case basicType arrtp of
                            BType Int  => false
                          | _          => true
            val byte_algn_size=
                    if one_byte then ((tot_size + 3) div 4) * 4
                                else tot_size * 4

            val tmp_reg = "_tmp_" ^ newName()
            val dat_reg = "_tmp_" ^ newName()

            (* write the array dimensions and strides *)
            val header0 = [ Mips.MOVE(place, HP),
                            Mips.ADDI(HP, HP, makeConst (8*arr_rank + byte_algn_size)),
                            Mips.MOVE(tmp_reg, place) ]
            val header1 = foldr (fn (d,l) => [ Mips.LI  (dat_reg, makeConst d ),
                                               Mips.SW  (dat_reg, tmp_reg, "0"),
                                               Mips.ADDI(tmp_reg, tmp_reg, "4") ] @ l
                                ) [] (shape @ strides)

            val header  = header0 @ header1 @
                          [ Mips.ADDI(tmp_reg, tmp_reg, "4" ),
                            Mips.SW  (tmp_reg, tmp_reg, "-4") ]

            (* tmp_reg now points to the first element of the flat array *)
            (* we write the elements with a fold, i.e., completely unrolled loop *)
            val e_code  = foldr (fn (e,l) => compileExp(vtable, e, dat_reg)
                                             @ ( if one_byte
                                                 then [ Mips.SB  (dat_reg, tmp_reg, "0"),
                                                        Mips.ADDI(tmp_reg, tmp_reg, "1") ]
                                                 else [ Mips.SW  (dat_reg, tmp_reg, "0"),
                                                        Mips.ADDI(tmp_reg, tmp_reg, "4") ] )
                                             @ l
                                ) [] explst
        in  header @ e_code
        end

    | compileExp( vtable, LValue (lv,pos), place ) =
        let val (code, loc) = compileLVal(vtable, lv, pos)
            val tp = typeOfExp( LValue(lv,pos) )
        in case loc of
             Reg r => [ Mips.MOVE (place,r) ]
           | Mem x => case tp of
                        BType Char  => code @ [Mips.LB (place,x,"0")]
                      | BType Bool  => code @ [Mips.LB (place,x,"0")]
                      | other       => code @ [Mips.LW (place,x,"0")]
        end

    | compileExp( vtable, Plus (e1, e2, _), place ) =
        let val t1 = "plus1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "plus2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
        in c1 @ c2 @ [Mips.ADD (place,t1,t2)]
        end

    | compileExp( vtable, Minus(e1, e2, _), place ) =
        let val t1 = "minus1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "minus2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
        in c1 @ c2 @ [Mips.SUB (place,t1,t2)]
        end

    | compileExp( vtable, Times(e1, e2, _), place ) =
        let val t1 = "times1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "times2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
        in c1 @ c2 @ [Mips.MUL (place,t1,t2)]
        end

    | compileExp( vtable, Div(e1, e2, _), place ) =
        let val t1 = "div1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "div2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
        in c1 @ c2 @ [Mips.DIV (place,t1,t2)]
        end

    | compileExp( vtable, Equal(e1, e2, _), place ) =
    (* Check if arguments are arrays *)
        let val tp1 = typeOfExp e1
            val isArray = 
                  case tp1 of
                  Array _ => true
                  | _ => false
            val t1 = "eq1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "eq2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
            val lEq = "_equal_" ^ newName()
    (* If arguments are arrays, get rank and type and initiate appropiate 'variables' *)
        in if isArray
           then
           let val (rnk,btp) = 
                   case tp1 of
                   Array(d, b) => (d,b)
                   | _ => raise Error2("Something wrong with array input")
               val endOf    = "_endOf_" ^ newName()
               val nextElm = "_nextElm_" ^ newName()
               val ctlDgt  = "_ctlDgt_" ^ newName()
               val chk_shp = "_chk_shp_" ^ newName()
               val chk_elm = "_chk_elm_" ^ newName()
               val shpOk   = "_shpOk_" ^ newName()
               val elmOk   = "_elmOk_" ^ newName()
               val t3      = "_t3_" ^ newName()
               val t4      = "_t4_" ^ newName()
               val t5      = "_t5_" ^ newName()
           in
    (* MIPS code to check if shape is the same for the arrays *)
            c1 @ c2 @ (case btp of 
                       Int => [ Mips.ADDI (nextElm, "0", makeConst 4)]
                       | _ => [ Mips.ADDI (nextElm, "0", makeConst 1)] ) @
           [ Mips.LI (place, "0"),
             Mips.ADDI (endOf, "0", "0"),
             Mips.ADDI (t5, "0", "1"),
             Mips.ADDI (endOf, endOf, makeConst rnk),
             Mips.SLL (endOf, endOf, makeConst 2),
             Mips.LABEL chk_shp,
             Mips.BEQ (endOf, "0", shpOk),
             Mips.LW (t3, t1, makeConst 0),
             Mips.LW (t4, t2, makeConst 0),
             Mips.ADDI (t1, t1, makeConst 4),
             Mips.ADDI (t2, t2, makeConst 4),
             Mips.ADDI (endOf, endOf, makeConst ~4),
             Mips.MUL (t5, t5, t3),
             Mips.BNE (t3, t4, lEq),
             Mips.J chk_shp,
             Mips.LABEL shpOk,
    (* MIPS code to check if data is the same in the arrays *)
             Mips.ADDI (endOf, "0", makeConst rnk),
             Mips.SLL (endOf, endOf, makeConst 2),
             Mips.ADD (t1, t1, endOf),
             Mips.ADD (t2, t2, endOf),
             Mips.MUL (endOf, t5, nextElm),
             Mips.LABEL chk_elm,
             Mips.BEQ (endOf, "0", elmOk),
             Mips.LB (t3, t1, makeConst 0),
             Mips.LB (t4, t2, makeConst 0),
             Mips.ADD (t1, t1, nextElm),
             Mips.ADD (t2, t2, nextElm),
             Mips.SUB (endOf, endOf, nextElm),
             Mips.BNE (t3, t4, lEq),
             Mips.J chk_elm,
             Mips.LABEL elmOk,
             Mips.LI (place, "1"),
             Mips.LABEL lEq]
           end
           else c1 @ c2 @
           [ Mips.LI (place,"1"), Mips.BEQ (t1, t2, lEq),
             Mips.LI (place,"0"), Mips.LABEL lEq]
        end

    | compileExp( vtable, Less (e1, e2, _), place ) =
        let val t1 = "less1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "less2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
        in c1 @ c2 @ [Mips.SLT (place,t1,t2)]
        end

    | compileExp( vtable, And(e1, e2, _), place ) =
        let val t1 = "and1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "and2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
            val lA = "_and_" ^ newName()
        in c1 (* do first part, skip 2nd part if already false *)
           @ [Mips.MOVE (place, t1), Mips.BEQ (place, "0", lA) ]
           @ c2 (* when here, t1 was  true, so the result is t2 *)
           @ [Mips.MOVE (place, t2), Mips.LABEL lA ]
        end

    | compileExp( vtable, Or(e1, e2, _), place ) =
        let val t1 = "or1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
            val t2 = "or2_" ^ newName()
            val c2 = compileExp(vtable, e2, t2)
            val lA = "_or_" ^ newName()
        in c1 (* do first part, skip 2nd part if already true *)
           @ [ Mips.MOVE (place, t1), Mips.BNE (place, "0", lA) ]
           @ c2 (* when here, t1 was false, so the result is t2 *)
           @ [ Mips.MOVE (place, t2), Mips.LABEL lA ]
        end

    | compileExp( vtable, Not(e1, _), place ) =
        let val t1 = "not1_" ^ newName()
            val c1 = compileExp(vtable, e1, t1)
        in c1 @ [ Mips.XORI (place, t1, "1") ]
        end

    | compileExp( vtab, FunApp (("len",(_,_)),args,pos), place ) =
       ( case args of
           [d, arr]=> let val a_reg = "_tmp_"^newName()
                          val cond_reg = "_cond_"^newName()
                          val (line,_) = pos
                          val rank  = case typeOfExp arr of
                                        Array(d, btp) => d
                                      | tp => raise Error("type checker bug: second arg of len "^
                                                          "not an array, i.e., type: "^pp_type tp,pos)
                      in    compileExp( vtab, d,     place)
                          @ [ Mips.SLTI(cond_reg, place, makeConst rank),  (* check 0 <= d < rank *)
                              Mips.LI(a_reg, "-1"),
                              Mips.SLT(a_reg, a_reg, place),
                              Mips.AND(cond_reg, cond_reg, a_reg),
                              Mips.LI("5", makeConst line),
                              Mips.BEQ(cond_reg, "0", "_IllegalArrIndexError_")
                            ]
                          @ [ Mips.SLL(place, place,  "2" ) ]
                          @ compileExp( vtab, arr,   a_reg)
                          @ [ Mips.ADD(a_reg, a_reg, place),
                              Mips.LW (place, a_reg,  "0" ) ]
                      end
         | other   => raise Error("function len requires two arguments, at ", pos)
       )

    | compileExp( vtab, FunApp (("read",(_,rtp)),args,pos), place ) =
        let val sanity_check = if null args then true
                               else raise Error("read called with non-empty args, at ", pos)
        in  case rtp of
              SOME (BType Int)  => [ Mips.JAL ("readInt",  ["2"]),
                                     Mips.MOVE(place,      "2" ) ]

            | SOME (BType Char) => [ Mips.JAL ("readChar", ["2"]),
                                     Mips.MOVE(place,      "2" ) ]

            | SOME (BType Bool) =>
                let val fl = "_false_lab_"^newName()
                in [ Mips.JAL ("readInt", ["2"]), Mips.MOVE( place, "2" ),
                     Mips.BEQ ( place, "0", fl ), Mips.LI  ( place, "1" ), Mips.LABEL fl ]
                end
            | othertype  => raise Error("read called for a non-basic type, at ", pos)
        end

    | compileExp( vtab, FunApp (("new", (_,rtp)),dims,pos), place ) =
        let val tmp_reg = "_tmp_" ^newName()
            val   e_reg = "_ereg_"^newName()
            val loop_beg= "_loop_beg_"^newName()
            val loop_end= "_loop_end_"^newName()
            val check_sz= "_check_"^newName()
            val (line,_)= pos

            fun checkDimGT0(res_reg, dim_reg) =
                let val cond_reg = "_cond_"^newName()
                in  [ Mips.SLT(cond_reg, "0", dim_reg), Mips.AND(res_reg, res_reg, cond_reg) ]
                end

            val arr_rank = length   dims
            val elm_type = case rtp of
                             SOME tp => ( case basicType tp of
                                            BType btp => btp
                                          | _         => raise Error("Impossible case in "^
                                                                     "new array, at ", pos) )
                           | NONE    => raise Error("new does not have return type, at ", pos)

            val alloc_header= [ Mips.LI  (check_sz, "1"),
                                Mips.MOVE(place, HP),
                                Mips.ADDI(HP, HP, makeConst (8*arr_rank)),
                                Mips.LI  (tmp_reg, "1") ]

            (*  The code below fills in the metadata of an array, i.e., the
                array's shape and strides.  The pseudocode is:

                    tmp = 1;
                    for (i = R-1, i>0; i--) {       // R is the rank of the array
                        tmp        = tmp * dims[i];
                        arr[i]     = dims[i];       // set array dims
                        arr[R+i-1] = tmp;           // set array strides
                    }
                    arr[0] = dims[0];
                    arr[2*R] = arr + (2*R+1)

                but in our case `dims' is not represented as a Mips array,
                but rather we completely unroll the loop, since the number of
                dimension (rank) is statically known, i.e., length dims.
            *)
            val ind_count  = tl (List.tabulate(arr_rank, fn x => x))
            val code_strides= foldl (fn ((e,i),code) =>
                                        let val code_e = compileExp(vtab, e, e_reg)
                                            val code_d = [  Mips.SW  (e_reg,   place,   makeConst (i*4) ),
                                                            Mips.MUL (tmp_reg, tmp_reg, e_reg             ),
                                                            Mips.SW  (tmp_reg, place,   makeConst ((arr_rank+i-1)*4)) ]
                                        in code_e @ checkDimGT0(check_sz, e_reg) @ code_d @ code
                                        end
                                    ) [] ( ListPair.zip (tl dims,ind_count) )

            (* fill in the first dimension and compute the total size of the (flattened) array *)
            val code_e = compileExp(vtab, (hd dims), e_reg)
            val all_dimstrd = code_strides @ code_e @ checkDimGT0(check_sz, e_reg)
                              @ [ Mips.LI("5", makeConst line),  (* if length < 0 then error *)
                                  Mips.BEQ(check_sz, "0", "_IllegalArrSizeError_") ]
                              @ [ Mips.SW (e_reg,  place, "0"),
                                  Mips.MUL(tmp_reg,tmp_reg, e_reg),
                                  Mips.SW (HP, place, makeConst (4*(2*arr_rank-1)) ) ]

            (* tmp_reg now stores the size of the whole array *)
            (* and we make e_reg to store the array size in bytes *)
            val code_tp = case elm_type of
                            Int  =>   [ Mips.SLL (e_reg, tmp_reg, "2") ]
                          | other=>   (* align to 4 byte boundary *)
                                      [ Mips.MOVE(e_reg, tmp_reg   ),
                                        Mips.ADDI(e_reg, e_reg, "3"),
                                        Mips.SRA (e_reg, e_reg, "2"),
                                        Mips.SLL (e_reg, e_reg, "2") ]

            val alloc_code = alloc_header @ all_dimstrd @ code_tp @
                             [ Mips.MOVE(tmp_reg, HP), Mips.ADD(HP, HP, e_reg) ]

            (* loop that sets the elements of the array to *)
            (* `flat_arr_elems' or to default 0 if NONE    *)
            (* tmp_reg iterates through the array elements, up until HP is reached *)
            val loop_code = [ Mips.LABEL loop_beg ,
                              Mips.BEQ(tmp_reg, HP, loop_end) ] @
                            ( case elm_type of
                                Int => [ Mips.SW("0", tmp_reg, "0"),
                                         Mips.ADDI(tmp_reg, tmp_reg, "4") ]
                              | _   => [ Mips.SB("0", tmp_reg, "0"),
                                         Mips.ADDI(tmp_reg, tmp_reg, "1") ] ) @
                            [ Mips.J loop_beg, Mips.LABEL loop_end ]

            (* val () = print (concat (map (fn x => Mips.pp_mips x^"\n") loop_code)) *)
        in  alloc_code @ loop_code
        end

    | compileExp( vtable, FunApp ((f,_),es,_), place ) =
        let val (mvcode,maxreg) = putArgs es vtable minReg
            val regs = List.tabulate ( maxreg - minReg,
                                       makeConst o (fn x => x + minReg) )

        in  mvcode
          @ [ Mips.JAL (f,regs) ]
          @ [ Mips.MOVE (place, "2") ]
        end


  (* move args to callee registers *)
  and putArgs [] vtable reg =
        ([], reg)
    | putArgs (e::es) vtable reg =
      let
          val t1 = "_funarg_"^newName()
          val code1 = compileExp(vtable, e, t1)
          val (code2, maxreg) = putArgs es vtable (reg+1)
      in
          (   code1                          (* compute arg1 *)
            @ code2                          (* compute rest *)
            @ [Mips.MOVE (makeConst reg,t1)] (* store in reg *)
            , maxreg)
      end

  (* move args from callee registers *)
  and moveBackArgs [] vtable reg p = []
    | moveBackArgs (e::es) vtable reg p =
      let
          val uncode2 = moveBackArgs es vtable (reg+1) p
          val moveBack = case e of
                             LValue (Var (name, _), _) =>
                             let val x = SymTab.tryLookup name vtable p
                             in [Mips.MOVE (x, makeConst reg)]
                             end
                           | _ => []
      in
          uncode2 @ moveBack
      end

  and compileLVal( vtab : VTab, Var (n,_) : LVAL, pos : Pos ) : Mips.mips list * Location =
        ( case SymTab.lookup n vtab of
            SOME reg => ([], Reg reg)
          | NONE     => raise Error ("unknown variable "^n, pos) )

    | compileLVal( vtab : VTab, Index ((n,_),  []) : LVAL, pos : Pos ) =
        raise Error("variable "^"n"^" with empty index, at ", pos)
 
    | compileLVal( vtab : VTab, Index ((n,t),inds) : LVAL, pos : Pos ) =
        ( case SymTab.lookup n vtab of
            SOME arr_reg =>
                let val tmp_reg   = "_tmp_" ^newName()
                    val e_reg     = "_ereg_"^newName()
                    val strd_reg  = "_tmp_" ^newName()
                    val ind_check = "_safe_"^newName()
                    val (line, _) = pos
                    val el_type   = typeOfExp (LValue( Index((n,t),inds), pos ))
                    val arr_rank  = length inds

                    fun checkBounds(res_reg, ind_reg, arr_reg, pos : int) =
                        let val cond_reg = "_cond_"^newName()
                        in [Mips.LI (cond_reg, "-1"),
                            Mips.SLT(cond_reg, cond_reg, ind_reg),
                            Mips.AND(res_reg, res_reg, cond_reg),
                            Mips.LW (cond_reg, arr_reg, makeConst (4*pos)),
                            Mips.SLT(cond_reg, ind_reg, cond_reg),
                            Mips.AND(res_reg, res_reg, cond_reg)
                           ]
                        end


                    (* code that computes the last index *)
                    val last_ind = List.nth( inds, arr_rank-1 )
                    val last_ind = compileExp(vtab, last_ind, tmp_reg)
                    val code_last_ind = [Mips.LI(ind_check, "1")] @ last_ind @
                                        checkBounds(ind_check, tmp_reg, arr_reg, arr_rank-1)

                    (* the flat index of [i_1,..,i_r], hld in tmp_reg, is 
                        i_r + i_{r-1}*stride_{r-1} + .. + i_1 * stride_1   *)
                    val ind_int_pairs = ListPair.zip (inds, List.tabulate(arr_rank-1, fn x => x) )
                    val code_flat_ind= code_last_ind @
                        foldl( fn ((e,i),code) =>
                            let val offset = 4*(i + arr_rank)
                                val code_e = compileExp(vtab, e, e_reg)
                            in    code_e
                                @ checkBounds(ind_check, e_reg, arr_reg, i)
                                @ [ Mips.LW (strd_reg, arr_reg, makeConst offset),
                                    Mips.MUL(e_reg, e_reg, strd_reg),
                                    Mips.ADD(tmp_reg, tmp_reg, e_reg) ]
                                @ code
                            end
                        ) [] ind_int_pairs

                    (* multiply by 4 if element type is Int, stored in tmp_reg *)
                    val code_byte_ind = code_flat_ind
                        @ [ Mips.LI("5", makeConst line),  (* if index check fails, error *)
                            Mips.BEQ(ind_check, "0", "_IllegalArrIndexError_") ]
                        @ ( case el_type of
                              BType Int => [ Mips.SLL (tmp_reg, tmp_reg, "2") ]
                            | _         => [] )

                    (* finally get the content of the array and add the byte index to it *)
                    val offset = 4 * (2 * arr_rank - 1)
                    val code = code_byte_ind @
                                [ Mips.LW (e_reg, arr_reg, makeConst offset),
                                  Mips.ADD(e_reg, e_reg, tmp_reg) ]
                    (* val () = print (concat (map Mips.pp_mips code)) *)
                in ( code, Mem e_reg )
                end
          | NONE     => raise Error ("unknown variable "^n, pos) )


  (* instr.s for one statement. exitLabel is end (for control flow jumps) *)
  and compileStmt( vtab, ProcCall (("write",_), [e], pos), _ ) =
        let val place    = "_dat_"^newName()
            val code_arg = compileExp(vtab, e, place)
        in case typeOfExp e of
             BType Int    => code_arg @ [ Mips.MOVE("2",place), Mips.JAL("writeInt", ["2"]) ]
           | BType Char   => code_arg @ [ Mips.MOVE("2",place), Mips.JAL("writeChar",["2"]) ]
           | BType Bool   => code_arg @ [ Mips.MOVE("2",place), Mips.JAL("writeInt", ["2"]) ]
           | Array(1,btp) => (* write one element at a time *)
                let val arr_end  = "_arr_end_"^newName()
                    val loop_beg = "_write_loop_beg"^newName()
                    val loop_end = "_write_loop_end"^newName()
                in  code_arg
                  @ [ Mips.LW(arr_end, place, "0") ]
                  @ ( case btp of Int => [Mips.SLL(arr_end, arr_end, "2")] | _ => [] )
                  @ [ Mips.LW (place,   place,   "4"  ),
                      Mips.ADD(arr_end, arr_end, place),
                      Mips.LABEL(loop_beg),
                      Mips.BEQ(place, arr_end, loop_end),
                      Mips.LB("2", place, "0")
                    ]
                  @ ( case btp of
                        Int  => [ Mips.JAL ("writeInt",  ["2"]),
                                  Mips.ADDI(place, place, "4" ) ]

                      | Char => [ Mips.JAL ("writeChar", ["2"]),
                                  Mips.ADDI(place, place, "1" ) ]

                      | Bool => [ Mips.JAL ("writeInt",  ["2"]),
                                  Mips.ADDI(place, place, "1" ) ]
                    )
                  @ [ Mips.J    (loop_beg),
                      Mips.LABEL(loop_end)
                    ]
                end
           | othertype    => raise Error("write for multi-dim arrays not implemented, at ", pos)
        end
    | compileStmt( vtab, ProcCall (("write",_), _,   pos), _ ) =
        raise Error("procedure write should receive only 1 argument, at ", pos)

    | compileStmt(vtable, s, exitLabel) =
      case s of
          Return (NONE, p) => [ Mips.J exitLabel ]
        | Return (SOME e, p) =>
          let val t     = "_return_"^newName()
              val code0 = compileExp(vtable, e, t)
          in code0 @ [ Mips.MOVE ("2",t), Mips.J exitLabel ]
          end
        | ProcCall ((n,_), es, p) => 
          let
              val (mvcode, maxreg) = putArgs es vtable minReg
              val unmvcode = moveBackArgs es vtable minReg p
          in
              mvcode @ [Mips.JAL (n, List.tabulate (maxreg, makeConst))] @ unmvcode
          end
        | Assign (lv, e, p) =>
          let val (codeL,loc) = compileLVal(vtable, lv, p)
              val t = typeOfExp ( LValue(lv,p) )
              val ereg  = "_assign_" ^ newName()
              val codeE = compileExp(vtable, e, ereg)
          in case (t,loc) of
                 (BType Char,Reg x) => codeL @ codeE @ [ Mips.ANDI (x,ereg,"255") ]
               | (    _     ,Reg x) => codeL @ codeE @ [ Mips.MOVE (x, ereg) ]
               | (BType Char,Mem a) => codeL @ codeE @ [ Mips.SB (ereg, a, "0") ]
               | (BType Bool,Mem a) => codeL @ codeE @ [ Mips.SB (ereg, a, "0") ]
               | (    _     ,Mem a) => codeL @ codeE @ [ Mips.SW (ereg, a, "0") ]
          end
        | IfThEl (e,blockT,blockF,p) =>
          let val ereg  = "_if_" ^ newName()
              val els   = "_else_" ^ newName()
              val endl  = "_endif_" ^ newName()
              val codeE = compileExp(vtable, e, ereg)
              val codeT = compileStmts blockT vtable exitLabel
              val codeF = compileStmts blockF vtable exitLabel
          in codeE @ [ Mips.BEQ (ereg, "0", els) ]
             @ codeT @ [ Mips.J endl]
             @ ( Mips.LABEL els) :: codeF @ [ Mips.LABEL endl ]
          end
        | While (e,block,p) =>
          let val eReg  = "_while_" ^ newName()
              val entry = "_wEntry_" ^ newName()
              val exit  = "_wExit_" ^ newName()
              val eCode = compileExp(vtable, e, eReg)
              val bCode = compileStmts block vtable exitLabel
          in [ Mips.LABEL entry ] @ eCode @ [ Mips.BEQ (eReg, "0", exit) ]
             @ bCode @ [ Mips.J entry, Mips.LABEL exit ]
          end

  (* generate statements for an entire block.
     exitLabel is end (for control flow jumps) *)
  and compileStmts (Block (ds,ss)) vtable exitLabel =
      let val vtable' =  List.foldr extend vtable ds (* add local decs ds to vtable *)
      in List.concat (List.map (fn s => compileStmt(vtable', s, exitLabel)) ss)
      end

  (* code for saving and restoring callee-saves registers *)
  fun stackSave currentReg maxReg savecode restorecode offset =
    if currentReg > maxReg
    then (savecode, restorecode, offset)  (* done *)
    else stackSave (currentReg+1)
                   maxReg
                   (Mips.SW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: savecode) (* save register *)
                   (Mips.LW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: restorecode) (* restore register *)
                   (offset+4) (* adjust offset *)

  (* add function arguments to symbol table *)
  and getMovePairs     []      vtable   _     = ([], vtable) 
    | getMovePairs (Dec ((v,t),p)::vs) vtable nextReg =
           if nextReg > maxCaller
             then raise Error ("Too many arguments (max  " ^ 
                               makestring (maxCaller-minReg) ^ "!", p)
             else
               let val vname = v ^ "_arg_" ^ newName()
                   val vtable1 = SymTab.bind v vname vtable
                   val (pairs2, vtable2) = getMovePairs vs vtable1 (nextReg + 1)
               in ((vname, makeConst nextReg) :: pairs2, vtable2)
               end

  and compileF (isProc, fname, args, block, pos) =
      (* at this point, we do not care about the return type (or no return) *)
      let (* make a vtable from bound formal parameters,
               then evaluate expression in this context, return it *)
          (* arguments passed in registers, "move" into local vars.
             Code generator imposes max. 13 arguments (maxCaller-minReg)
           *)
          val () = if length args <= maxCaller - minReg + 1 then ()
                   else raise Error (fname ^ ": too many arguments (max "
                                     ^ makestring (maxCaller - minReg + 1)
                                     ^")", pos)
          val (movePairs, vtable) = getMovePairs args [] minReg
          val copy_in  = map (fn (v, reg) => Mips.MOVE (v, reg)) movePairs
          val copy_out = if isProc
                         then map (fn (v, reg) => Mips.MOVE (reg, v)) movePairs
                         else []
          val body = compileStmts block vtable (fname ^ "_exit")
          val (body1, _, maxr, spilled) =  (* call register allocator *)
              RegAlloc.registerAlloc 
                  (copy_in @ body @ (* copy-in, body, exit label, copy-out *)
                   [(Mips.LABEL (fname^"_exit"))] @ copy_out )
                                     ["2"] minReg maxCaller maxReg 0
                                     (* 2 contains return val*)
          val (savecode, restorecode, offset) = (* save/restore callee-saves *)
              stackSave (maxCaller+1) maxr [] [] (4*spilled)
        in  [Mips.COMMENT ("Function " ^ fname),
             Mips.LABEL fname,       (* function label *)
             Mips.SW (RA, SP, "-4"), (* save return address *)
             Mips.ADDI (SP,SP,makeConst (~4-offset))] (* move SP "up" *)
          @ savecode                 (* save callee-saves registers *)
          @ body1                    (* code for function body *)
            (* previously, the _exit label (jump target for the return 
               statement) was here. With call-by-value-result, we must
               not jump over the copy-out code we created, so it is now
               inside the "body1" code block, see above *)
          @ restorecode              (* restore callee-saves registers *)
          @ [Mips.ADDI (SP,SP,makeConst (4+offset))] (* move SP "down" *)
          @ [Mips.LW (RA, SP, "-4"),  (* restore return addr *)
             Mips.JR (RA, [])]       (* return *)
        end

(* Stack layout upon entering body1:
      ----------------
      |    space     | <-new SP
      |      to      |        |
      |     spill    |        | 4 * spilled
      |--------------|        | (initial offset for stackSave)
      | calleeSav 1  | <------+
      | calleeSav 2  |
      |     ...      |
      | return addr  | <------final offset goes here
      |--------------|
      |     ...      | <- old SP
 *)

  (* compiling functions and procedures *)
  fun compileFun (Func (resty, fname, args, block, pos)) =
      compileF (false, fname, args, block, pos)
    | compileFun (Proc (       fname, args, block, pos)) =
      compileF (true, fname, args, block, pos)

  (* compile program *)
  fun compile funs =
    let val () = stringTable := []
        val funsCode = List.concat (List.map compileFun funs)
        val (stringinit_sym, stringdata) =
                ListPair.unzip (List.map buildString (!stringTable))

        val (stringinit, _, _, _) =
            ( case stringinit_sym of
                []    => ([],[],maxCaller,0)
              | other => RegAlloc.registerAlloc (* call register allocator *)
                            (List.concat stringinit_sym)
                            ["2"] minReg maxCaller maxReg 0
            )
        (* val stringinit = List.concat stringinit_sym *)

    in [  Mips.TEXT "0x00400000",
          Mips.GLOBL "main"]

       @ (Mips.LA (HP, "_heap_"):: stringinit) (* initialise heap and string pointers *)

       @ [ Mips.JAL ("main",[]),    (* run program *)
           Mips.LABEL "_stop_",     (* runtime errors will jump here & also return from main *)
           Mips.LI ("2","10"),      (* syscall exit *)
           Mips.SYSCALL ]

       @ funsCode                  (* code for functions *)

       (* pre-defined ord: char -> int and chr: int -> char *)
       @ [Mips.LABEL "ord", (* truncate to 8 bits to prevent negative ints *)
          Mips.ANDI("2", "2", makeConst 255),
          Mips.JR (RA,[]),
          Mips.LABEL "chr", (* int values are truncated to 8 bit (ASCII), *)
          Mips.ANDI ("2", "2", makeConst 255),
          Mips.JR (RA,[]),

          (* len : array of * -> int *)
          (* TODO len depends on chosen representation, which might change *)
          Mips.LABEL "len",
          Mips.LW("2", "2", "0"),
          Mips.JR (RA,[])
         ]

       (* built-in read and write functions *)
       @ [(* int *)
          Mips.LABEL "writeInt",
          Mips.ADDI(SP,SP,"-8"),
          Mips.SW ("2",SP,"0"),    (* save used registers *)
          Mips.SW ("4",SP,"4"),
          Mips.MOVE ("4","2"),     (* convention: number to be written in r2 *)
          Mips.LI ("2","1"),       (* write_int syscall *)
          Mips.SYSCALL,
          Mips.LW ("2",SP,"0"),    (* reload used registers *)
          Mips.LW ("4",SP,"4"),
          Mips.ADDI(SP,SP,"8"),
          Mips.JR (RA,[]),

          Mips.LABEL "readInt",
          Mips.LI ("2","5"),       (* read_int syscall *)
          Mips.SYSCALL,
          Mips.JR (RA,[]) ]

       @ [(* char *)
          Mips.LABEL "writeChar",
          Mips.ADDI(SP,SP,"-8"),
          Mips.SW ("2",SP,"0"),    (* save used registers *)
          Mips.SW ("4",SP,"4"),
          Mips.MOVE ("4","2"),
          Mips.LI("2", "11"),
          Mips.SYSCALL,
          Mips.LW ("2",SP,"0"),    (* reload used registers *)
          Mips.LW ("4",SP,"4"),
          Mips.ADDI(SP,SP,"8"),
          Mips.JR (RA,[]),

          Mips.LABEL "readChar",
          Mips.ADDI(SP,SP,"-8"),
          Mips.SW ("4",SP,"0"),    (* save used registers *)
          Mips.SW ("5",SP,"4"),
          Mips.LI("2", "12"),
          Mips.SYSCALL,
          Mips.MOVE("5","2"),      (* temporarily move the result in reg $5*)
          Mips.LI ("2","4"),       (* writestring syscall *)
          Mips.LA("4","_cr_"),
          Mips.SYSCALL,            (* write CR *)
          Mips.MOVE("2", "5"),     (* put the result back in $2*)
          Mips.LW ("4", SP, "0"),  (* restore registers *)
          Mips.LW ("5", SP, "4"),
          Mips.ADDI(SP,SP,"8"),
          Mips.JR (RA,[]) ]

      @  (* fixed error code for indexing errors *)
         [
            Mips.LABEL "_IllegalArrSizeError_",
            Mips.LA ("4","_IllegalArrSizeString_"),
            Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
            Mips.MOVE ("4","5"),
            Mips.LI ("2","1"), Mips.SYSCALL, (* print line number *)
            Mips.LA ("4","_cr_"),
            Mips.LI ("2","4"), Mips.SYSCALL, (* print CR *)
            Mips.J "_stop_"]

      @  [
            Mips.LABEL "_IllegalArrIndexError_",
            Mips.LA ("4","_IllegalArrIndexString_"),
            Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
            Mips.MOVE ("4","5"),
            Mips.LI ("2","1"), Mips.SYSCALL, (* print line number *)
            Mips.LA ("4","_cr_"),
            Mips.LI ("2","4"), Mips.SYSCALL, (* print CR *)
            Mips.J "_stop_"]


      @  [ (* String constants used at runtime, e.g., error message, CR. *)
            Mips.DATA "",
            Mips.ALIGN "2",
            Mips.LABEL "_cr_",       (* carriage return string *)
            Mips.ASCIIZ "\n",
            Mips.ALIGN "2",
            Mips.LABEL "_IllegalArrSizeString_",
            Mips.ASCIIZ "Error: Array size less or equal to 0 at line ",
            Mips.ALIGN "2",
            Mips.LABEL "_IllegalArrIndexString_",
            Mips.ASCIIZ "Error: Array index out of bounds at line " ]

       @ (* program's string literals *)
         (Mips.COMMENT "String Literals" ::
          List.concat stringdata)

       (* Heap (to allocate arrays in, word-aligned) *)
       @ [Mips.ALIGN "2",
          Mips.LABEL "_heap_",
          Mips.SPACE "100000" ]
    end

end
