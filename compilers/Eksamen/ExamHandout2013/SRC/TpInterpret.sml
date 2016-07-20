(*************************************************************)
(********************* Interpreter ***************************)
(*************************************************************)

structure TpInterpret :> TpInterpret = struct
    open TpAbSyn

    type Pos = TpAbSyn.Pos
    type v   = TpAbSyn.Value
    type t   = TpAbSyn.Type
    type e   = TpAbSyn.Exp
    type b   = TpAbSyn.StmtBlock
    type d   = TpAbSyn.Dec
    type f   = TpAbSyn.FunDec

    exception Error of string*(Pos)

(****************************)
(***   HELPER FUNCTIONS   ***)
(****************************)

  (* Portable inputLine. *)
  fun inputLine stream =
      case TextIO.input1 stream of
          NONE       => ""
        | SOME #"\n" => "\n"
        | SOME c => str c ^ inputLine stream

(*************************************************)
(*** Function table associates a function name ***)
(***   its declaration, i.e. TpAbSyn.FunDec    ***)
(*************************************************)

fun buildFtab [] =  (** first dummy declarations for built-in functions **)
        let val p     = (0,0)
            val ch    = #"a"
        in [ ( "chr", Func(BType Char, "chr", [Dec( ("n", BType Int ), p)], Block( [], [] ), p) ),
             ( "ord", Func(BType Int , "ord", [Dec( ("c", BType Char), p)], Block( [], [] ), p) ),

             ( "len", Func(BType Int , "len", [Dec( ("n", BType Int ), p), Dec( ("a",Array(1,Int)), p)],
                                                  Block( [], [] ), p) ),
             ( "read" , Func(BType Int,"read", [], Block( [], [] ), p) ),
             ( "new"  , Func(Array(1,Int), "new" ,[], Block( [], [] ), p) ),

             ("write" , Proc("write", [], Block( [], [] ), p) )
           ] @  SymTab.empty()
        end
                    (** regular functions, i.e., defined inside the program **)
  | buildFtab ( fdcl::fs ) =
        let val fid   = getFunName(fdcl)
            val pos   = getFunPos (fdcl)
            val ftab  = buildFtab fs
            val flook = SymTab.lookup fid ftab
        in  case flook of
              NONE   => SymTab.bind fid fdcl ftab
            | SOME f => raise Error ("in buildFtab: already defined function: "^fid, pos)
        end


(*************************************************)
(*** helpers:                                  ***)
(***   1. getBasicVal0   2. isGetString        ***)
(*************************************************)

(* getBasicVal0 ( tp : BasicType ) : BasicVal *)
(* returns the neutral element of a basic type, i.e., 0 *)
fun getBasicVal0 Int  = Num 0
  | getBasicVal0 Bool = Log false
  | getBasicVal0 Char = Chr (chr 0)

fun isGetString( BVal _ ) = ""
  | isGetString( Arr(arr, shape, stride, arr_tp) ) =
        if (length shape) = 1 andalso (hd shape) > 0
        then case Array.sub(arr, 0) of
               (Chr _) => String.implode (
                            Array.foldr (fn (a,lst) =>
                                            case a of
                                              Chr c => c :: lst
                                            | _     => lst
                                  ) [] arr )
             | _       => ""
        else ""

(**************************************************)
(*** Evaluating (i) binary operators +, -, etc. ***)
(***            (ii)equality operator           ***)
(***           (iii)relational operator <,>     ***)
(**************************************************)

(* evalBinop( bop : ((Int,Int)->Int), v1 : Value, v2 : Value, p : Pos ) : Value *)
fun evalBinop ( bop, BVal(Num n1), BVal(Num n2), pos ) = BVal( Num ( bop(n1, n2) ) )
  | evalBinop ( bop, v1, v2, pos ) =
        raise Error("Arguments of binop Are Not Integers! Arg1: " ^
                    pp_val v1 ^ " Arg2: " ^ pp_val v2, pos )

(* evalEq ( v1 : Value, v2 : Value, p : Pos ) : Value *)
fun evalEq ( BVal(Num n1), BVal(Num n2), pos ) = BVal( Log (n1 = n2) )
  | evalEq ( BVal(Log b1), BVal(Log b2), pos ) = BVal( Log (b1 = b2) )
  | evalEq ( BVal(Chr c1), BVal(Chr c2), pos ) = BVal( Log (c1 = c2) )
  | evalEq ( v1, v2, pos ) = raise Error("Argument Types Do Not Match! Arg1: " ^
                                          pp_val v1 ^ " Arg2: " ^ pp_val v2, pos )

(* evalRelop( bop, v1 : Value, v2 : Value, p : Pos ) : Value *)
fun evalRelop ( bop, BVal(Num n1), BVal(Num n2), pos ) = BVal( Log( bop(n1, n2) ) )
  | evalRelop ( bop, BVal(Chr n1), BVal(Chr n2), pos ) = BVal( Log( bop(Char.ord(n1),Char.ord(n2)) ) )
  | evalRelop ( bop, BVal(Log n1), BVal(Log n2), pos ) =
        let val i1 = if n1 then 1 else 0
            val i2 = if n2 then 1 else 0
            val res= bop (i1, i2)
        in  BVal( Log res )
        end
  | evalRelop ( bop, v1, v2, pos ) =
        raise Error( "Argument Types Do Not Match! Arg1: " ^
                      pp_val v1 ^ " Arg2: " ^ pp_val v2, pos )

fun evalAnd (BVal (Log b1), BVal (Log b2), pos) = BVal (Log (b1 andalso b2))
  | evalAnd (v1, v2, pos) =
        raise Error( "And: argument types do not match. Arg1: " ^
                      pp_val v1 ^ ", arg2: " ^ pp_val v2, pos )

fun evalOr (BVal (Log b1), BVal (Log b2), _) = BVal (Log (b1 orelse b2))
  | evalOr (v1, v2, pos) =
    raise Error( "Or: argument types do not match. Arg1: " ^
                 pp_val v1 ^ ", arg2: " ^ pp_val v2, pos )

fun evalNot (BVal (Log b), pos) = BVal (Log (not b))
  | evalNot (v, pos) =
    raise Error( "Not: argument type does not match. Arg1: " ^ pp_val v, pos )


(***********************************************)
(*** Getting/Setting an Array Index,         ***)
(***   with bounds checking                  ***)
(***********************************************)
fun getArrIndex( Arr(arr, shape, strides, tp), inds : Value list, pos ) : Value =
        let val iis = toIntInds( inds, pos )
            val ok1 = (List.length iis) = (List.length shape)
            val ok2 = ListPair.foldl
                          (fn (i, d, b) => b andalso i >= 0 andalso i < d) true (iis, shape)
        in  if ok1 andalso ok2
            then BVal ( Array.sub( arr, flatInd( iis, strides ) ) )
            else raise Error( "in getArrIndex: incomplete "    ^
                              "index or index out of bounds: [ " ^ pp_vals inds ^ " ] ", pos )
        end
  | getArrIndex( arr : Value, inds : Value list, pos : Pos ) : Value =
        raise Error("in getArrIndex: Arg Is Not An Array " ^
                    pp_val arr ^ " or Index Is Not A NUM [ " ^ pp_vals inds ^ " ] ", pos )

fun setArrIndex( Arr(arr, shape, strides, tp), inds : Value list, BVal v, pos ) : unit =
        let val iis = toIntInds( inds, pos )
            val ok1 = (List.length iis) = (List.length shape)
            val ok2 = ListPair.foldl
                          (fn (i, d, b) => b andalso i >= 0 andalso i < d) true (iis, shape)
        in  if ok1 andalso ok2
            then let val flati  = flatInd( iis, strides )
                     val oldval = Array.sub( arr, flati )
                     val ok3    = typesEqual( typeOfVal (BVal oldval), typeOfVal (BVal v) )
                 in  if ok3
                     then Array.update( arr, flati, v )
                     else raise Error("in setArrIndex: types (of the old/updated elem) " ^
                                      pp_bval oldval ^ " does not match: " ^ pp_bval v ^ " at: ", pos)
                 end
            else raise Error( "in setArrIndex: incomplete "    ^
                              "index or index out of bounds: [ " ^ pp_vals inds ^ " ] ", pos )
        end
  | setArrIndex( arr : Value, inds : Value list, _, pos : Pos ) : unit =
        raise Error("in setArrIndex: Arg Is Not An Array " ^
                    pp_val arr ^ " or Index Is Not A NUM [ " ^ pp_vals inds ^ " ] ", pos )


(*******************************************************)
(*** Creates a new value-symbol table (vtable) that  ***)
(***   binds the name of the formal parameter to the ***)
(***   value of the corresponding actual parameter   ***)
(*******************************************************)

(* bindTypeIds : (Dec list, Value list, Ident, Pos, Pos) : (Ident, Value) list *)
fun bindTypeIds ([], [], fid, pd, pc) = SymTab.empty()
  | bindTypeIds ([], _,  fid, pd, pc) =
        raise Error("in bindTypeIds: Number of formal and " ^
                    "actual params differ! In call to " ^ fid,     pc )
  | bindTypeIds (_,  [], fid, pd, pc) =
        raise Error("in bindTypeIds: Number of formal and " ^
                    "actual params differ! In call to " ^ fid,     pc )
  | bindTypeIds ( (Dec((faid,fatp), fap)) :: fargs, a::aargs, fid, pd, pc ) =
        let val vtab   = bindTypeIds( fargs, aargs, fid, pd, pc )
        in  if  typesEqual( fatp, typeOfVal a )
              then case SymTab.lookup faid vtab of
                     NONE   => SymTab.bind faid (ref a) vtab
                   | SOME m => raise Error("Formal Argument Is Already In Symbol Table!"^
                                           " In function: "^fid^" formal argument: "^faid, pd)
              else raise Error( "Actual and Formal Argument Type Do Not Match!"^
                                " In function: "^fid^" formal argument: "^faid^
                                " of type: "^pp_type(fatp)^
                                " does not matches actual argument: " ^ pp_val a,    pc )
        end

(*********************************************)
(*********************************************)
(*** INTERPRETER FOR PRG:                  ***)
(*** 1. builds the functions' symbol table ***)
(*** 2. interprets the body of "main" and  ***)
(*** 3. Returns the interpreted result     ***)
(*********************************************)
(*********************************************)
fun execPgm funlst =
        let val ftab  = buildFtab funlst
            val mainf = SymTab.lookup "main" ftab
        in  case mainf of
              NONE   => raise Error("in evalPgm: Did Not Find Main Fun! ", (0,0))
            | SOME m => callFun(getFunDec m, [], [], [], ftab, (0,0))
	end

(**************************************************)
(******* Executing Function/Procedure Calls *******)
(*** Input:                                     ***)
(***   1. the function declaration as a tiple of***)
(***      the return type (rtp), fun/proc name  ***)
(***      (fid), formal-argument declarations   ***)
(***      (farg), fun/proc body (body), position***)
(***      (pdcl).                               ***)
(***   2. actual-argument values (aargs),       ***)
(***   3. actual-argument expressions(aexps)--to***)
(***        be used for call by value result    ***)
(***   4. the symbol table that binds variable  ***)
(***        identifiers with their values at the***)
(***        point of the call (vtab) -- to be   ***)
(***        used for call by value result       ***)
(***   5. the fun/procedure symbol table (ftab) ***)
(***   6. position of the function call (pcall) ***)
(*** The result is a value option:              ***)
(***    functions return of value, i.e., SOME v,***)
(***    while procedures do not, i.e., NONE     ***)
(**************************************************)

and callFun ((rtp : Type option, fid : string, fargs : Dec list,
              body : StmtBlock, pdcl : Pos ),
             aargs : Value list, aexps : Exp list,
             vtab, ftab, pcall : Pos ) : Value option =
    case fid of
        (* treating "special" functions such as ord/chr/write, etc. *)
        "ord"    => ( case aargs of
                          [BVal(Chr c)] => SOME ( BVal( Num (ord c) ) )
                        | otherwise => raise Error("in call ord: arg Does Not " ^
                                                   "Evaluate to Char: " ^
                                                   pp_vals aargs, pcall) )

      | "chr"    => ( case aargs of
                          [BVal(Num n)] => SOME ( BVal( Chr (chr n) ) )
                        | otherwise => raise Error("in call chr: arg Does Not " ^
                                                   "Evaluate to Int:  " ^
                                                   pp_vals aargs, pcall) )

      | "len"    => ( case aargs of
                          [BVal(Num i), Arr(_,shp,_,_)] =>
                            if i < 0 orelse i >= (length shp)
                            then raise Error("in call to len, illegal dimension: "^
                                             Int.toString i^", at ", pcall)
                            else SOME ( BVal ( Num (List.nth( shp, i ) ) ) )
                        | otherwise => raise Error("in call len: illegal arguments: " ^
                                                   pp_vals aargs, pcall) )

      | "readInt"=> ( case aargs of
                          [] => ( case Int.fromString( inputLine(TextIO.stdIn) ) of
                                      SOME n => SOME (BVal( Num  n ))
                                    | NONE   => raise Error("In readNum Failed! ",
                                                          pcall) )
                        | _  => raise Error("call to readNum: Non-Empty argument" ^
                                            " list! At: ", pcall) )

      | "readBool"=>( case aargs of
                          [] => ( case Int.fromString( inputLine(TextIO.stdIn) ) of
                                      SOME b =>
                                        if( b = 0 )
                                        then SOME (BVal( Log false ))
                                        else SOME (BVal( Log true ))
                                    | NONE   => raise Error("in readLog Failed! ",
                                                            pcall) )
                        | _  => raise Error("In call to readLog: Non-Empty argument" ^
                                            " list! At: ", pcall) )

      | "readChar"=>( case aargs of
                          [] => ( case Char.fromCString( inputLine(TextIO.stdIn) ) of
                                      SOME c => SOME (BVal( Chr  c ))
                                    | NONE   => raise Error("in readChr Failed! ",
                                                            pcall) )
                        | _  => raise Error("In call to readChr: Non-Empty argument" ^
                                            " list! At: ", pcall) )

        | "newIntArr" => ( case aargs of
                               []   => raise Error("In call to newIntArr: no " ^
                                                   "arguments! At: ", pcall)
                             | dims => SOME ( mkNewArr( Int, dims, pcall ) ) )

        | "newBoolArr"=> ( case aargs of
                               []   => raise Error("In call to newBoolArr: no " ^
                                                   "arguments! At: ", pcall)
                             | dims => SOME ( mkNewArr( Bool, dims, pcall ) ) )

        | "newCharArr"=> ( case aargs of
                               []   => raise Error("In call to newCharArr: no " ^
                                                   "arguments! At: ", pcall)
                             | dims => SOME ( mkNewArr( Char, dims, pcall ) ) )

        | "write"  => ( case aargs of
                            [BVal(Num n)] => ( print(Int.toString n); NONE )
                          | [BVal(Log b)] => let val res = if b then "1" else "0"
                                             in (print res; NONE) end
                          | [BVal(Chr c)] => ( print (str c)  ; NONE )
                          | [arr] => let val str_arr = isGetString arr
                                     in  if str_arr = ""
                                         then ( print (pp_val arr); NONE )
                                         else ( print str_arr;      NONE )
                                     end
                          | otherwise => raise Error("in call write: illegal " ^
                                                     " arguments: " ^
                                                     pp_vals aargs, pcall ) )

        | other    =>
          let val new_vtab = bindTypeIds(fargs, aargs, fid, pdcl, pcall)
              val res  = execBlock( body, new_vtab, ftab )
          in case (rtp, res) of
                 (NONE  , _     ) => (map (updateOuterVtable vtab new_vtab)
                                          (ListPair.zip (aexps, fargs)); NONE)
               | (SOME t, SOME r) => if typesEqual(t, typeOfVal r)
                                     then SOME r
                                     else raise Error("in call fun: result does " ^
                                                      "not match the return type! " ^
                                                      "In fun/proc:" ^ fid ^
                                                      " rettype: " ^ pp_type t ^
                                                      " result: "^pp_val r, pcall )
                  | otherwise        => raise Error("in call: fun/proc " ^ fid ^
                                                    " illegal result: ", pcall )
          end

(* Update the outer vtable with data from the inner vtable.  Here, out_exp is an
 * expression in a call, e.g. x in f(x), and in_arg is the local argument name
 * in f, e.g. m when 'procedure f(m) ... end' is defined.  Remember that call by value
 * result requires that argument expressions are variable names, i.e. expressions like
 * '2 + x' do not work, since '2 * x' is not an LValue variable name.
 *)
and updateOuterVtable vtabOuter vtabInner (out_exp, in_arg) =
    case (out_exp, in_arg) of
        (LValue (Var (name, _), _), Dec ((faid, _), _)) =>
        (case (SymTab.lookup name vtabOuter, SymTab.lookup faid vtabInner) of
             (SOME v_out, SOME v_in) => v_out := !v_in
           | _ => ())
      | _ => ()


and mkNewArr( btp : BasicType, shpval : Value list, pos : Pos ) : Value =
        let val shape  = map ( fn d => case d of
                                         BVal(Num n) => n
                                       | _ => raise Error("in new array of " ^ pp_type (BType btp) ^
                                                          " dimension val not an int: "^ pp_val d, pos)
                             ) shpval
            val strides  = mkStrides shape
            val arr_sz = foldl (op * ) 1 shape
            val arr    = Array.tabulate( arr_sz, fn i => getBasicVal0 btp )
        in  Arr( arr, shape, strides, Array(length shpval, btp) )
        end


(****************************************)
(******** Executing a Stmt BLOCK ********)
(****************************************)

(* execBlock ( b : StmtBlock, vtab : (Ident * Value ) list,
                              ftab : (Ident * FunDec) list ) : Value option  *)
and execBlock( Block(decs, stmts) : StmtBlock, vtab, ftab ) : Value option =
        let val new_vtab = vtab @ execDecs(decs)
            val res      = foldl (fn (s,rr : Value option) => case rr of
                                                  NONE   => execStmt( s, new_vtab, ftab )
                                                | SOME r => SOME r
                                 ) NONE stmts
        in  res
        end
(* execDecs ( decs : Dec list ) : (Ident * Value) list  *)
and execDecs ( [] ) = []
  | execDecs ( (Dec((id,tp),pos)::decs) ) : (string * Value ref) list =
        let val vtab = execDecs decs
            val newv = case tp of
                         Array(btp,_)=> Arr( Array.fromList [], [], [], tp)
                       | BType btp   => BVal( getBasicVal0 btp )
        in  SymTab.insert id (ref newv) vtab
        end

(**************************************)
(******** Executing Statements ********)
(**************************************)
(* execStmt ( s : Stmt, vtab : (string * Value ) list,
                        ftab : (string * FunDec) list ) : Value option  *)
and execStmt ( Return   (SOME e,       _), vtab, ftab ) = SOME ( evalExp(e, vtab, ftab) )
  | execStmt ( Return   (NONE  ,       _), vtab, ftab ) = SOME ( BVal (Num 1) )
  | execStmt ( ProcCall ((fid,(_,_)), aargs, pos), vtab, ftab ) =
        let val evargs = map (fn e => evalExp(e, vtab, ftab) ) aargs
            val resopt = if fid = "write"
                         then callFun( (NONE, fid, [], Block([],[]), (0,0)), evargs, aargs, vtab, ftab, pos )
                         else ( case  ( SymTab.lookup fid ftab ) of
                                  SOME f => callFun(getFunDec f, evargs, aargs, vtab, ftab, pos)
                                | NONE   => raise Error("in execStmt: Procedure " ^ fid ^
                                                        " Is Not In Symbol Table! Called At: ", pos) )
        in  case resopt of
              SOME v => raise Error("in execStmt: procedure call " ^
                                     fid ^ " returns a value, Called At: ", pos)
            | NONE   => NONE
        end
  | execStmt ( Assign( Var  (id, _    ), e, pos), vtab, ftab ) =
      ( case SymTab.lookup id vtab of
          SOME vref => let val vnew = evalExp(e, vtab, ftab)
                       in  ( vref := vnew; NONE )
                       end
        | NONE => raise Error( "in Assign-Var Stmt: " ^
                               "Scalar var "^id^" Is Not In Symbol Table!\n", pos ) )
  | execStmt ( Assign( Index((id,_), inds), e, pos), vtab, ftab ) =
      ( case SymTab.lookup id vtab of
          SOME (ref arr ) =>
                let val indvals = map ( fn x => evalExp(x, vtab, ftab) ) inds
                    val ()      = setArrIndex( arr, indvals, evalExp(e, vtab, ftab), pos )
                in  NONE
                end
        | NONE   => raise Error( "in Assign-Array-Index Stmt: " ^
                                 "Array var "^id^" Is Not In Symbol Table!\n", pos ) )
  | execStmt ( IfThEl(cond, then_stmts, else_stmts, pos), vtab, ftab ) =
      ( case evalExp(cond, vtab, ftab) of
          BVal(Log true ) => execBlock(then_stmts, vtab, ftab)
        | BVal(Log false) => execBlock(else_stmts, vtab, ftab)
        | otherwise => raise Error("in exec IF stmt: condition does not evaluate to a bool, at: ", pos) )
  | execStmt ( While(cond, body, pos), vtab, ftab ) =
      ( case evalExp(cond, vtab, ftab) of
          BVal(Log true ) =>
            ( case execBlock(body, vtab, ftab) of
                NONE      => execStmt ( While(cond, body, pos), vtab, ftab )
              | SOME v    => SOME v )
        | BVal(Log false) => NONE
        | otherwise => raise Error("in exec while: condition does not evaluate to a bool, at: ", pos) )

(********************************************************************)
(********************************************************************)
(*** INTERPRETER FOR EXP case analysis after expression's shape:  ***)
(***   1. vtab holds the binding between the variable name        ***)
(***      and its interpreted value. The value can be an integer, ***)
(***      a character, a boolean, or an arbitrary array, i.e.,    ***)
(***      AbSyn values.                                           ***)
(***   2. ftab holds the binding between the function name        ***)
(***      and its declaration, i.e., TpAbsyn.FunDec               ***)
(***   3. the result is the interpreted value of the expression   ***)
(********************************************************************)
(********************************************************************)

(* evalExp ( e : Exp, vtab : (String * Value) list, ftab : (Ident * FunDec) list ) : Value  *)

and evalExp ( Literal(lit,_), vtab, ftab ) = lit
  | evalExp ( StrLit (sss,_), vtab, ftab ) =
        let val strarr = Array.fromList ( map (fn c => Chr c ) (explode sss) )
        in  Arr( strarr, [size sss], [1], Array(1, Char) )
        end
  | evalExp ( ArrLit (lst, arrtp, pos), vtab, ftab ) =
        let val shape   = mkShape ( ArrLit (lst, arrtp, pos) )
            val strides = mkStrides shape
            val explst  = flattenArrRep ( ArrLit (lst, arrtp, pos) )
            val vallst  = map ( fn x => evalExp(x, vtab, ftab) ) explst
            val bvals = map ( fn x => ( case x of
                                          BVal bv => bv
                                        | _       => raise Error("in ArrLit: array element " ^
                                                                 " Is Not a basic value "^pp_val x, pos )
                                      )
                            ) vallst
            (* the check is redundant since it passed the type checker *)
            val el_t  = case arrtp of
                          Array(0,btp) => BType btp
                        | Array(n,btp) => BType btp
                        | _ => raise Error("in evalExp ArrLit, should have failed at type check, "^
                                           " illegal array type "^pp_type arrtp^", at ", pos)
            val elsok = foldl (fn (x,b)=> b andalso typesEqual(x, el_t)) true (map typeOfVal vallst)
        in  if elsok then Arr( Array.fromList bvals, shape, strides, arrtp )
            else raise Error("in evalExp ArrLit, basic element type does not matches, at ", pos)
        end
  | evalExp ( LValue( Var(id,tp), pos ), vtab, ftab ) =
      ( case SymTab.lookup id vtab of
          NONE   => raise Error( "in evalExp of Var: " ^ "Symbol "^id^
                                 " Is Not In Symbol Table!\n",    pos )
        | SOME m => !m  )
  | evalExp ( LValue ( Index((id,tp), inds), pos ), vtab, ftab ) =
        let val indvals = map ( fn x => evalExp(x, vtab, ftab) ) inds
            val arrval  = case (SymTab.lookup id vtab) of
                            SOME v => !v
                          | NONE   => raise Error( "in evalExp of Index: " ^
                                                   "Symbol "^id^" Is Not In Symbol Table!\n", pos )
        in  getArrIndex( arrval , indvals, pos )
        end
  | evalExp ( Plus(e1, e2, pos), vtab, ftab ) =
        let val res1   = evalExp(e1, vtab, ftab)
            val res2   = evalExp(e2, vtab, ftab)
        in  evalBinop(op +, res1, res2, pos)
        end
  | evalExp ( Minus(e1, e2, pos), vtab, ftab ) =
        let val res1   = evalExp(e1, vtab, ftab)
            val res2   = evalExp(e2, vtab, ftab)
        in  evalBinop(op -, res1, res2, pos)
        end
  | evalExp ( Times(e1, e2, pos), vtab, ftab ) =
        let val res1   = evalExp(e1, vtab, ftab)
            val res2   = evalExp(e2, vtab, ftab)
        in  evalBinop(op *, res1, res2, pos)
        end
  | evalExp ( Div(e1, e2, pos), vtab, ftab ) =
        let val res1   = evalExp(e1, vtab, ftab)
            val res2   = evalExp(e2, vtab, ftab)
        in  evalBinop(op div, res1, res2, pos)
        end
  | evalExp ( Equal(e1, e2, pos), vtab, ftab ) =
        let val r1 = evalExp(e1, vtab, ftab)
            val r2 = evalExp(e2, vtab, ftab)
	in evalEq(r1, r2, pos)
	end
  | evalExp ( Less(e1, e2, pos), vtab, ftab ) =
        let val r1 = evalExp(e1, vtab, ftab)
            val r2 = evalExp(e2, vtab, ftab)
	in  evalRelop(op <, r1, r2, pos)   (* > *)
	end
  | evalExp ( And(e1, e2, pos), vtab, ftab ) =
        let val r1 = evalExp(e1, vtab, ftab)
            val r2 = evalExp(e2, vtab, ftab)
	in  evalAnd(r1, r2, pos)
	end

  | evalExp ( Or(e1, e2, pos), vtab, ftab ) =
        let val r1 = evalExp(e1, vtab, ftab)
            val r2 = evalExp(e2, vtab, ftab)
	in  evalOr(r1, r2, pos)
	end

  | evalExp ( Not(e, pos), vtab, ftab ) =
        let val r = evalExp(e, vtab, ftab)
	in  evalNot(r, pos)
	end

  (************************************************************************)
  (** application of regular functions, i.e., defined in the program     **)
  (** special built-in functions "ord" and "chr" are handled in callFun **)
  (************************************************************************)
  | evalExp ( FunApp( (fid, (atps,SOME rtp)), aargs, pos ), vtab, ftab ) =
        let val evargs = map (fn e => evalExp(e, vtab, ftab) ) aargs
            val resopt =
                    if( fid = "ord" orelse fid = "chr" orelse fid = "len" orelse
                        fid = "read" orelse fid = "new" )
                     (***********************************************************)
                     (** dirty trick to handle the built in functions :        **)
                     (** just send the (valid) function id and the evaluated   **)
                     (** actual arguments and 'callFun' will handle the rest  **)
                     (***********************************************************)
                    then let val ffid = case fid of
                                          "ord" => "ord"
                                        | "chr" => "chr"
                                        | "len" => "len"
                                        | "read"=> ( case rtp of
                                                       BType Int  => "readInt"
                                                     | BType Char => "readChar"
                                                     | BType Bool => "readBool"
                                                     | _ => raise Error("in evalExp of read call, type checker "^
                                                                        "gives incorrect non-basic type "^pp_type rtp^", at ", pos) )
                                        | "new" => let val dimvs = map (fn x => (case x of BVal(Num d) => if d>0 then d else 0 | _ => 0)) evargs
                                                       val prod  = foldr (op * ) 1 dimvs
                                                   in  if prod <= 0
                                                       then raise Error("in evalExp of new call, zero or negative "^
                                                                        "dimension value: [ "^pp_vals evargs^" ], at ", pos)
                                                       else
                                                        ( case (basicType rtp) of
                                                            BType Int  => "newIntArr"
                                                          | BType Char => "newCharArr"
                                                          | BType Bool => "newBoolArr"
                                                          | _ => raise Error("in evalExp of new call, impossible case, at ", pos) )
                                                   end
                                        | _ => raise Error("in evalExp of special function call, reached unreachable case ", pos)

                         in  callFun( (SOME rtp, ffid, [], Block([],[]), (0,0)), evargs, aargs, vtab, ftab, pos)
                         end
                     (***********************************************************)
                     (** we take the regular-function declaration from ftable  **)
                     (***********************************************************)
                    else case  ( SymTab.lookup fid ftab ) of
                           SOME f => callFun(getFunDec f, evargs, aargs, vtab, ftab, pos)
                         | NONE   => raise Error("in evalExp of FunApp: Function " ^
                                                 fid ^ " Is Not In Symbol Table! Called At: ", pos)
        in  case resopt of
              SOME v => v
            | NONE   => raise Error("in evalExp of FunApp: Function " ^
                                     fid ^ " Result value is NONE, Called At: ", pos)
        end
  | evalExp ( FunApp( (fid, (atps, NONE)), aargs, pos ), vtab, ftab ) =
        raise Error("in evalExp of function call, illegal NONE result type (bug in tye checker) ", pos)

  | evalExp ( Map ((fid,signat), arrexp, pos), vtab, ftab ) =
        raise Error("In TpInterpret, map is Unimplemented!", pos)

  | evalExp ( Red ((fid,signat), arrexp, pos), vtab, ftab ) =
        raise Error("In TpInterpret, reduce is Unimplemented!", pos)

  | evalExp ( ZipWith ((fid,signat), arrexp1, arrexp2, pos), vtab, ftab ) =
        raise Error("In TpInterpret, zipWith is Unimplemented!", pos)

(*  | evalExp _  = raise Error("Unimplemented!", (0,0)) *)

end
