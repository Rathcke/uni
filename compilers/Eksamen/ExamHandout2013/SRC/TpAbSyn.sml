
structure TpAbSyn =
struct

  type Pos   = int * int  (* position: (line,column) *)

  exception Error of string*(Pos)

  datatype BasicVal =
      Num   of int         (* e.g., 123              *)
    | Log   of bool        (* e.g., true or false    *)
    | Chr   of char        (* e.g., 'c'              *)

  datatype Value =
      BVal  of BasicVal
    | Arr   of BasicVal array * int list * int list * Type
                (* e.g., 2-dim array: { {1, 2}, {3,4} }     *)
                (* is represented as: ( flat array, dims, strides ) *)
                (* i.e., ([1, 2, 3, 4], [2,2], [2,1]) *)
                (* the last argument is the array's type*)

  and BasicType =
      Int
    | Bool
    | Char

  and Type =
      BType of BasicType
    | Array of int * BasicType

  type  Ident    = string  * Type
  type Signature = Type list * Type option
  type FIdent    = string * Signature

  datatype Exp
    = Literal of Value             * Pos
    | StrLit  of string            * Pos      (* e.g., "Hello World!\n" *)
    | ArrLit  of Exp list * Type   * Pos      (* e.g., { {1+x, 3}, {2, {1+4}} *)
                                              (* Type is the array's type *)
    | LValue  of LVAL              * Pos
    | Plus    of Exp * Exp         * Pos      (* e.g., x + 3 *)
    | Minus   of Exp * Exp         * Pos      (* e.g., x - 3 *)
    | Times   of Exp * Exp         * Pos      (* e.g., x * 3 *)
    | Div     of Exp * Exp         * Pos      (* e.g., x / 3 *)
    | Equal   of Exp * Exp         * Pos      (* e.g., x = 3 *)
    | Less    of Exp * Exp         * Pos      (* e.g., a < b *)
    | And     of Exp * Exp         * Pos      (* e.g., (x<1) and y *)
    | Or      of Exp * Exp         * Pos      (* e.g., (x=5) or y *)
    | Not     of Exp               * Pos      (* e.g., not (x>3) *)
    | FunApp  of FIdent * Exp list * Pos      (* e.g., f(1, 3+x) *)
    | Map     of FIdent * Exp      * Pos      (* map(f,    {a1, ..., an}) == { f(a1), ..., f(an) }    *)
    | Red     of FIdent * Exp      * Pos      (* red(f,    {a1, ..., an}) == f (... f( f(a1, a2), a3 ) ..., an) *) 
    | ZipWith of FIdent * Exp * Exp* Pos      (* zipWith(f,{a1,...,an},{b1,...,bn}) == { f(a1,b1), ..., f(an,bn) } *)   
(*  FOR THE EXAM:
    | Pow     of Exp * Exp         * Pos
 *)

  and Dec = Dec of Ident * Pos

  and LVAL = Var    of Ident                  (* e.g., x     *)
           | Index  of Ident  * Exp list      (* arr[1,2,3]  *)

  and Stmt = Return   of Exp option                  * Pos (* return a[i];           *)
    |        ProcCall of FIdent * Exp list           * Pos (* my_proc(arr, x, y);    *)
    |        Assign   of LVAL   * Exp                * Pos (* Assignment: a[i]:=x+y; *)
    |        IfThEl   of Exp * StmtBlock * StmtBlock * Pos (* if x<y then d:=y-x; else d:=x-y; *)
    |        While    of Exp * StmtBlock             * Pos (* while a<b do begin x[i]:= a;a:=a+1 end *)
(*  FOR THE EXAM:
    |        RepeatUntil of Exp * StmtBlock  * Pos
    |        Break of Pos
    |        Continue of Pos
    |        ForLoop   of Ident * Exp * bool * Exp * StmtBlock * Pos
    |        GuardedDo of (Exp * StmtBlock) list * Pos
    |        Switch    of Exp * Case list * Pos

  and Case = Case of BasicVal * StmtBlock * Pos
           | DefaultCase of StmtBlock * Pos
*)

  and StmtBlock = Block of Dec list * Stmt list

  and FunDec = Func of Type * string * Dec list * StmtBlock * Pos   (* return type * fun name * var decls * body * pos *)
    |          Proc of        string * Dec list * StmtBlock * Pos   (*               fun name * var decls * body * pos *)

  type Prog = FunDec list

(**************************************************)
(*** Accessors for Function Declaration:        ***)
(***   (i) return type option (procs have none) ***)
(***  (ii) function name,                       ***)
(*** (iii) formal arguments names & types,      ***)
(***  (iv) function's body, (v) position.       ***)
(**************************************************)

  fun getFunDec ( f : FunDec ) : (Type option * string * Dec list * StmtBlock * Pos) =
      case f of
        Func(rtp, id, decs, body, pos) => (SOME rtp, id, decs, body, pos)
      | Proc(     id, decs, body, pos) => (NONE,     id, decs, body, pos)

  fun getFunRetp( f : FunDec ) : Type option =
      let val (rtp, _, _, _, _) = getFunDec f in rtp end
  fun getFunName( f : FunDec ) : string =
      let val (_, fid, _, _, _) = getFunDec f in fid end
  fun getFunArgs( f : FunDec ) : Dec list    =
      let val (_, _, arg, _, _) = getFunDec f in arg end
  fun getFunBody( f : FunDec ) : StmtBlock =
      let val (_, _, _, bdy, _) = getFunDec f in bdy end
  fun getFunPos ( f : FunDec ) : Pos =
      let val (_, _, _, _, pos) = getFunDec f in pos end


(****************************************************)
(********** Pretty-Printing Functionality ***********)
(****************************************************)

  fun makeDepth 0 = ""
    | makeDepth n = "    " ^ (makeDepth (n-1))

  (*************************************)
  (*** pretty printing a basic value ***)
  (*************************************)

  fun pp_bval (Num n)           = Int.toString  n
    | pp_bval (Log b)           = Bool.toString b
    | pp_bval (Chr c)           = "'" ^ Char.toCString c ^ "'"

  (***********************************************)
  (*** pretty printing array values & helper   ***)
  (***********************************************)
  fun printArr ( _, [], _  ) : string = ""
    | printArr ( _, _ , [] ) : string = ""
    | printArr ( arr : BasicVal array, (d::dims) : int list, (s::strides) : int list ) : string =
        let fun printRecArr( [d],     [one] ,   (lb,ub) : int*int ) =
                  if lb < 0 orelse ub < 0 orelse lb >= ub orelse one <> 1 orelse d <> (ub - lb)
                  then raise Error("In printRecArr: Illegal lower/upper bounds!", (0,0))
                  else if d = 1
                       then pp_bval(Array.sub(arr, lb))
                       else let val s1  = pp_bval(Array.sub(arr, lb))
                                val sz  = ub - lb - 1
                                val rng = List.tabulate( sz, fn i => i + lb + 1)
                                val lst = map ( fn i => ", " ^ pp_bval(Array.sub(arr, i)) ) rng
                            in  s1 ^ concat lst
                            end
              | printRecArr( d::dms, s::spns, (lb, ub) : int*int ) =
                  let val iotad= List.tabulate(d, fn x => x)
                      val lubs = map ( fn i => let val is = lb+i*s in (is, is+s) end ) iotad
                      val res  = map ( fn x => " { " ^ printRecArr(dms, spns, x) ^ " } " ) lubs
                  in  if length res <= 1
                      then concat res
                      else (hd res) ^ concat ( map (fn x => ", " ^ x) (tl res) )
                  end
              | printRecArr( _ , _, _ ) = raise Error("In printRecArr: shape and stride Do Not Match!", (0,0))


            val lb = 0
            val ub = d * s
        in  if lb < 0 orelse ub < 0 orelse lb >= ub
            then raise Error(" In printArr: illegal lower/upper bounds ", (0,0))
            else " { " ^ printRecArr( d::dims, s::strides, (lb, ub) ) ^ " } "
        end

  fun pp_val (BVal bv)                     = pp_bval bv
    | pp_val (Arr( arr, dims, strides, _)) = printArr( arr, dims, strides )

  and pp_vals( [] ) = ""
    | pp_vals( (v::vs) : Value list ) : string =
        pp_val v ^ concat (map (fn x => ", " ^ pp_val x) vs)

  fun pp_exps [] = ""
    | pp_exps ( (e::es) : Exp list ) : string = pp_exp e ^ concat (map (fn x => ", " ^ pp_exp x) es)

  (***********************************)
  (*** pretty printing an l-value ****)
  (***********************************)
  and pp_Lval (Var    (nm,_)        )   = nm
    | pp_Lval (Index ((nm,_), inds) )   = nm ^ "[ " ^ pp_exps inds ^ " ] "

  (*************************************)
  (*** pretty printing an expression ***)
  (*************************************)
  and pp_exp (Literal (lit,    _))  = pp_val lit
    | pp_exp (StrLit  (str,    _))  = "\"" ^ String.toCString str ^ "\""
    | pp_exp (ArrLit  (els, _, _))  = " { " ^ pp_exps els ^ " } "
    | pp_exp (LValue  (lv,     _))  = pp_Lval lv

    | pp_exp (Plus  (e1, e2, _))    = "( " ^ pp_exp e1 ^ " + " ^ pp_exp e2 ^ " )"
    | pp_exp (Minus (e1, e2, _))    = "( " ^ pp_exp e1 ^ " - " ^ pp_exp e2 ^ " )"
    | pp_exp (Times (e1, e2, _))    = "( " ^ pp_exp e1 ^ " * " ^ pp_exp e2 ^ " )"
    | pp_exp (Div   (e1, e2, _))    = "( " ^ pp_exp e1 ^ " / " ^ pp_exp e2 ^ " )"
    | pp_exp (Equal (e1, e2, _))    = "( " ^ pp_exp e1 ^ " = " ^ pp_exp e2 ^ " )"
    | pp_exp (Less  (e1, e2, _))    = "( " ^ pp_exp e1 ^ " < " ^ pp_exp e2 ^ " )"
    | pp_exp (And   (e1, e2, _))    = "( " ^ pp_exp e1 ^ " and " ^ pp_exp e2 ^ " )"
    | pp_exp (Or    (e1, e2, _))    = "( " ^ pp_exp e1 ^ " or " ^ pp_exp e2 ^ " )"
    | pp_exp (Not   (e,      _))    = "( not " ^ pp_exp e ^ " )"

    | pp_exp (FunApp ((nm,_), args, _)) = nm ^ "( " ^ pp_exps args ^ " )"
    | pp_exp (Map    ((nm,_), arr , _)) = "map ( " ^ nm ^ ", " ^ pp_exp arr ^ " ) "
    | pp_exp (Red    ((nm,_), arr , _)) = "reduce ( " ^ nm ^ ", " ^ pp_exp arr ^ " ) "
    | pp_exp (ZipWith((nm,_), arr1 , arr2, _)) = "zipWith ( " ^ nm ^ ", " ^ pp_exp arr1 ^ pp_exp arr2 ^ " ) "
(*
    | pp_exp (Pow    (e1, e2, p) = " ( " ^ pp_exp e1 ^ " ) " ^ " ^ " ^ " ( " ^ pp_exp e2 ^ " ) " 
*)

  (******************************)
  (*** pretty printing a type ***)
  (******************************)
  and pp_btype Int  = "int "
    | pp_btype Bool = "bool "
    | pp_btype Char = "char "

  and pp_type  (BType   btp ) = pp_btype btp
    | pp_type  (Array(0,btp)) = pp_btype btp
    | pp_type  (Array(r,btp)) =
        if r > 0 then "array of " ^ pp_type (Array((r-1), btp))
        else pp_type (BType btp)

  and pp_types [] = ""
    | pp_types ( (t::ts) : Type list ) : string = pp_type t ^ concat (map (fn x => ", " ^ pp_type x) ts)

  (***********************************)
  (*** pretty printing a statement ***)
  (***********************************)
  and pp_stmt d (Return  (SOME e,       _)) = "\n" ^ makeDepth d ^ "return " ^ pp_exp e
    | pp_stmt d (Return  (NONE,         _)) = "\n" ^ makeDepth d ^ "return "
    | pp_stmt d (Assign  (lhs,    rhs,  _)) = "\n" ^ makeDepth d ^ pp_Lval lhs ^ " := " ^ pp_exp rhs
    | pp_stmt d (ProcCall((nm,_), args, _)) = "\n" ^ makeDepth d ^ nm ^"( " ^ pp_exps args ^ " )"

    | pp_stmt d (IfThEl (cond, stblck1, stblck2, _)) =
        let val prologue = "\n" ^ makeDepth d ^ "if ( " ^ pp_exp cond ^ " ) "
            val then_exp = pp_block (d+1) stblck1
            val else_exp = pp_block (d+1) stblck2
        in  if ( (size else_exp) = 0 )
            then prologue ^ " then " ^ then_exp
            else prologue ^ " then " ^ then_exp ^ " else " ^ else_exp
        end

    | pp_stmt d (While (cond, stblck, _)) =
        let val prologue = "\n" ^ makeDepth d ^ "while ( " ^ pp_exp cond ^ " ) do "
            val body     = pp_block (d+1) stblck
        in  prologue ^ body
        end
(*
    | pp_stmt d (RepeatUntil (cond, blk, _)) =
      "\n" ^ makeDepth d ^ "repeat " ^ pp_block (d+1) blk ^
      "\n" ^ makeDepth d ^ "until " ^ pp_exp cond
    | pp_stmt d (Break p)    =  "\n" ^ makeDepth d ^ "break"
    | pp_stmt d (Continue p) =  "\n" ^ makeDepth d ^ "continue"
    | pp_stmt d (ForLoop ((i,_),e1,dir,e2,b,_)) = 
      "\n" ^ makeDepth d ^ "for " ^ i ^ " = " ^ pp_exp e1 ^ 
      (if dir then " to " else " downto ") ^ pp_exp e2 ^
      " do" ^ pp_block (d+2) b
    | pp_stmt d (GuardedDo ([],p)) = raise Error ("empty guard list",p)
    | pp_stmt d (GuardedDo ((g1,blk1)::gs,_)) =
        "\n " ^ makeDepth d ^ "do " ^ pp_exp g1 ^ ": " ^ pp_block (d+2) blk1 ^
        concat (map (fn (g,b) => "\n " ^ makeDepth d ^ "[] " ^ 
                                 pp_exp g ^ ": " ^ pp_block (d+2) b) gs) ^
        "\n" ^ makeDepth d ^ "done;"
    | pp_stmt d (Switch (exp, cases, _)) =
        let val prologue = "\n" ^ makeDepth d ^ "switch " ^ pp_exp exp ^ ":\n"
            val body     = String.concatWith "\n" (map (pp_case d) cases)
        in  prologue ^ body
        end

  and pp_case d (Case (v, sblk, _)) = makeDepth d ^ "case " ^ pp_bval v ^ ":"
                                      ^ pp_block (d + 2) sblk
    | pp_case d (DefaultCase (sblk, _)) = makeDepth d ^ "default:"
                                          ^ pp_block (d + 2) sblk
*)
  and pp_stmts d ss = concat ( map ( fn x => pp_stmt d x ^ ";") ss )

  and pp_block d (Block (dcls, stmts)) =
        if null stmts
        then ""
        else "\n" ^ makeDepth (d-1) ^
             (  if null dcls
                then ""
                else "var "^concat (map (pp_dec  d) dcls )^
                     "\n" ^ makeDepth (d-1)
             )
             ^ "begin " ^ pp_stmts d stmts ^
               "\n" ^ makeDepth (d-1) ^ "end "

  and pp_dec   d (Dec ((id, tp), _))=
        "\n" ^ makeDepth d ^ id ^ " : " ^ pp_type tp ^ ";"

  (********************************************)
  (*** pretty printing a procedure/function ***)
  (********************************************)
  fun pp_fun ( f : FunDec ) =
    let val (mrtp, id, args, body, _) = getFunDec f
        fun pp_bd  (Dec ((nm,tp),_) ) = nm ^ " : " ^ pp_type tp
        fun pp_cbd (bd : Dec )        = ", " ^ pp_bd bd
        fun pp_bindings []            = ""
          | pp_bindings [bd]          = pp_bd bd
          | pp_bindings (bd::l)       = pp_bd bd ^ concat (map pp_cbd l)

        val midstr = id ^ "( " ^ pp_bindings args ^ " ) "
        val fdcstr = case mrtp of
                         SOME rtp => "function  " ^ midstr ^ " : " ^ pp_type rtp ^ " "
                       | NONE     => "procedure " ^ midstr ^ " "
    in  fdcstr ^ pp_block 1 body ^ ";\n\n"
    end

  (*********************************)
  (*** pretty printing a PROGRAM ***)
  (*********************************)
  fun prettyPrint (prog : Prog) = "program SomeName;\n\n" ^ concat (map pp_fun prog) ^ "\n"


(**************************************************************)
(*** Type Equality / Extracting the Basic Type / Converting ***)
(**************************************************************)

  fun typesEqual (BType Int   , BType Int   ) = true
    | typesEqual (BType Bool  , BType Bool  ) = true
    | typesEqual (BType Char  , BType Char  ) = true
    | typesEqual (Array(r1,t1), Array(r2,t2)) =
        ( r1 = r2 andalso typesEqual(BType t1, BType t2) )
    | typesEqual (_           , _           ) = false

  (* basicType ( t : Type ) : Type *)
  fun basicType  (BType   t ) = BType t
    | basicType  (Array(r,t)) = BType t

  fun isBasicType (BType _) = true
    | isBasicType _         = false

  fun toTpAbSynType (AbSyn.Int      _ ) = BType Int
    | toTpAbSynType (AbSyn.Char     _ ) = BType Char
    | toTpAbSynType (AbSyn.Bool     _ ) = BType Bool
    | toTpAbSynType (AbSyn.Array(tp,_)) =
        let val tpm1 = toTpAbSynType tp
        in  case tpm1 of
              BType   btp => Array(1,    btp)
            | Array(r,btp)=> Array((r+1),btp)
        end

  fun toTpAbSynOptType(t : AbSyn.Type option) : Type option =
        case t of
          NONE    => NONE
        | SOME tp => SOME (toTpAbSynType tp)


(***********************************************)
(*** Helper Functions typeOfFun/Val/Exp/Stmt ***)
(***********************************************)

  (*  isPolyFun ( fid : string ) : Bool *)
  fun isPolyFun "write" = true
    | isPolyFun "read"  = true
    | isPolyFun "len"   = true
    | isPolyFun "new"   = true
    | isPolyFun "map"   = true
    | isPolyFun "reduce"= true
    | isPolyFun "zipWith"=true
    | isPolyFun "shape" = true
    | isPolyFun "view"  = true
    | isPolyFun _       = false

  (*  typeOfFun ( f : FunDec ) : (Type list    * Type option) *)
  (*                              type of args * return type  *)
  fun typeOfFun fd =
        if isPolyFun (getFunName fd)
        then raise Error("in typeOfFun: function/procedure "^
                         getFunName fd^" is polymorphic; check "^
                         "with TpAbSyn.isPolyFun before calling typeOfFun ", (0,0) )
        else case (getFunName fd) of
               "chr" => ([BType Int] , SOME (BType Char))
             | "ord" => ([BType Char], SOME (BType Int ))
             | otherwise =>
                 let val args    = getFunArgs fd
                     val argtps  = map ( fn (Dec ((_,t), _)) => t ) args
                 in  (argtps, getFunRetp fd)
                 end

  (*  typeOfVal ( v : Value ) : Type *)
  fun typeOfVal ( BVal(Num   _) ) = BType Int
    | typeOfVal ( BVal(Log   _) ) = BType Bool
    | typeOfVal ( BVal(Chr   _) ) = BType Char
    | typeOfVal ( Arr (_,_,_,t) ) = t

  fun toTpBValue (AbSyn.Num n) = Num n
    | toTpBValue (AbSyn.Chr c) = Chr c
    | toTpBValue (AbSyn.Log b) = Log b

  (*  typeOfExp ( e : Exp   ) : Type *)
  fun typeOfExp ( Literal(v,  _) ) = typeOfVal v
    | typeOfExp ( StrLit (_,  _) ) = Array(1,Char)
    | typeOfExp ( ArrLit (_,t,_) ) = t
    | typeOfExp ( Plus   (a,b,_) ) = typeOfExp a
    | typeOfExp ( Minus  (a,b,_) ) = typeOfExp a
    | typeOfExp ( Times  (a,b,_) ) = typeOfExp a
    | typeOfExp ( Div    (a,b,_) ) = typeOfExp a
    | typeOfExp ( Equal  (_,_,_) ) = BType Bool
    | typeOfExp ( Less   (_,_,_) ) = BType Bool
    | typeOfExp ( And    (a,_,_) ) = typeOfExp a (* BType Bool *)
    | typeOfExp ( Or     (a,_,_) ) = typeOfExp a (* BType Bool *)
    | typeOfExp ( Not    (_,  _) ) = BType Bool
(*  | typeofExp ( Pow    (a,b,_) ) = BType Int   *)

    | typeOfExp ( LValue (Var    (_,t)      , _) ) = t
    | typeOfExp ( LValue (Index ((v,t),inds), p) ) =
        ( case t of
            Array(r,btp) =>
                if (r <> 0) andalso (r - (length inds) = 0)
                then BType btp
                else raise Error("In typeOfExp of Index: illegal var "^v^" of type: "^
                                 pp_type t^" indexed with "^pp_exps inds^", at ", p)
          | tp   =>  raise Error("In typeOfExp of Index: illegal var "^v^
                                 " of non-array type: "^pp_type tp^", at ", p)
        )
    | typeOfExp ( FunApp ((_,(_,SOME t))  , _, p) ) = t
    | typeOfExp ( FunApp ((fid,(_,NONE  ))  , _, p) ) =
        raise Error("In typeOfExp function call "^fid^" computed result type is NONE, at", p)

    | typeOfExp ( Map    ( (_,(_, SOME (BType btp) ) ), a, p) ) =
        (* assumes type checking has checked that `btp' is a basic type *)
        ( case (typeOfExp a) of
            Array(r,atp) => Array(r,btp)
          | _            => raise Error("In typeOfExp of Map, array argument "^pp_exp a^
                                        " has non-array type: "^pp_type (typeOfExp a)^", at ", p)    )
    | typeOfExp ( Map    ((fid,(_,SOME tp)),a,p) ) =
        raise Error("In typeOfExp of Map, function "^fid^
                    " returns non-basic type "^pp_type tp^", at ", p)
    | typeOfExp ( Map    ((fid,(_,NONE)),a,p) ) =
        raise Error("In typeOfExp of Map, function "^fid^
                    " lacks return type, at ", p)

    | typeOfExp ( Red    ( _, a, p) ) =
        (* assumes it was type checked *)
        ( case (typeOfExp a) of
            Array(r,atp) => BType atp
          | _            => raise Error("In typeOfExp of Red, array argument "^pp_exp a^
                                        " has non-array type: "^pp_type (typeOfExp a)^", at ", p)    )

    | typeOfExp ( ZipWith    ( (_,(_, SOME (BType btp) ) ), a1, a2, p) ) =
        (* assumes type checking has checked that `btp' is a basic type *)
        ( case (typeOfExp a1, typeOfExp a2) of
            ( Array(r1,a1tp), Array(r2, a2tp) ) => 
                if r1 = r2 then Array(r1,btp)
                else raise Error("In typeOfExp of ZipWith, array arguments have different ranks "^
                                    Int.toString r1^" <> "^Int.toString r2, p)
          | _  => raise Error("In typeOfExp of ZipWith, one of the array arguments "^
                              pp_exp a1^", "^pp_exp a2^" has non-array type: "^
                              pp_type (typeOfExp a1)^", "^pp_type (typeOfExp a2)^", at ", p)    )
    | typeOfExp ( ZipWith    ((fid,(_,SOME tp)),a1,a2,p) ) =
        raise Error("In typeOfExp of ZipWith, function "^fid^
                    " returns non-basic type "^pp_type tp^", at ", p)
    | typeOfExp ( ZipWith    ((fid,(_,NONE)),a1,a2,p) ) =
        raise Error("In typeOfExp of ZipWith, function "^fid^
                    " lacks return type, at ", p)


  (*  typeOfStmt( e : Stmt   ) : Type option *)
  fun typeOfStmt( Return (SOME e,_) ) = SOME (typeOfExp e)
    | typeOfStmt( Return (NONE  ,_) ) = NONE
    | typeOfStmt(ProcCall(_, _,  p) ) = NONE
    | typeOfStmt( Assign (_, _,  p) ) = NONE
    | typeOfStmt( While  (_, _,  p) ) = NONE
    | typeOfStmt(IfThEl(_, _, _, p) ) = NONE
(*
    | typeOfStmt(RepeatUntil(_,_,_) ) = NONE
    | typeOfStmt(Break _) )           = NONE
    | typeOfStmt(Continue _) )        = NONE
    | typeOfStmt(ForLoop(_,_,_,_,_,_))= NONE
    | typeOfStmt(GuardedDo(_,_) )     = NONE
    | typeOfStmt(Switch(_,_,_) )      = NONE
*)

(***************************************************)
(*** Helper Function posOfExp/Stmt/Block/Fun     ***)
(***************************************************)

  (*  posOfExp ( e : Exp ) : Pos *)
  fun posOfExp  ( Literal(_,  p) ) = p
    | posOfExp  ( StrLit (_,  p) ) = p
    | posOfExp  ( ArrLit (_,_,p) ) = p
    | posOfExp  ( LValue (_,  p) ) = p
    | posOfExp  ( Plus   (_,_,p) ) = p
    | posOfExp  ( Minus  (_,_,p) ) = p
    | posOfExp  ( Times  (_,_,p) ) = p
    | posOfExp  ( Div    (_,_,p) ) = p
    | posOfExp  ( Equal  (_,_,p) ) = p
    | posOfExp  ( Less   (_,_,p) ) = p
    | posOfExp  ( And    (_,_,p) ) = p
    | posOfExp  ( Or     (_,_,p) ) = p
    | posOfExp  ( Not    (_,  p) ) = p
    | posOfExp  ( FunApp (_,_,p) ) = p
    | posOfExp  ( Map    (_,_,p) ) = p
    | posOfExp  ( Red    (_,_,p) ) = p
    | posOfExp  ( ZipWith(_,_,_,p))= p
(*  | posOfExp  ( Pow    (_,_,p) ) = p  *)


  (*  posOfStmt ( s : Stmt ) : Pos *)
  fun posOfStmt ( Return (_,  p) ) = p
    | posOfStmt (ProcCall(_,_,p) ) = p
    | posOfStmt ( Assign (_,_,p) ) = p
    | posOfStmt ( While  (_,_,p) ) = p
    | posOfStmt (IfThEl(_,_,_,p) ) = p
(*
    | posOfStmt(RepeatUntil(_,_,p) ) = p
    | posOfStmt(Break p) )           = p
    | posOfStmt(Continue p) )        = p
    | posOfStmt(ForLoop(_,_,_,_,_,p))= p
    | posOfStmt(GuardedDo(_,p) )     = p
    | posOfStmt(Switch(_,_,p) )      = p
*)

  (*  posOfExp ( d : Dec ) : Pos *)
  fun posOfDec  ( Dec    (_,  p) ) = p

  (*  posOfExp ( b : StmtBlock ) : Pos *)
  fun posOfBlock( Block ([], []   )) = (0,0)
    | posOfBlock( Block ([], s::ss))= posOfStmt s
    | posOfBlock( Block (d::ds,  _))= posOfDec  d

  (*  posOfExp ( f : FunDec ) : Pos *)
  fun posOfFun  ( Func(_,_,_,_,p) ) = p
    | posOfFun  ( Proc(  _,_,_,p) ) = p



(*******************************************************)
(*** Flattenning/Unflattenning Helpers:              ***)
(*******************************************************)

  fun toIntInds( inds : Value list, pos : Pos ) : int list =
        map (fn x => case x of
                       BVal (Num i) => i
                     | v            =>
                         raise Error( "Interpreter applyIndexing: " ^
                                      "Index not an int: " ^ pp_val v, pos )
            ) inds

  fun flatInd( inds : int list, strides : int list ) : int =
    foldl (op + ) 0 ( ListPair.map (fn (i,s) => i*s) (inds, strides) )

  fun liftInd( ind : int, []        : int list ) : int list = []
    | liftInd( ind : int, (s::strides) : int list ) : int list =
    let val i = ind div s
        val r = ind - (i * s)
    in  i :: liftInd( r, strides )
    end

(*************************************************)
(*** helpers to translate ArrLit to Arr (Value)***)
(***   1. mkShape    &      mkStrides          ***)
(***   2. flattenArrRep, 3. getBasicVal0       ***)
(*************************************************)

fun mkShape ( ArrLit( [], _, _  ) ) : int list = [0]
  | mkShape ( ArrLit( exp_lst, _, pos ) ) : int list =
        let val d    = length exp_lst
            val dss  = map mkShape exp_lst
            val dims = hd dss
            val allok= foldl (fn (dd, b) => b andalso dd = dims) true dss
        in  if allok
            then (d :: dims)
            else raise Error ("in mkShape: found irregular array", pos)
        end
  | mkShape _ = [] (* raise Error ("in mkShape: requires ArrLit", pos) *)

fun mkStrides [] = []
  | mkStrides (dims : int list) : int list =
        let fun scanl_ f e    []     = [e]
              | scanl_ f e (x::rest) = e :: (scanl_ f (f (e,x)) rest)

        in  rev ( scanl_ (op * ) 1 (rev (tl dims)) )
        end

fun flattenArrRep ( ArrLit( exp_lst, _, pos ) ) : Exp list =
        List.concat (map flattenArrRep exp_lst)
  | flattenArrRep e = [e]

end

