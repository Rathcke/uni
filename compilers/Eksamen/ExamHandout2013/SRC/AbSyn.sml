
structure AbSyn =
struct

  type Pos   = int * int  (* position: (line, column) *)
  type Ident = string

  exception Error of string*(Pos)

  datatype BasicVal =
      Num   of int         (* e.g., 123              *)
    | Log   of bool        (* e.g., true or false    *)
    | Chr   of char        (* e.g., 'c'              *)

  datatype Value =
      BVal  of BasicVal
    | Arr   of BasicVal array * int list * int list
                (* e.g., 2-dim array: { {1, 2}, {3,4} }     *)
                (* is represented as: ( flat array, dims, strides ) *)
                (* i.e., ([1, 2, 3, 4], [2,2], [2,1]) *)

  datatype Type =
      Int   of Pos
    | Bool  of Pos
    | Char  of Pos
    | Array of Type * Pos

  and Exp
    = Literal of Value             * Pos
    | StrLit  of string            * Pos      (* e.g., "Hello World!\n" *)
    | ArrLit  of Exp list          * Pos      (* e.g., { {1+x, 3}, {2, {1+4}} *)
    | LValue  of LVAL              * Pos
    | Plus    of Exp * Exp         * Pos      (* e.g., x + 3 *)
    | Minus   of Exp * Exp         * Pos      (* e.g., x - 3 *)
    | Times   of Exp * Exp         * Pos      (* e.g., x * 3 *)
    | Div     of Exp * Exp         * Pos      (* e.g., x / 3 *)
    | Equal   of Exp * Exp         * Pos      (* e.g., x = 3 *)
    | Less    of Exp * Exp         * Pos      (* e.g., a < b *)
    | And     of Exp * Exp         * Pos      (* e.g., (x<1) & y *)
    | Or      of Exp * Exp         * Pos      (* e.g., (x=5) or y *)
    | Not     of Exp               * Pos      (* e.g., not (x>3) *)
    | FunApp  of Ident  * Exp list * Pos      (* e.g., f(1, 3+x) *)
(*  FOR THE EXAM:
    | Pow     of Exp * Exp         * Pos
 *)

  and Dec = Dec of Ident * Type * Pos

  and LVAL = Var    of Ident                        (* x           *)
           | Index  of Ident  * Exp list            (* arr[1,2,3]  *)

  and Stmt = Return   of Exp option                  * Pos (* return a[i];           *)
    |        ProcCall of Ident  * Exp list           * Pos (* my_proc(arr, x, y);    *)
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

  and FunDec = Func of Type * Ident * Dec list * StmtBlock * Pos
    |          Proc of        Ident * Dec list * StmtBlock * Pos

  type Prog = FunDec list

(**************************************************)
(*** Accessors for Function Declaration:        ***)
(***   (i) return type option (procs have none) ***)
(***  (ii) function name,                       ***)
(*** (iii) formal arguments names & types,      ***)
(***  (iv) function's body, (v) position.       ***)
(**************************************************)

    fun getFunDec ( f : FunDec ) : (Type option * Ident * Dec list * StmtBlock * Pos) =
        case f of
          Func(rtp, id, decs, body, pos) => (SOME rtp, id, decs, body, pos)
        | Proc(     id, decs, body, pos) => (NONE,     id, decs, body, pos)

    fun getFunRetp( f : FunDec ) : Type option =
        let val (rtp, _, _, _, _) = getFunDec f in rtp end

    fun getFunName( f : FunDec ) : Ident =
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
            then raise Error("AbSyn Error3 in printArr!", (0,0))
            else " { " ^ printRecArr( d::dims, s::strides, (lb, ub) ) ^ " } "
        end

  fun pp_val (BVal bv)                  = pp_bval bv
    | pp_val (Arr( arr, dims, strides)) = printArr( arr, dims, strides )

  and pp_vals( [] ) = ""
    | pp_vals( (v::vs) : Value list ) : string =
        pp_val v ^ concat (map (fn x => ", " ^ pp_val x) vs)


  (******************************)
  (*** pretty printing a type ***)
  (******************************)
  and pp_type (Int  _)        = "int "
    | pp_type (Bool _)        = "bool "
    | pp_type (Char _)        = "char "
    | pp_type (Array (tp, _)) = "[" ^ pp_type tp ^ "]"

(*******************************************************)
(*** Testing Simple Type Equality (ignores position) ***)
(*******************************************************)

  fun typesEqual (Int  _, Int  _ ) = true
    | typesEqual (Bool _, Bool _ ) = true
    | typesEqual (Char _, Char _ ) = true
    | typesEqual (Array (t1,_), Array (t2,_)) = typesEqual(t1, t2)
    | typesEqual ( _ , _ )         = false

(*******************************************************)
(*** Flatenning/Unflatenning Helpers:                ***)
(*******************************************************)

  fun toIntInds( inds : Value list, pos : Pos ) : int list =
        map (fn x => case x of
                       BVal (Num i) => i
                     | v            =>
                         raise Error( "Interpreter applyIndexing: " ^
                                      "Index not an int: " ^ pp_val v, pos )
            ) inds

  fun flatInd( inds : int list, strides : int list ) : int =
    foldl (op + ) 0 ( ListPair.map (fn (i,s) => i*s) (inds,strides))

  fun liftInd( ind : int, []        : int list ) : int list = []
    | liftInd( ind : int, (s::strides) : int list ) : int list =
    let val i = ind div s
        val r = ind - (i * s)
    in  i :: liftInd( r, strides )
    end


end

