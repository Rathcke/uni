structure Type :> Type =
struct
  open TpAbSyn

  (* Position (line,column) *)
  type Pos = int*int

  (* TpAbSyn.Signature = is the signature of a function/procedure,
                        (TpAbSyn.Type list * TpAbSyn.Type option),
                        i.e., (tp1 * ... * tpn) -> rtp
  *)
  type FTab = (string * TpAbSyn.Signature) list
  type VTab = (string * TpAbSyn.Type) list

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*Pos

  (* table of predefined conversion functions *)
  val functionTable : FTab ref
    = let val noPos = (0,0)
      in ref [ ("ord",  ([BType   Char], SOME (BType Int ))) (* ord: char->int  *)
             , ("chr",  ([BType   Int ], SOME (BType Char))) (* chr: int ->char *)

             (* "len", "read", "write" and "new" are polymorphic, i.e.,
              * not expressible directly in Paladim! *)
             , ("len",  ([Array(3,Int)], SOME (BType Int )))
             , ("read", ([BType Int, BType Char], SOME (BType Int )))
             , ("write",([BType Int, BType Char], NONE             ))
             , ("new",  ([BType Int, BType Char], SOME (BType Int )))
             ]
      end

  fun checkDuplicateDecs decs =
      let val pos_table = rev (map (fn (Dec((id,_),pos)) => (id, pos)) decs)
          val (ids,_) = ListPair.unzip pos_table
      in (List.foldr (fn (Dec((id,_),pos),tab) => SymTab.insert id pos tab) [] decs; ())
         handle SymTab.Duplicate key =>
                let val pos = case SymTab.lookup key pos_table of
                                  NONE   => (0,0)
                                | SOME p => p
                in raise Error ("duplicate declaration " ^ key, pos)
                end
      end

  (* the type of the currently checked function/procedure *)
  val current_rettp : (TpAbSyn.Type option) ref = ref NONE

  fun showFunType ( argtps : TpAbSyn.Type list, rtp : TpAbSyn.Type option ) : string =
        "( " ^ pp_types argtps ^ " ) -> " ^
        ( case rtp of
            NONE   => "()"
          | SOME t => pp_type t )

  (********************************************************************)
  (*** Expected Type is derived from the context and passed as      ***)
  (***   an inherited attrbiute to help type check an expression    ***)
  (*** Represents either:                                           ***)
  (***   1. an array of unknown rank but known base type, SomeArray ***)
  (***   2. some known type, KnownType, or                          ***)
  (***   3. a completely unknwon type, UnknownType                  ***)
  (********************************************************************)
  datatype ExpectType = SomeArray of TpAbSyn.BasicType
                      | KnownType of TpAbSyn.Type
                      | UnknownType

  fun removeOneArrDim (KnownType (Array (r, btp) ) ) =
        if r = 1
        then KnownType (BType btp)
        else if r > 1
             then (KnownType (Array (r-1, btp)))
             else UnknownType
    | removeOneArrDim _ = UnknownType

  fun expectedBasicType (SomeArray btp) = SOME btp
    | expectedBasicType (KnownType tp ) =
        (  case basicType tp of
             BType btp => SOME btp
           | _ => NONE              )
    | expectedBasicType UnknownType = NONE


  (*******************************************************************************)
  (*******************************************************************************)
  (*** typeCheckExp ( vtab: VTab, f: AbSyn.Exp, etp: ExpectType ): TpAbSyn.Exp ***)
  (***                                                                         ***)
  (*** Type checks an expression:                                              ***)
  (***   Input : 1. the vtable, which associates a variable with its type      ***)
  (***           2. an untyped expression, i.e., `AbSyn.Exp',                  ***)
  (***           3. an expected type,derived from the context (inherited atrib)***)
  (***                                                                         ***)
  (***   Output: a  typed   expression, i.e., `TpAbSyn.Exp',                   ***)
  (***                                                                         ***)
  (***   Remark: 1. we do not return the type as in the book, because          ***)
  (***                TpAbSyn provides function `typeOfExp' that (cheaply)     ***)
  (***                computes the type of an arbitrary expression             ***)
  (***           2. `etp' is used to infer the types of, for example the       ***)
  (***                 `read', and `new' special functions.                    ***)
  (***              Note that we do not check against the expected type, since ***)
  (***              the check is going to happen anyway at the place where     ***)
  (***              the expected type has been computed.                       ***)
  (*******************************************************************************)
  (*******************************************************************************)

  (*  typeCheckExp(vtab : VTab,
                   exp : AbSyn.Exp,
                   expected_type : TpAbSyn.Type ) : TpAbSyn.Exp *)
  fun typeCheckExp( vtab, AbSyn.Literal(AbSyn.BVal v, pos), _ ) =
        Literal(BVal (toTpBValue v), pos)
    | typeCheckExp( vtab, AbSyn.Literal(AbSyn.Arr  _, pos), _ ) =
        raise Error("in type check array value expression: array value " ^
                    " should not have been built (interpretation only), at ", pos)

    | typeCheckExp( vtab, AbSyn.StrLit(strg, pos), _ ) =
        StrLit(strg, pos)

    | typeCheckExp( vtab, AbSyn.ArrLit([], pos), _ ) =
        raise Error("in type check array lit: empty array literal not supported, at ", pos)

    | typeCheckExp( vtab, AbSyn.ArrLit(elms, pos), etp ) =
        let val new_elms = map (fn e => typeCheckExp(vtab, e, removeOneArrDim etp)) elms
            val el_tp    = typeOfExp (hd new_elms)
            (* check that all elements have the same type *)
            val elm_tps  = map typeOfExp new_elms
            val ok_tps   = foldl ( fn (x,b) => b andalso typesEqual(x, el_tp) ) true elm_tps
            val () = if ok_tps then ()
                     else raise Error("in type check array literal, not all elements "^
                                      "have the same type ["^pp_types elm_tps^" at ", pos)

            (* check that either all array literal elements are scalars,
                marked of length 0, or ArrLit, marked with their size *)
            fun checkArrLitElems e =
                case (e, typeOfExp e) of
                    (ArrLit(l,t,p), Array _) => length l
                  | (ArrLit(l,t,p), tp     ) =>
                    raise Error("in type check array literal, impossible, element " ^
                                "is array literal, " ^ pp_exp (ArrLit(l,t,p)) ^
                                " but of non-array type "^ pp_type tp^", at",
                                posOfExp e)
                  | (e, Array _) =>
                    raise Error("in type check array literal, element "^pp_exp e^
                                " of Array type is not an array literal, at", posOfExp e)
                  | (e, tp) => 0  (* fine, scalar type, i.e., length 0 *)

            val ms = map checkArrLitElems new_elms

            (* check that array is regular, i.e., all rows have the same size *)
            val ok_reg_size = foldl (fn (x,b) => (x = hd ms) andalso b) true ms
            val _ = ok_reg_size orelse
                    raise Error("in type check array literal { "^pp_exps new_elms^
                                " rows have different sizes, at ", pos)

            val new_tp = case el_tp of
                           Array(r,btp) => Array(r+1,btp)
                         | BType   btp  => Array(1,  btp)
        in ArrLit(new_elms, new_tp, pos)
        end

    | typeCheckExp( vtab, AbSyn.LValue(AbSyn.Var id, pos), _ ) =
        (case SymTab.lookup id vtab of
             SOME tp => LValue( Var (id, tp), pos )
           | NONE    => raise Error("in type check variable, var "^id^" not in VTab, at ", pos)
        )

    | typeCheckExp( vtab, AbSyn.LValue( AbSyn.Index(id, inds), pos ), _ ) =
        let
            val new_inds = map (fn x => typeCheckExp(vtab, x, KnownType (BType Int))) inds

            (* check all indices evaluate to int scalars *)
            val tp_inds  = map (fn e => typeOfExp e) new_inds
            val ok_inds  = List.all (fn x => typesEqual (x, BType Int)) tp_inds
            val _ = if ok_inds then true
                    else raise Error(" in indexed variable "^id^"[ "^pp_exps new_inds^
                                     " ] not all indices are ints, at ", pos);

            (* check that array is defined and has correct number of dimensions *)
            val id_tp =
                case SymTab.lookup id vtab of
                  SOME (Array(r,btp)) =>
                    if(r = length inds andalso r > 0) then Array(r,btp)
                    else raise Error(" in indexed variable "^id^"[ "^pp_exps new_inds^
                                     " ] num of indexes does not match the array rank,"^
                                     " at ", pos)
                | SOME tp =>
                    raise Error(" in indexed variable "^id^"[ "^pp_exps new_inds^
                                " ], var "^id^" is of non-array type "^pp_type tp^
                                ", at ", pos)
                | NONE    =>
                    raise Error("in type check indexed exp," ^
                                " var "^id^" not in VTab, at ", pos)

        in LValue(Index ((id, id_tp), new_inds), pos)
        end

    | typeCheckExp( vtab, AbSyn.Plus (e1, e2, pos), _ ) =
        let val e1_new = typeCheckExp( vtab, e1, KnownType(BType Int) )
            val e2_new = typeCheckExp( vtab, e2, KnownType(BType Int) )
            val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
        in  if  typesEqual(BType Int, tp1) andalso typesEqual(BType Int, tp2)
            then Plus(e1_new, e2_new, pos)
            else raise Error("in type check plus exp, one argument is not of int type "^
                             pp_type tp1^" and "^pp_type tp2^" at ", pos)
        end

    | typeCheckExp( vtab, AbSyn.Minus (e1, e2, pos), _ ) =
        let val e1_new = typeCheckExp(vtab, e1, KnownType(BType Int) )
            val e2_new = typeCheckExp(vtab, e2, KnownType(BType Int) )
            val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
        in  if  typesEqual(BType Int, tp1) andalso typesEqual(BType Int, tp2)
            then Minus(e1_new, e2_new, pos)
            else raise Error("in type check minus exp, one argument is not of int type "^
                             pp_type tp1^" and "^pp_type tp2^" at ", pos)
        end

    | typeCheckExp( vtab, AbSyn.Times (e1, e2, pos), _ ) =
        let val e1_new = typeCheckExp(vtab, e1, KnownType(BType Int) )
            val e2_new = typeCheckExp(vtab, e2, KnownType(BType Int) )
            val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
        in  if  typesEqual(BType Int, tp1) andalso typesEqual(BType Int, tp2)
            then Times(e1_new, e2_new, pos)
            else raise Error("in type check times exp, one argument is not of int type "^
                             pp_type tp1^" and "^pp_type tp2^" at ", pos)
        end

    | typeCheckExp( vtab, AbSyn.Div (e1, e2, pos), _ ) =
        let val e1_new = typeCheckExp(vtab, e1, KnownType(BType Int) )
            val e2_new = typeCheckExp(vtab, e2, KnownType(BType Int) )
            val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
        in  if  typesEqual(BType Int, tp1) andalso typesEqual(BType Int, tp2)
            then Div(e1_new, e2_new, pos)
            else raise Error("in type check div exp, one argument is not of int type "^
                             pp_type tp1^" and "^pp_type tp2^" at ", pos)
        end


    (* The following allows either left or right to be polymorphic, but not both *)
    | typeCheckExp ( vtab, AbSyn.Equal(e1, e2, pos), _ ) =
      let val e1_try = SOME ( typeCheckExp (vtab, e1, UnknownType) ) handle _ => NONE
          val e2_try = SOME ( typeCheckExp (vtab, e2, UnknownType) ) handle _ => NONE
          val (e1_new, e2_new) =
              case (e1_try, e2_try) of
              (SOME a, SOME b) => (a, b)
            | (SOME a, NONE  ) => (a, typeCheckExp (vtab, e2, KnownType(typeOfExp a)))
            | (NONE  , SOME b) => (typeCheckExp (vtab, e1, KnownType(typeOfExp b)), b)
            | (NONE  , NONE  ) => raise Error("in type check equal, neither operand" ^
                                              "type-checks (possibly polymorphism)," ^
                                              " at ", pos)

          val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)

          val _ = typesEqual(tp1, tp2) orelse
                  raise Error("in type check equal, argument types do not match, "^
                              pp_type tp1^" isn't "^pp_type tp2^" at ", pos)

      in Equal(e1_new, e2_new, pos) end

    (* The following allows either left or right to be polymorphic, but not both *)
    | typeCheckExp ( vtab, AbSyn.Less (e1, e2, pos), _ ) =
      let val e1_try = SOME ( typeCheckExp (vtab, e1, UnknownType) ) handle _ => NONE
          val e2_try = SOME ( typeCheckExp (vtab, e2, UnknownType) ) handle _ => NONE
          val (e1_new, e2_new) =
              case (e1_try, e2_try) of
              (SOME a, SOME b) => (a, b)
            | (SOME a, NONE  ) => (a, typeCheckExp (vtab, e2, KnownType(typeOfExp a)))
            | (NONE  , SOME b) => (typeCheckExp (vtab, e1, KnownType(typeOfExp b)), b)
            | (NONE  , NONE  ) => raise Error("in type check equal, neither operand" ^
                                              "type-checks (possibly polymorphism)," ^
                                              " at ", pos)

          val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)

          val _ = typesEqual(tp1, tp2) orelse
                  raise Error("in type check less, argument types do not match, "^
                              pp_type tp1^" isn't "^pp_type tp2^" at ", pos)

          val _ = case tp1 of
                      Array _ => raise Error("in type check less, first expression "^
                                             pp_exp e1_new^ " is an array (of type) "^
                                             pp_type tp1^" at ", pos)
                    | _ => ()

      in Less(e1_new, e2_new, pos) end

    | typeCheckExp ( vtab, AbSyn.And (e1, e2, pos), _ ) =
      let val e1_new = typeCheckExp(vtab, e1, KnownType (BType Bool) )
          val e2_new = typeCheckExp(vtab, e2, KnownType (BType Bool) )
          val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
      in if typesEqual(BType Bool, tp1) andalso typesEqual(BType Bool, tp2)
         then And(e1_new, e2_new, pos)
         else raise Error("in type check and, one argument is not of bool type "^
                          pp_type tp1^" and "^pp_type tp2^" at ", pos)
      end

    | typeCheckExp ( vtab, AbSyn.Or (e1, e2, pos), _) =
      let val e1_new = typeCheckExp(vtab, e1, KnownType (BType Bool) )
          val e2_new = typeCheckExp(vtab, e2, KnownType (BType Bool) )
          val (tp1, tp2) = (typeOfExp e1_new, typeOfExp e2_new)
      in if  typesEqual(BType Bool, tp1) andalso typesEqual(BType Bool, tp2)
         then Or(e1_new, e2_new, pos)
         else raise Error("in type check and, one argument is not of bool type "^
                          pp_type tp1^" and "^pp_type tp2^" at ", pos)
      end

    | typeCheckExp ( vtab, AbSyn.Not (e, pos), _) =
      let val e_new = typeCheckExp(vtab, e, KnownType (BType Bool))
      in Not(e_new, pos) end

    (********************************************************************************)
    (*** SPECIAL CASES of Function Application (read & new USE THE Expected Type) ***)
    (********************************************************************************)
    | typeCheckExp ( vtab, AbSyn.FunApp ("len", [d,arr], pos), _ ) =
      let val new_d     = typeCheckExp( vtab, d,   KnownType (BType Int) )
          val new_arr   = typeCheckExp( vtab, arr, UnknownType           )
          val (d_tp, arr_tp) = (typeOfExp new_d, typeOfExp new_arr)

          val () = case d_tp of
                       BType Int => ()
                     | tp => raise Error("in type checking call to len, first arg"^
                                         " not an int: "^pp_type tp^", at ", pos)

          val () = case arr_tp of
                       Array _ => ()
                     | tp => raise Error("in type checking call to len, second arg"^
                                         "not an array: "^pp_type tp^", at ", pos)

      in FunApp( ( "len", ([d_tp,arr_tp], SOME (BType Int)) ), [new_d, new_arr], pos )
      end

    | typeCheckExp ( vtab, AbSyn.FunApp ("len", args, pos), _ ) =
        raise Error("In type checking call to function len, "^
                    "len should take two args, an int and an array, given "^
                    Int.toString (length args) ^", at ", pos)

      (* function call to `read' uses expected type to infer the to-be-read result *)
    | typeCheckExp ( vtab, AbSyn.FunApp ("read", [], pos), KnownType (BType btp) ) =
        FunApp( ( "read", ([], SOME (BType btp)) ), [], pos )

    | typeCheckExp ( vtab, AbSyn.FunApp ("read", [], pos), KnownType tp ) =
        raise Error("in type check call to read, type inference fails because "^
                    "expected type is not a basic type: "^pp_type tp^", at ", pos)

    | typeCheckExp ( vtab, AbSyn.FunApp ("read", [], pos), _ ) =
        raise Error("in type check call to read, type inference fails because "^
                    "of unknwon expected type, at ", pos)

      (* function call to `new' uses expected type to infer the to-be-read result *)
  | typeCheckExp ( vtab, AbSyn.FunApp ("new", args, pos), etp ) =
    ( case expectedBasicType etp of
          SOME btp => 
          let val expint  = KnownType (BType Int)
              val new_args= map   ( fn x => typeCheckExp(vtab, x, expint) ) args
              val tps     = map   ( fn x => typeOfExp x ) new_args
              val okargs  = foldl ( fn (x,b)=>typesEqual(x,BType Int) andalso b)
                                  true tps
              val rtp      = Array ( length args, btp )
          in  if okargs andalso (length args) > 0
              then FunApp( ("new", (tps, SOME rtp)),  new_args, pos)
              else raise Error("in call to new, arg not of type int", pos)
          end
        | NONE     => raise Error("type inference fails because "^
                                  "of unknwon expected basic type, at ", pos) )
      (*** EXAM: flat Map ***)
    | typeCheckExp ( vtab, AbSyn.FunApp ("map", args, pos), etp ) =
        raise Error("EXAM TASK requires student implementation, please!", pos)
        (* result should use the TpAbSyn.sml's constructor: Map of FIdent * Exp * Pos *)

      (*** EXAM: flat Reduce ***)
    | typeCheckExp ( vtab, AbSyn.FunApp ("reduce", args, pos), etp ) =
    (* Check if function name exists and returns the type list and tye option, otherwise errors are given*)
        if List.length args = 2
        then let val fid = (case (List.nth (args,0)) of
                            AbSyn.LValue(AbSyn.Var(fid),pos) => fid
                           | _ => raise Error ("Wrong argument at ",pos))
                 val (fun_arg_tps, fun_ret_tp) =
                    case SymTab.lookup fid (!functionTable) of
                      NONE => raise Error("In typeCheck function call, function "^fid^"not in function table, at ", pos)
                    | SOME (tps, SOME rtp) => if ( (length tps) = (length args) ) then (tps, rtp)
                                              else raise Error("In typeCheckExp function "^fid^" call, the number"^" of formal and actual args do not match, at ", pos)
                    | SOME ssig => raise Error("In typeCheck function call, function "^fid^
                                               " of signature "^showFunType ssig^" has no return type at ", pos)
    (* Returns the type of the array *)
                 val arr = List.nth(args,1)
                 val new_arr = typeCheckExp(vtab, arr, etp)
                 val tp1 = typeOfExp new_arr
                 val tp2 = case tp1 of
                           Array(_, b) => b
                          | _ => raise Error("Array in argument is not correct, at ", pos)
    (* Checks if arguments in function matches the BType of the array, otherwise errors are given *)
                 val args_ok = map ( fn e => typesEqual(e, BType tp2) orelse
                                                raise Error("Arguments not of same type, at ", pos)) fun_arg_tps
              in
                Red( (fid, (fun_arg_tps, SOME fun_ret_tp)), new_arr, pos) 
              end
        
        else raise Error("Not correct amount of arguments, at ", pos)

      (*** EXAM: flat zipWith ***)
    | typeCheckExp ( vtab, AbSyn.FunApp ("zipWith", args, pos), etp ) =
        raise Error("EXAM TASK requires student implementation, please!", pos)
        (* result should use TpAbSyn.sml's Exp constructor: ZipWith of FIdent * Exp * Exp* Pos *)

      (* all the other, i.e., regular, function calls *)
    | typeCheckExp ( vtab, AbSyn.FunApp (fid, args, pos), _ ) =
        let val (fun_arg_tps, fun_ret_tp) =
                case SymTab.lookup fid (!functionTable) of
                  NONE => raise Error("In typeCheck function call, function "^fid^" not in function table, at ", pos)
                | SOME (tps, SOME rtp) => if ( (length tps) = (length args) ) then (tps, rtp)
                                          else raise Error("In typeCheckExp function "^fid^" call, the number"^
                                                           " of formal and actual args do not match, at ", pos)
                | SOME ssig => raise Error("In typeCheck function call, function "^fid^
                                           " of signature "^showFunType ssig^" has no return type at ", pos)

            val exp_arg_tps = map ( fn t => KnownType t ) fun_arg_tps
            val new_args    = ListPair.map
                                  ( fn (e,et) => typeCheckExp(vtab, e, et) )
                                  (args, exp_arg_tps)
            val args_tps    = map ( fn e => typeOfExp e ) new_args

            (* check that the type of actual and formal parameters match *)
            val tpok = ListPair.foldl
                           (fn (t1, t2, b) => b andalso typesEqual(t1,t2)) true
                           (fun_arg_tps, args_tps)
        in  if tpok
                 (* the function Ident now contains also the return type of the function *)
            then FunApp( (fid,(fun_arg_tps,SOME fun_ret_tp)), new_args, pos )
            else raise Error("In typeCheck procedure call stmt "^fid^
                             ", actual and formal argument types do not match, at ", pos)
        end



  (*******************************************************************************)
  (*******************************************************************************)
  (***       typeCheckStmt ( f : AbSyn.Stmt ) : TpAbSyn.Stmt                   ***)
  (***                                                                         ***)
  (*** Type checks a statement:                                                ***)
  (***   Input:  an untyped statement, i.e., `AbSyn.Stmt',                     ***)
  (***   Output: a  typed   statement, i.e., `TpAbSyn.Stmt',                   ***)
  (*******************************************************************************)
  (*******************************************************************************)

  (*  typeCheckStmt ( vtab : (string * Type), stmt : AbSyn.Stmt ) : TpAbSyn.Stmt *)
  fun typeCheckStmt ( vtab, AbSyn.Return( eopt, pos ) ) =
      ( case (eopt, !current_rettp) of
          (NONE,   NONE  ) => Return (NONE, pos)
        | (SOME e, SOME t) =>
            let val new_e = typeCheckExp(vtab, e, KnownType t)
                val tp_e  = typeOfExp new_e
            in  if typesEqual( t, typeOfExp(new_e) )
                then Return(SOME new_e, pos)
                else raise Error("In typeCheck Return Stmt: return type of fun "^pp_type t ^
                                 " does not match the type of result exp: "^pp_type tp_e^" at ", pos)
            end
        | (_, _) => raise Error("In typeCheck Return Stmt, one of the fun/stmt " ^
                                "return type is NONE, the other is not, at ", pos)  )

    | typeCheckStmt ( vtab, AbSyn.ProcCall( "write", [a], pos ) ) =
        let val a_new = typeCheckExp( vtab, a, UnknownType )
        in  ProcCall( ("write", ([typeOfExp a_new], NONE)), [a_new], pos )
        end
    | typeCheckStmt ( vtab, AbSyn.ProcCall( "write", args, pos ) ) =
        raise Error("In typeCheck procedure call stmt: write should receive exactly one argument, at ", pos)


        (* treat the other, i.e., regular, procedure calls *)
    | typeCheckStmt ( vtab, AbSyn.ProcCall( fid, args, pos ) ) =
        let val proc_arg_tps =
                case SymTab.lookup fid (!functionTable) of
                  NONE => raise Error("In typeCheck procedure call stmt, procedure "^fid^" not in function table, at ", pos)
                | SOME (tps, _) => if ( (length tps) = (length args) ) then tps
                                   else raise Error("In typeCheckStmt procedure "^fid^" call, the number"^
                                                    " of formal and actual args do not match, at ", pos)

            (* building the expected types for the arguments *)
            val expect_arg_tps = map (fn t => KnownType t) proc_arg_tps

            val new_args = ListPair.map
                               ( fn (e,etp) => typeCheckExp(vtab, e, etp) )
                               (args, expect_arg_tps)
            val args_tps = map ( fn e => typeOfExp e ) new_args
            val tpok = ListPair.foldl
                           (fn (t1, t2, b) => b andalso typesEqual(t1,t2)) true
                           (proc_arg_tps, args_tps)
        in  if tpok
            then ProcCall( (fid, (proc_arg_tps, NONE)), new_args, pos )
            else raise Error("procedure call stmt "^fid^
                             ", actual and formal argument types do not match, at ", pos)
        end
    | typeCheckStmt( vtab, AbSyn.Assign(AbSyn.Var(id), e, pos) ) =
        let val id_tp = (case SymTab.lookup id vtab of
                             SOME tp => tp
                           | NONE    => raise Error("var "^id^" not in VTab, at ", pos)
                        )
            val new_e = typeCheckExp(vtab, e, KnownType id_tp)
            val e_tp  = typeOfExp new_e

        in  if typesEqual(id_tp, e_tp)
            then Assign( Var(id,id_tp) , new_e, pos )
            else raise Error( "assignment stmt for var "^ id ^", its type is "^
                              pp_type id_tp^" but assigned exp type "^pp_type e_tp^" does not match, at ", pos )
        end
    | typeCheckStmt( vtab, AbSyn.Assign(AbSyn.Index(id, inds), e, pos) ) =
        let val new_ind_e = typeCheckExp( vtab, AbSyn.LValue (AbSyn.Index(id, inds), pos), UnknownType )
            val tp_ind    = typeOfExp new_ind_e
            val lval_ind  = case new_ind_e of
                              LValue(Index (idd,indds), _) => Index (idd, indds)
                            | _ => raise Error("impossible in type check indexed assignm, indexed var "^
                                               " translated to " ^pp_exp new_ind_e ^ " at ", pos )
            val new_e = typeCheckExp(vtab, e, KnownType tp_ind)
            val e_tp  = typeOfExp new_e
        in  if typesEqual(tp_ind, e_tp)
            then Assign( lval_ind, new_e, pos )
            else raise Error("in type check assignment (index) stmt, type of indexed var "^
                             pp_type tp_ind^" and of rhs exp "^pp_type e_tp^" do not match, at ", pos)
        end
    | typeCheckStmt( vtab, AbSyn.IfThEl(cond, then_blk, else_blk, pos) ) =
        let val new_cond = typeCheckExp( vtab, cond, KnownType (BType Bool) )
            val cond_tp  = typeOfExp    new_cond
            val new_then = typeCheckBlock( vtab, then_blk )
            val new_else = typeCheckBlock( vtab, else_blk )
        in  if  typesEqual( cond_tp, BType Bool )
            then IfThEl( new_cond, new_then, new_else, pos)
            else raise Error("in type check if-then-else statement, illegal condition type "^pp_type cond_tp^" at ", pos)
        end
    | typeCheckStmt( vtab, AbSyn.While(cond, body, pos) ) =
        let val new_cond = typeCheckExp(vtab, cond, KnownType (BType Bool) )
            val cond_tp  = typeOfExp    new_cond
            val new_body = typeCheckBlock( vtab, body )
        in  if  typesEqual( cond_tp, BType Bool )
            then While( new_cond, new_body, pos)
            else raise Error("in type check while statement, illegal condition type "^pp_type cond_tp^" at ", pos)
        end

  (*******************************************************************************)
  (*******************************************************************************)
  (***       typeCheckBlock ( f : AbSyn.StmtBlock ) : TpAbSyn.StmtBlock        ***)
  (***                                                                         ***)
  (*** Type checks a block of statements:                                      ***)
  (***   Input:  an untyped block, i.e., `AbSyn.StmtBlock',                    ***)
  (***   Output: a  typed   block, i.e., `TpAbSyn.StmtBlock',                  ***)
  (***   Does:   type checks & translates each decl and stmt of the block      ***)
  (*******************************************************************************)
  (*******************************************************************************)

  (*  typeCheckBlock ( vtab : (string * Type) list, b : AbSyn.StmtBlock ) : TpAbSyn.StmtBlock *)
  and typeCheckBlock ( vtab : VTab, AbSyn.Block( decs, stmts ) ) : TpAbSyn.StmtBlock =
        let val new_decs = map ( fn (AbSyn.Dec(id,tp,p)) => Dec((id,toTpAbSynType tp),p) ) decs
            val ()       = checkDuplicateDecs new_decs
            val ds = map ( fn (Dec(idtp,_)) => idtp ) new_decs
            val new_vtab = ds @ vtab
            val new_stmts= map (fn stmt => typeCheckStmt(new_vtab, stmt)) stmts
        in  Block(new_decs, new_stmts)
        end

  (*******************************************************************************)
  (*******************************************************************************)
  (***       typeCheckFun ( f : AbSyn.FunDec ) : TpAbSyn.FunDec                ***)
  (***                                                                         ***)
  (*** Functions are guaranteed by syntax to have a known declared type.       ***)
  (***     This type is recorded in global variable `current_rettp' so that    ***)
  (***     return statements can be verified.                                  ***)
  (***   Input:  an untyped fun/procedure declaration, i.e., `AbSyn.FunDec',   ***)
  (***   Output: a  typed   fun/procedure declaration, i.e., `TpAbSyn.FunDec', ***)
  (***           in which for example the variable identifiers also records    ***)
  (***           the type. In particular, the type of any expression is        ***)
  (***           available via `TpAbSyn.typeOfExp' function.                   ***)
  (*******************************************************************************)
  (*******************************************************************************)

  (*  typeCheckFun ( f : AbSyn.FunDec ) : TpAbSyn.FunDec *)
  fun typeCheckFun ( AbSyn.Func (old_rtp, fnm, old_decs, old_body, pos) ) =
        let val new_rtp = toTpAbSynType old_rtp
            val ()      = current_rettp := SOME new_rtp
            val new_decs= map ( fn (  AbSyn.Dec (id,tp,p)) => Dec((id, toTpAbSynType tp),p) ) old_decs
            val ()      = checkDuplicateDecs new_decs
            val vtab    = map ( fn (Dec (idtp, _)) => idtp ) new_decs
            val ()      = current_rettp := (SOME new_rtp)
            val new_body= typeCheckBlock(vtab, old_body)
        in  Func ( new_rtp, fnm, new_decs, new_body, pos )
        end

    | typeCheckFun ( AbSyn.Proc ( fnm, old_decs, old_body, pos) ) =
        let val ()      = current_rettp := NONE
            val new_decs= map ( fn (  AbSyn.Dec (id,tp,p)) => Dec((id, toTpAbSynType tp),p) ) old_decs
            val ()      = checkDuplicateDecs new_decs
            val vtab    = map ( fn (Dec (idtp, _)) => idtp ) new_decs
            val ()      = current_rettp := NONE
            val new_body= typeCheckBlock(vtab, old_body)
        in  Proc ( fnm, new_decs, new_body, pos )
        end


  (*******************************************************************************)
  (***       typeCheckPgm ( f : AbSyn.FunDec list ) : TpAbSyn.FunDec list      ***)
  (***                                                                         ***)
  (*** All functions and procedures are type checked and are translated        ***)
  (***     to a typed abstract syntax tree, i.e., TpAbSyn.FunDec list,         ***)
  (***     which records the information necessary to querry the type of       ***)
  (***     each expression for example.                                        ***)
  (*******************************************************************************)
  fun typeCheckPgm old_fun_decs =
   let fun getType ( f : AbSyn.FunDec ) : (string * (TpAbSyn.Type list * TpAbSyn.Type option) ) =
                let val fnm    = AbSyn.getFunName f
                    val rtp    = AbSyn.getFunRetp f
                    val args   = AbSyn.getFunArgs f
                    val new_tps= map (fn AbSyn.Dec(id, tp, _) => toTpAbSynType tp) args
                in  ( fnm, (new_tps, toTpAbSynOptType rtp) )
                end
       val pos_table = map (fn f => (AbSyn.getFunName f, AbSyn.getFunPos f)) old_fun_decs
       val fun_table = map getType old_fun_decs
   in
    let val () = functionTable := List.foldr
                                    (fn ((n,ts),tab) => SymTab.insert n ts tab)
                                    (!functionTable) fun_table
        (* function table was passed around everywhere, could be removed *)
        val decorated = map typeCheckFun old_fun_decs
        val main_pos  = case SymTab.lookup "main" pos_table of
                            NONE     => raise Error ("No main function defined.", (0,0))
                          | SOME pos => pos
    in
        (* check main function presence and type () -> () *)
        case SymTab.lookup "main" (!functionTable) of
            NONE            => raise Error ("impossible case!", main_pos)
          | SOME ([], NONE) => decorated (* all fine, return *)
          | SOME (args,res) =>
               raise Error ("Unexpected argument to main: "
                            ^ showFunType (args,res)
                            ^ ", should be () -> ().", main_pos)
    end
    handle SymTab.Duplicate key
           => let val pos = case SymTab.lookup key pos_table of
                                NONE   => (0,0)
                              | SOME p => p
              in raise Error ("duplicate function " ^ key, pos)
              end
   handle TpAbSyn.Error(err, pos)
          => raise Error("Type Checker found error in TpAbSyn, "^err, pos)
   end

end

