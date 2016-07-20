structure LL1Parser =
struct

open AbSyn

(* Token type, used by the lexer as well *)
(* type Pos = int * int
type Ident = string
*)

datatype Token =
         (* Keywords *)
         TProgram of Pos | TFunction of Pos | TProcedure of Pos
       | TVar of Pos | TBegin of Pos | TEnd of Pos
       | TIf of Pos | TThen of Pos | TElse of Pos
       | TWhile of Pos | TDo of Pos | TReturn of Pos
       (* Type keywords *)
       | TArray of Pos | TOf of Pos
       | TInt of Pos | TChar of Pos | TBool of Pos
       (* Symbols *)
       | TSemi of Pos | TColon of Pos | TComma of Pos | TAssign of Pos
       (* Operations *)
       | TPlus of Pos | TMinus of Pos | TTimes of Pos | TSlash of Pos (*arith*)
       | TEq of Pos | TLess of Pos | TAnd of Pos (*comparison and bool*)
       (* Parentheses of different kind *)
       | TLParen of Pos   | TRParen of Pos
       | TLCurly of Pos   | TRCurly of Pos
       | TLBracket of Pos | TRBracket of Pos
       (* Identifiers *)
       | TId of Ident * Pos
       (* Literals *)
       | TNLit of int  * Pos
       | TBLit of bool * Pos
       | TCLit of char * Pos
       | TSLit of string * Pos
       (* EOF special token *)
       | TEOF of Pos

(* ************************* *)

(* all tokens by their name above: *)
val tnames = ["TProgram","TFunction","TProcedure","TVar","TBegin","TEnd"
            ,"TIf","TThen","TElse","TWhile","TDo","TReturn"
            ,"TArray","TOf","TInt","TChar","TBool"
            ,"TSemi","TColon","TComma","TAssign","TPlus","TMinus",
              "TTimes","TSlash","TEq","TLess","TAnd"
            ,"TLParen","TRParen","TLCurly","TRCurly","TLBracket","TRBracket"
            ,"TId","TNLit","TBLit","TCLit","TSLit","TEOF"]

(* These functions generate some code pasted into this file manually later *)

(* this functin is specified in SML, but not implemented in older Moscow ML *)
fun concatWith inter ss
  = let fun intersperse []        = []
          | intersperse [s]       = [s]
          | intersperse (s::rest) = s :: inter :: intersperse rest
    in String.concat (intersperse ss)
    end

fun cases f = concatWith "\n   | " ( List.map f tnames)

(* predicates on all tokens *)
fun mkPred s = "fun is" ^ s ^ " () = case !next of "
               ^ s ^ " _ => true | _ => false\n"
val mkPreds = concat (List.map mkPred tnames)

(* toString function for all tokens. Adapt for Id and Literals *)
val mkshowT = "fun   " ^
              cases (fn s => "showT (" ^ s ^ " (l,c)) = \"" ^ s ^
                              " \" ^ makestring l ^ \":\" ^ makestring c")

(* function to consume a token, uses showT in error msg. *)
val mkconsume
  = "fun consume lex (t : Token) = case ( !next, t ) of\n     "
    ^ cases (fn s =>  "(" ^ s ^ " _," ^ s ^ " _) => advance lex")
    ^ "\n   | other => parseError (\"unexpected token\" ^ showT (!next) ^\n"
    ^ "                            \", expected \" ^ showT t)"

(* selecting the position. Needs to be adapted a bit for Id and Literals *)
val mkgetPos = "fun   " ^ cases (fn s => " getPos (" ^ s ^ " p ) = p") ^ "\n"

(* ************************* *)

(* the current token is stored here. need to start somewhere, so TEOF*)
val next = ref (TEOF (0,0) : Token)

(* this fills the next ref and returns the previous (overwritten) token *)
fun advance lexer = let val prev = !next
                    in next := lexer (); prev
                    end

(* Use "raise Error (message)" for error messages *)
exception Error of string

(* extract position from a token. Auto-generated (and adapted later) *)
fun getPos (TProgram p ) = p
   |  getPos (TFunction p ) = p
   |  getPos (TProcedure p ) = p
   |  getPos (TVar p ) = p
   |  getPos (TBegin p ) = p
   |  getPos (TEnd p ) = p
   |  getPos (TIf p ) = p
   |  getPos (TThen p ) = p
   |  getPos (TElse p ) = p
   |  getPos (TWhile p ) = p
   |  getPos (TDo p ) = p
   |  getPos (TReturn p ) = p
   |  getPos (TArray p ) = p
   |  getPos (TOf p ) = p
   |  getPos (TInt p ) = p
   |  getPos (TChar p ) = p
   |  getPos (TBool p ) = p
   |  getPos (TSemi p ) = p
   |  getPos (TColon p ) = p
   |  getPos (TComma p ) = p
   |  getPos (TAssign p ) = p
   |  getPos (TPlus p ) = p
   |  getPos (TMinus p ) = p
   |  getPos (TTimes p ) = p
   |  getPos (TSlash p ) = p
   |  getPos (TEq p ) = p
   |  getPos (TLess p ) = p
   |  getPos (TAnd p ) = p
   |  getPos (TLParen p ) = p
   |  getPos (TRParen p ) = p
   |  getPos (TLCurly p ) = p
   |  getPos (TRCurly p ) = p
   |  getPos (TLBracket p ) = p
   |  getPos (TRBracket p ) = p
   |  getPos (TId p )   = #2 p
   |  getPos (TNLit p ) = #2 p
   |  getPos (TBLit p ) = #2  p
   |  getPos (TCLit p ) = #2 p
   |  getPos (TSLit p ) = #2 p
   |  getPos (TEOF p )  = p

(* predicates on the lookahead. Auto-generated *)
fun isTProgram () = case !next of TProgram _ => true | _ => false
fun isTFunction () = case !next of TFunction _ => true | _ => false
fun isTProcedure () = case !next of TProcedure _ => true | _ => false
fun isTVar () = case !next of TVar _ => true | _ => false
fun isTBegin () = case !next of TBegin _ => true | _ => false
fun isTEnd () = case !next of TEnd _ => true | _ => false
fun isTIf () = case !next of TIf _ => true | _ => false
fun isTThen () = case !next of TThen _ => true | _ => false
fun isTElse () = case !next of TElse _ => true | _ => false
fun isTWhile () = case !next of TWhile _ => true | _ => false
fun isTDo () = case !next of TDo _ => true | _ => false
fun isTReturn () = case !next of TReturn _ => true | _ => false
fun isTArray () = case !next of TArray _ => true | _ => false
fun isTOf () = case !next of TOf _ => true | _ => false
fun isTInt () = case !next of TInt _ => true | _ => false
fun isTChar () = case !next of TChar _ => true | _ => false
fun isTBool () = case !next of TBool _ => true | _ => false
fun isTSemi () = case !next of TSemi _ => true | _ => false
fun isTColon () = case !next of TColon _ => true | _ => false
fun isTComma () = case !next of TComma _ => true | _ => false
fun isTAssign () = case !next of TAssign _ => true | _ => false
fun isTPlus () = case !next of TPlus _ => true | _ => false
fun isTMinus () = case !next of TMinus _ => true | _ => false
fun isTTimes () = case !next of TTimes _ => true | _ => false
fun isTSlash () = case !next of TSlash _ => true | _ => false
fun isTEq () = case !next of TEq _ => true | _ => false
fun isTLess () = case !next of TLess _ => true | _ => false
fun isTAnd () = case !next of TAnd _ => true | _ => false
fun isTLParen () = case !next of TLParen _ => true | _ => false
fun isTRParen () = case !next of TRParen _ => true | _ => false
fun isTLCurly () = case !next of TLCurly _ => true | _ => false
fun isTRCurly () = case !next of TRCurly _ => true | _ => false
fun isTLBracket () = case !next of TLBracket _ => true | _ => false
fun isTRBracket () = case !next of TRBracket _ => true | _ => false
fun isTId () = case !next of TId _ => true | _ => false
fun isTNLit () = case !next of TNLit _ => true | _ => false
fun isTBLit () = case !next of TBLit _ => true | _ => false
fun isTCLit () = case !next of TCLit _ => true | _ => false
fun isTSLit () = case !next of TSLit _ => true | _ => false
fun isTEOF () = case !next of TEOF _ => true | _ => false

(* token-to-string and position function for better error messages *)
fun   showT (TProgram (l,c)) = "TProgram " ^ makestring l ^ ":" ^ makestring c
    | showT (TFunction (l,c)) = "TFunction " ^ makestring l ^ ":" ^ makestring c
    | showT (TProcedure (l,c)) = "TProcedure " ^ makestring l ^ ":" ^ makestring c
    | showT (TVar (l,c)) = "TVar " ^ makestring l ^ ":" ^ makestring c
    | showT (TBegin (l,c)) = "TBegin " ^ makestring l ^ ":" ^ makestring c
    | showT (TEnd (l,c)) = "TEnd " ^ makestring l ^ ":" ^ makestring c
    | showT (TIf (l,c)) = "TIf " ^ makestring l ^ ":" ^ makestring c
    | showT (TThen (l,c)) = "TThen " ^ makestring l ^ ":" ^ makestring c
    | showT (TElse (l,c)) = "TElse " ^ makestring l ^ ":" ^ makestring c
    | showT (TWhile (l,c)) = "TWhile " ^ makestring l ^ ":" ^ makestring c
    | showT (TDo (l,c)) = "TDo " ^ makestring l ^ ":" ^ makestring c
    | showT (TReturn (l,c)) = "TReturn " ^ makestring l ^ ":" ^ makestring c
    | showT (TArray (l,c)) = "TArray " ^ makestring l ^ ":" ^ makestring c
    | showT (TOf (l,c)) = "TOf " ^ makestring l ^ ":" ^ makestring c
    | showT (TInt (l,c)) = "TInt " ^ makestring l ^ ":" ^ makestring c
    | showT (TChar (l,c)) = "TChar " ^ makestring l ^ ":" ^ makestring c
    | showT (TBool (l,c)) = "TBool " ^ makestring l ^ ":" ^ makestring c
    | showT (TSemi (l,c)) = "TSemi " ^ makestring l ^ ":" ^ makestring c
    | showT (TColon (l,c)) = "TColon " ^ makestring l ^ ":" ^ makestring c
    | showT (TComma (l,c)) = "TComma " ^ makestring l ^ ":" ^ makestring c
    | showT (TAssign (l,c)) = "TAssign " ^ makestring l ^ ":" ^ makestring c
    | showT (TPlus (l,c)) = "TPlus " ^ makestring l ^ ":" ^ makestring c
    | showT (TMinus (l,c)) = "TMinus " ^ makestring l ^ ":" ^ makestring c
    | showT (TTimes (l,c)) = "TTimes " ^ makestring l ^ ":" ^ makestring c
    | showT (TSlash (l,c)) = "TSlash " ^ makestring l ^ ":" ^ makestring c
    | showT (TEq (l,c)) = "TEq " ^ makestring l ^ ":" ^ makestring c
    | showT (TLess (l,c)) = "TLess " ^ makestring l ^ ":" ^ makestring c
    | showT (TAnd (l,c)) = "TAnd " ^ makestring l ^ ":" ^ makestring c
    | showT (TLParen (l,c)) = "TLParen " ^ makestring l ^ ":" ^ makestring c
    | showT (TRParen (l,c)) = "TRParen " ^ makestring l ^ ":" ^ makestring c
    | showT (TLCurly (l,c)) = "TLCurly " ^ makestring l ^ ":" ^ makestring c
    | showT (TRCurly (l,c)) = "TRCurly " ^ makestring l ^ ":" ^ makestring c
    | showT (TLBracket (l,c)) = "TLBracket " ^ makestring l ^ ":" ^ makestring c
    | showT (TRBracket (l,c)) = "TRBracket " ^ makestring l ^ ":" ^ makestring c
    | showT (TId   (n,(l,c))) = "TId " ^ makestring l ^ ":" ^ makestring c
    | showT (TNLit (n,(l,c))) = "TNLit " ^ makestring l ^ ":" ^ makestring c
    | showT (TBLit (n,(l,c))) = "TBLit " ^ makestring l ^ ":" ^ makestring c
    | showT (TCLit (n,(l,c))) = "TCLit " ^ makestring l ^ ":" ^ makestring c
    | showT (TSLit (n,(l,c))) = "TSLit " ^ makestring l ^ ":" ^ makestring c
    | showT (TEOF (l,c)) = "TEOF " ^ makestring l ^ ":" ^ makestring c

(* on errors, extract position from current token (!next) in error msg. *)
fun parseError s = let val p    = getPos (!next)
                       val pstr = "At position (" ^ makestring (#1 p)
                                  ^ ", " ^ makestring (#2 p) ^ ")"
                   in raise Error ( pstr ^ ": " ^ s)
                   end

(* consume an expected token and return it. Auto-generated *)
fun consume lex (t : Token) = case ( !next, t ) of
     (TProgram _,TProgram _) => advance lex
   | (TFunction _,TFunction _) => advance lex
   | (TProcedure _,TProcedure _) => advance lex
   | (TVar _,TVar _) => advance lex
   | (TBegin _,TBegin _) => advance lex
   | (TEnd _,TEnd _) => advance lex
   | (TIf _,TIf _) => advance lex
   | (TThen _,TThen _) => advance lex
   | (TElse _,TElse _) => advance lex
   | (TWhile _,TWhile _) => advance lex
   | (TDo _,TDo _) => advance lex
   | (TReturn _,TReturn _) => advance lex
   | (TArray _,TArray _) => advance lex
   | (TOf _,TOf _) => advance lex
   | (TInt _,TInt _) => advance lex
   | (TChar _,TChar _) => advance lex
   | (TBool _,TBool _) => advance lex
   | (TSemi _,TSemi _) => advance lex
   | (TColon _,TColon _) => advance lex
   | (TComma _,TComma _) => advance lex
   | (TAssign _,TAssign _) => advance lex
   | (TPlus _,TPlus _) => advance lex
   | (TMinus _,TMinus _) => advance lex
   | (TTimes _,TTimes _) => advance lex
   | (TSlash _,TSlash _) => advance lex
   | (TEq _,TEq _) => advance lex
   | (TLess _,TLess _) => advance lex
   | (TAnd _,TAnd _) => advance lex
   | (TLParen _,TLParen _) => advance lex
   | (TRParen _,TRParen _) => advance lex
   | (TLCurly _,TLCurly _) => advance lex
   | (TRCurly _,TRCurly _) => advance lex
   | (TLBracket _,TLBracket _) => advance lex
   | (TRBracket _,TRBracket _) => advance lex
   | (TId _,TId _) => advance lex
   | (TNLit _,TNLit _) => advance lex
   | (TBLit _,TBLit _) => advance lex
   | (TCLit _,TCLit _) => advance lex
   | (TSLit _,TSLit _) => advance lex
   | (TEOF _,TEOF _) => advance lex
   | other => parseError ("unexpected token" ^ showT (!next) ^
                            ", expected " ^ showT t)

(* ************************* *)

(* functions to extract Token content and convert to AbSyn values*)

fun getName (TId (name,_)) = name
  | getName other          = raise Error ("getName: not an identifier Token")

fun absynType (TInt p)  = AbSyn.Int p
  | absynType (TBool p) = AbSyn.Bool p
  | absynType (TChar p) = AbSyn.Char p
  | absynType other     = raise Error ("absynType: not a base type token")

(* ************************* *)

(* Parser code *)

val nopos  = (0,0)
val noid   = ( "" ,nopos)
val noint  = ( 0  ,nopos)
val nobool = (true,nopos)
val nochar = (#"?",nopos)


(* special return types for some productions *)
datatype IR  = IRAssign of Exp list * Exp
             | IRCall   of Exp list
datatype BR2 = BR2Type of  Type * Dec list * Stmt list
             | BR2Stmt of IR

(* look-ahead filled before, using eat-functions defined from token function *)
fun parse token lexbuf
  = let val _ = next := token lexbuf
        fun eat t     = consume (fn () => token lexbuf) t
        fun eatAny () = advance (fn () => token lexbuf)

        (* [Prog       -> program ID ; FunDecs]         program *)
        fun Prog () = let val id    = (eat (TProgram nopos); eat (TId noid))
                          val funs  = (eat (TSemi nopos); FunDecs() )
                      in funs (* AbSyn does not use name *)
                      end

        (* [FunDecs    -> Fun FD']                      function *)
        and FunDecs () = if isTFunction () orelse isTProcedure ()
                         then Fun() :: FD()
                         else parseError "expected function or procedure"

        (* [Fun        -> function ID ( Ps ) : Type Block ;]  function *)
        (* [Fun        -> procedure ID ( Ps ) Block ;]        procedure *)
        and Fun () = case !next of
                         TFunction p
                         => let val id     = (eatAny ();
                                              eat (TId noid))
                                val params = (eat (TLParen nopos); Ps() )
                                val typ    = (eat (TRParen nopos);
                                              eat (TColon nopos); Type() )
                                val body   = Block()
                            in (eat (TSemi nopos);
                                Func (typ, getName id, params, body, p))
                            end
                       | TProcedure p
                         => let val id     = (eat (TProcedure nopos);
                                              eat (TId noid))
                                val params = (eat (TLParen nopos); Ps() )
                                val body   = (eat (TRParen nopos); Block() )
                            in (eat (TSemi nopos);
                                AbSyn.Proc (getName id, params, body, p))
                            end
                       | other => parseError "expected function or procedure"

        (* [FD'        -> Fun FD']                      function procedure *)
        (* [FD'        -> \e]                           \e *)
        and FD () = case !next of
                        TFunction _  => Fun() :: FD()
                      | TProcedure _ => Fun() :: FD()
                      | TEOF _       => []
                      | other
                        => parseError ("expected function, procedure or end")

        (* [Block      -> var Dec ; BRest]              var *)
        (* [Block      -> begin Stmt ; SSeq end]        begin *)
        (* [Block      -> Stmt]                         if while return ID *)
        and Block () (* -> (Decs,Stmts) *)
          = case !next of
                TVar _ => let val dec  = (eatAny (); Dec() )
                              val rest = (eat (TSemi nopos);BRest())
                          in AbSyn.Block (dec :: #1 rest, #2 rest)
                          end
              | TBegin _ => let val s1 = (eatAny (); Stmt() )
                                val ss = (eat (TSemi nopos); SSeq() )
                            in (eat (TEnd nopos); AbSyn.Block ([], s1::ss ))
                            end
              | other => if isTIf() orelse isTWhile()
                            orelse isTReturn() orelse isTId()
                         then AbSyn.Block ( [] , [ Stmt() ] )
                else parseError ("expected statement or start of a block")

        (* [BRest      -> ID BRest2]                    ID *)
        (* [BRest      -> if Exp then Block IfRest]     if *)
        (* [BRest      -> while Exp do Block]           while *)
        (* [BRest      -> return Ret]                   return *)
        (* [BRest      -> begin Stmt ; SSeq end]        begin *)
        and BRest () = case !next of
                           TId (n,p)
                           => (eatAny();case BRest2() of
                                BR2Stmt (IRCall es) (* id started procCall *)
                                => ( [], [ProcCall (n, es, p)] )
                              | BR2Stmt (IRAssign ([], e)) (* assign var *)
                                => ( [], [Assign (Var n, e, p)] )
                              | BR2Stmt (IRAssign (es, e)) (* assign idx *)
                                => ( [], [Assign (Index (n,es), e, p)] )
                              | BR2Type (t,ds,ss) (* type decl and rest *)
                                => (AbSyn.Dec (n, t, p) :: ds , ss )
                              )
                         | TIf p
                           => let val e = (eatAny(); Exp() )
                                  val b = (eat (TThen nopos); Block() )
                                  val r = IfRest()
                              in ([], [ IfThEl (e, b, r, p) ] )
                              end
                         | TWhile p
                           => let val e = (eatAny(); Exp() )
                                  val b = (eat (TDo nopos); Block() )
                              in ([], [ While (e, b, p) ] )
                              end
                         | TReturn p
                           => (eatAny(); let val e = Ret()
                                         in ([], [Return ( e, p )] )
                                         end
                              )
                         | TBegin p
                           => let val s1 = (eatAny(); Stmt() )
                                  val ss = (eat (TSemi nopos); SSeq() )
                              in ( eat (TEnd nopos); ( [], s1 :: ss ))
                              end
                         | other
                           => parseError "expected statement or identifier"

        (* [BRest2     -> : Type ; BRest]               : *)
        (* [BRest2     -> IRest ]                       [ := ( *)
        and BRest2 ()
          = case !next of
                TColon _ => let val t = (eatAny(); Type() )
                                  val r = (eat (TSemi nopos);
                                           BRest() )
                              in (BR2Type (t,#1 r, #2 r))
                              end
              | TLParen _   => BR2Stmt ( IRest() )
              | TLBracket _ => BR2Stmt ( IRest() )
              | TAssign _   => BR2Stmt ( IRest() )
              | other => parseError "expected one of ':', '[', ':=', '('"

        (* [SSeq       -> Stmt ; SSeq]                  if while return ID *)
        (* [SSeq       -> \e]                           end *)
        and SSeq () = if isTIf () orelse isTWhile ()
                         orelse isTReturn () orelse isTId ()
                      then let val s1 = Stmt()
                               val ss = (eat (TSemi nopos); SSeq() )
                           in ( s1 :: ss )
                           end
                      else if isTEnd () then []
                           else parseError "expected statement or 'end'"

        (* [Ps         -> Dec PR]                       ID *)
        (* [Ps         -> \e]                           ) *)
        and Ps () = if isTId()
                    then Dec() :: PR()
                    else
                    if isTRParen()
                    then []
                    else parseError "expected Id or ')'"

        (* [PR         -> ; Dec PR]                     ; *)
        (* [PR         -> \e]                           ) *)
        and PR () = if isTSemi()
                    then let val d = (eatAny (); Dec() )
                         in d :: PR()
                         end
                    else if isTRParen() then []
                    else parseError "expected ';' or ')'"

        (* [Dec        -> ID : Type]                    ID *)
        and Dec () = let val id = eat (TId noid)
                         val t  = (eat (TColon nopos); Type() )
                     in AbSyn.Dec (getName id, t, getPos id)
                     end

        (* [Type       -> int]                          int *)
        (* [Type       -> bool]                         bool *)
        (* [Type       -> char]                         char *)
        (* [Type       -> array of Type]                array *)
        and Type () = if isTInt() orelse isTBool() orelse isTChar()
                      then absynType (eatAny ())
                      else
                      if isTArray()
                      then let val p = getPos (!next)
                           in (eatAny(); eat (TOf nopos); Array (Type(),p) )
                           end
                      else parseError "expected a type"

        (* [Stmt       -> if Exp then Block IfRest]     if *)
        (* [Stmt       -> while Exp do Block]           while *)
        (* [Stmt       -> return Ret]                   return *)
        (* [Stmt       -> ID IRest]                     ID *)
        and Stmt () = case !next of
                           TIf p
                           => let val e = (eatAny(); Exp() )
                                  val b = (eat (TThen nopos); Block() )
                                  val r = IfRest()
                              in IfThEl (e, b, r, p)
                              end
                         | TWhile p
                           => let val e = (eatAny(); Exp() )
                                  val b = (eat (TDo nopos); Block() )
                              in While (e, b, p)
                              end
                         | TReturn p
                           => (eatAny();
                               Return ( Ret(), p ))
                         | TId (n,p)
                           => (eatAny (); case IRest() of
                                IRCall es (* id started procCall *)
                                => ProcCall (n, es, p)
                              | IRAssign ([], e) (* assign var *)
                                => Assign (Var n, e, p)
                              | IRAssign (es, e) (* assign idx *)
                                => Assign (Index (n,es), e, p)
                              )
                         | other
                           => parseError
                                 "expected if, while, return, or an identifier"

        (* [RET        -> Exp]       { ( LogLit NumLit CharLit ID *)
        (* [RET        -> \e]        ; else end *)
        and Ret () = if isTLCurly() orelse isTLParen() orelse isTBLit()
                           orelse isTNLit() orelse isTCLit() orelse isTId()
                     then SOME (Exp())
                     else
                         if isTSemi() orelse isTElse() orelse isTEnd()
                         then NONE
                         else parseError "expected expression, else, end, ';'"

        (* [ERest      -> , Exp ERest]                  , *)
        (* [ERest      -> \e]                           ] } ) *)
        and ERest () = case !next of
                           TComma p
                           => (eatAny(); let val e1 = Exp()
                                             val es = ERest()
                                         in ( e1 :: es )
                                         end)
                         | other
                           => if isTRBracket() orelse isTRCurly()
                                 orelse isTRParen()
                              then []
                          else parseError "expected ',' or closing parenthesis"

        (* [Idx        -> [ Exp ERest ]]     [ *)
        (* [Idx        -> \e]       :=   * + = & then do ; else   end , ] } ) *)
        and Idx () = case !next of
                         TLBracket p
                         =>  let val e1 = ( eatAny(); Exp() )
                                 val es = ERest()
                             in (eat (TRBracket nopos); ( e1 :: es ))
                             end
                       | TAssign _ => []
                       | TTimes _  => []
                       | TSlash _  => []
                       | TPlus  _  => []
                       | TMinus _  => []
                       | TEq  _    => []
                       | TLess _   => []
                       | TAnd _    => []
                       | TThen _   => []
                       | TDo _     => []
                       | TSemi _   => []
                       | TElse _   => []
                       | TEnd  _   => []
                       | TComma _  => []
                       | TRBracket _ => []
                       | TRCurly _ => []
                       | TRParen _ => []
                       | other
                         => parseError
                            "expected: := ) * + = & do then ; else end , ] }"

        (* [IfRest     -> else Block]                   else *)
        (* [IfRest     -> \e]                           ; end else *)
        (* Here is a hack (dangling else) prefer the "else" branch *)
        and IfRest () = case !next of
                            TElse _ => (eatAny(); Block())
                          | TSemi _ => AbSyn.Block ([],[]) (* empty block *)
                          | TEnd _  => AbSyn.Block ([],[])
                          | other
                            => parseError "expected ';', end, or else"

        (* [IRest      -> ( Params )]               ( *)
        (* [IRest      -> Idx := Exp]               [ := *)
        and IRest () = case !next of
                           TLParen _
                           => let val ps = (eatAny(); Params() )
                              in (eat (TRParen nopos); IRCall ps)
                              end
                         | TLBracket _
                           => let val is = Idx()
                                  val e  = (eat (TAssign nopos); Exp() )
                              in IRAssign (is,e)
                              end
                         | TAssign _ (* cutting short... we know Idx => \e *)
                           => (eatAny(); IRAssign ([], Exp()) )
                         | other => parseError "expected '(','[' or ':='"

        (* [Exp        -> { Exp ERest }]                { *)
        (* [Exp        -> LogLit]                       LogLit *)
        (* [Exp        -> AExp ER]                      NumLit CharLit ( ID *)
        and Exp () = case !next of
                         TLCurly p => let val e1 = (eatAny(); Exp() )
                                          val es = ERest()
                                      in (eat (TRCurly nopos);
                                          ArrLit (e1::es, p) )
                                      end
                       | TBLit (b,p) => (eatAny();Literal (BVal (Log b),p))
                       | other => if isTNLit() orelse isTCLit() orelse isTSLit()
                                     orelse isTLParen() orelse isTId()
                                  then let val a  = AExp()
                                           val rs = ER()
                                       in applyLeft a rs
                                       end
                                  else
                                   parseError
                                    "expected literal, identifier, '(' or '{'"

        (* all operations are left-associative... problem with "=" and "&", see below *)
        and applyLeft a  []                  = a
          | applyLeft a ((con,a',p) :: rest) = applyLeft (con (a,a',p)) rest

        (* [ER         -> = AExp ER]            = *)
        (* [ER         -> & AExp ER]            & *)
        (* [ER         -> \e]                  then do ; else end , ] } ) *)
        and ER () = case !next of
                        TEq  p => let val a'  = (eatAny(); AExp() )
                                        val rs  = ER()
                                    in (AbSyn.Equal, a', p) :: rs
                                    end
                      | TLess  p => let val a'  = (eatAny(); AExp() )
                                        val rs  = ER()
                                    in (AbSyn.Less, a', p) :: rs
                                    end
                      (* This part is problematic, as "=" and "&" get equal precedence *)
                      | TAnd p => let val a'  = (eatAny(); AExp() )
                                        val rs  = ER()
                                    in (AbSyn.And, a', p) :: rs
                                    end
                      | TDo _ => []
                      | TThen _ => []
                      | TElse _ => []
                      | TEnd _  => []
                      | TSemi _ => []
                      | TComma _ => []
                      | TRBracket _ => []
                      | TRCurly _ => []
                      | TRParen _ => []
                      | other => parseError "unexpected token in expression"

        (* [AExp       -> Term AR]                      NumLit CharLit ( ID *)
        and AExp () = if isTNLit() orelse isTCLit() orelse isTSLit()
                         orelse isTLParen() orelse isTId()
                      then let val t  = Term()
                               val rs = AR()
                           in applyLeft t rs
                           end
                      else parseError "expected literal, '(', or identifier"

        (* [AR         -> + Term AR]         + *)
        (* [AR         -> \e]                = & then do ; else end , ] } ) *)
        and AR () = case !next of
                        TPlus p => let val a'  = (eatAny(); Term() )
                                       val rs  = AR()
                                    in (AbSyn.Plus, a', p) :: rs
                                    end
                      | TMinus p => let val a'  = (eatAny(); Term() )
                                       val rs  = AR()
                                    in (AbSyn.Minus, a', p) :: rs
                                    end
                      | TEq  _  => []
                      | TLess _ => []
                      | TAnd _  => []
                      | TDo _   => []
                      | TThen _ => []
                      | TElse _ => []
                      | TEnd _  => []
                      | TSemi _  => []
                      | TComma _ => []
                      | TRBracket _ => []
                      | TRCurly _   => []
                      | TRParen _   => []
                      | other => parseError "unexpected token in expression"

        (* [Term       -> Fac TR]                       NumLit CharLit ( ID *)
        and Term () = if isTNLit() orelse isTCLit() orelse isTSLit()
                         orelse isTLParen() orelse isTId()
                      then let val t  = Fac()
                               val rs = TR()
                           in applyLeft t rs
                           end
                      else parseError "expected literal, '(', or identifier"

        (* [TR         -> * Fac TR]   * *)
        (* [TR         -> \e]         + = & then do ; else end , ] } ) *)
        and TR () = case !next of
                        TTimes p => let val a'  = (eatAny(); Fac() )
                                        val rs  = TR()
                                    in (AbSyn.Times, a', p) :: rs
                                    end
                      | TSlash p => let val a'  = (eatAny(); Fac() )
                                        val rs  = TR()
                                    in (AbSyn.Div, a', p) :: rs
                                    end
                      | TPlus _  => []
                      | TMinus _  => []
                      | TEq   _  => []
                      | TLess _  => []
                      | TAnd _   => []
                      | TDo _   => []
                      | TThen _ => []
                      | TElse _ => []
                      | TEnd _  => []
                      | TSemi _  => []
                      | TComma _ => []
                      | TRBracket _ => []
                      | TRCurly _   => []
                      | TRParen _   => []
                      | other => parseError "unexpected token in expression"

        (* [Fac        -> NumLit]                       NumLit *)
        (* [Fac        -> CharLit]                      CharLit *)
        (* [Fac        -> ( Exp )]                      ( *)
        (* [Fac        -> ID IRest2]                    ID *)
        and Fac () = case !next of
                         TNLit (i,p) => (eatAny();Literal (BVal (Num i), p))
                       | TCLit (c,p) => (eatAny();Literal (BVal (Chr c), p))
                       | TSLit (s,p) => (eatAny();StrLit (s, p))
                       | TLParen _ => let val e = ( eatAny();Exp() )
                                      in (eat (TRParen nopos); e )
                                      end
                       | TId (n,p) => (eatAny(); case IRest2() of
                                           IRCall es       => FunApp (n, es, p)
                                           (* Hack, reusing IR type *)
                                         | IRAssign ([],_) => LValue (Var n,p)
                                         | IRAssign (es,_)
                                           => LValue (Index (n, es), p)
                                       )
                       | other
                         => parseError "expected literal, identifier or '('"

        (* [IRest2     -> ( Params )]  ( *)
        (* [IRest2     -> Idx]         [ * + = & then do ; else end , ] } ) *)
        and IRest2 () = case !next of
                            TLParen _ => let val ps = (eatAny(); Params() )
                                         in (eat (TRParen nopos); IRCall ps)
                                         end
                          | other
                            => if isTLBracket() orelse
                                  isTTimes() orelse isTSlash() orelse
                                  isTPlus() orelse isTMinus() orelse
                                  isTEq() orelse isTLess() orelse isTAnd()
                                  orelse isTThen() orelse isTDo() orelse
                                  isTElse() orelse isTEnd() orelse isTSemi()
                                  orelse isTComma() orelse isTRBracket()
                                  orelse isTRCurly() orelse isTRParen()
                               then IRAssign (Idx(), StrLit ("unused",nopos))
                                                     (* HACK see above *)
                               else
                                parseError "unexpected token after identifier"

        (* [Params -> Exp ERest]     { LogLit NumLit CharLit ( ID *)
        (* [Params -> \e]            ) *)
        and Params () = if isTLCurly() orelse isTLParen() orelse isTBLit()
                           orelse isTNLit() orelse isTCLit() orelse isTSLit()
                           orelse isTId()
                        then let val e1 = Exp()
                                 val es = ERest()
                             in e1 :: es
                             end
                        else
                            if isTRParen()
                            then []
                        else
                          parseError "expected literal, identifier, '{' or ')'"

    in Prog()
    end

end (* of struct *)
