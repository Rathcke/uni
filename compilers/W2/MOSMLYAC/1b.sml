local
in
datatype token =
    CharA of  int 
  | CharB of  int 
  | CharC of  int 
  | EOF of  int 
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

  
   (* See the Moscow ML manual for the syntax and structure. Roughly:

    `%``{`
         header (* with ML-like comments *)
    `%``}`
        declarations  /* with c-like comments */
    `%``%`
        rules         /* with c-like comments */
    `%``%`
        trailer (* with ML-like comments *)
    EOF

    The (optional) header and trailer contain ML code to include in the
    generated file (after the data declaration and at the end).

    Compiling this file with:
        $ mosmlyac -v G_AB.grm
    will generate the code in "G_AB.sig" and "G_AB.sml" files, and a
    file "G_AB.output" which describes the generated LR(0) automaton.
    *)

    type pos = int (* position in string *)
    (* Unfortunately, we cannot use this type in the declarations -
       the code ends up _after_ the data declaration. *)

    (* parse exception *)
    exception ParseErr of string * pos
(* Line 34, file 1b.sml *)
val yytransl = #[
  257 (* CharA *),
  258 (* CharB *),
  259 (* CharC *),
  260 (* EOF *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\\000\000";

val yylen = "\002\000\
\\002\000\003\000\003\000\003\000\001\000\001\000\001\000\000\000\
\\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\\000\000\000\000\001\000\002\000\003\000\004\000";

val yydgoto = "\002\000\
\\006\000\007\000";

val yysindex = "\002\000\
\\255\254\000\000\255\254\255\254\255\254\000\000\003\255\007\255\
\\008\255\006\255\000\000\000\000\000\000\000\000";

val yyrindex = "\000\000\
\\009\255\000\000\010\255\011\255\012\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\001\000";

val YYTABLESIZE = 16;
val yytable = "\003\000\
\\004\000\005\000\001\000\008\000\009\000\010\000\011\000\012\000\
\\014\000\013\000\000\000\000\000\008\000\005\000\006\000\007\000";

val yycheck = "\001\001\
\\002\001\003\001\001\000\003\000\004\000\005\000\004\001\001\001\
\\003\001\002\001\255\255\255\255\004\001\004\001\004\001\004\001";

val yyact = vector_ 10 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file 1b.grm, line 51 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : int
val d__2__ = peekVal 0 :  int 
in
( print "reduce 1\n";((d__1__))   ) end : int))
;
(* Rule 2, file 1b.grm, line 53 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 :  int 
val d__2__ = peekVal 1 : int
val d__3__ = peekVal 0 :  int 
in
( print "reduce 2\n";2+(d__2__)   ) end : int))
;
(* Rule 3, file 1b.grm, line 54 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 2 :  int 
val d__2__ = peekVal 1 : int
val d__3__ = peekVal 0 :  int 
in
( print "reduce 3\n";2+(d__2__)   ) end : int))
;
(* Rule 4, file 1b.grm, line 55 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 2 :  int 
val d__2__ = peekVal 1 : int
val d__3__ = peekVal 0 :  int 
in
( print "reduce 4\n";2+(d__2__)   ) end : int))
;
(* Rule 5, file 1b.grm, line 56 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 0 :  int 
in
( print "reduce 5\n";1      ) end : int))
;
(* Rule 6, file 1b.grm, line 57 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 :  int 
in
( print "reduce 6\n";1      ) end : int))
;
(* Rule 7, file 1b.grm, line 58 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 :  int 
in
( print "reduce 7\n";1      ) end : int))
;
(* Rule 8, file 1b.grm, line 59 *)
val _ = update_ yyact 8
(fn () => repr(let
in
( print "reduce 8\n";0      ) end : int))
;
(* Entry Start *)
val _ = update_ yyact 9 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Start lexer lexbuf = yyparse yytables 1 lexer lexbuf;
(* Line 62, file 1b.grm *)

(* SML trailer

 At this point we can use the parse function (%start above), whose type is
   Start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Exp;

  (Lexing.lexbuf -> token) is usually mosmllex-generated, but a simple hack
  here.

*)

  (* hacking a fake "lexer" that just reads characters *)
  fun token (#"a",pos) = (CharA pos)
    | token (#"b",pos) = (CharB pos)
    | token (#"c",pos) = (CharC pos)
    | token (  d ,pos) = raise ParseErr ("No token " ^ makestring d, pos)

  val lexhackTokens = ref []
  fun lexhackInit s 
    = let val cs = String.explode s
          val ps = List.tabulate (String.size s, fn x=>x)
      in lexhackTokens := ListPair.map token (cs, ps) @ [EOF (String.size s)]
      end

  fun lexhackNexttoken _ 
    = let val ts = !lexhackTokens
      in case ts of
             []        => (EOF 0)
           | (t::rest) => (lexhackTokens := rest; t)
      end

  fun parse s = let val fakebuf = Lexing.createLexerString s (* not used *)
                in lexhackInit s; Start lexhackNexttoken fakebuf
                end
                 handle ParseErr (msg,pos) 
                        => (print (msg ^ makestring pos); (0))
(* Line 185, file 1b.sml *)
