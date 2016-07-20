local open Obj Lexing in


   open Lexing

   exception LexicalError of string * int

   fun lexerError lexbuf s =
       raise LexicalError (s, getLexemeStart lexbuf);

   datatype TokenType
	 = String of string
     | EOF

   fun repeat lex b = let val res = lex b
                      in res :: (if res = EOF then [] else repeat lex b)
                      end

   fun Scan lex s = let val buf = createLexerString s
                    in repeat lex buf
                    end
        handle LexicalError (msg,pos)
           => (TextIO.output (TextIO.stdErr, msg ^ makestring pos ^"\n");[])


fun action_3 lexbuf = (
 lexerError lexbuf ("Lexical error at input `"^getLexeme lexbuf^ "`") )
and action_2 lexbuf = (
 EOF )
and action_1 lexbuf = (
String (getLexeme lexbuf) )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\n" => action_0 lexbuf
 |  #"\t" => action_0 lexbuf
 |  #"\r" => action_0 lexbuf
 |  #" " => action_0 lexbuf
 |  #"c" => action_1 lexbuf
 |  #"b" => action_1 lexbuf
 |  #"a" => state_4 lexbuf
 |  #"\^@" => action_2 lexbuf
 |  _ => action_3 lexbuf
 end)
and state_4 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_7 lexbuf
 |  #"a" => state_6 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_6 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => action_1 lexbuf
 |  #"a" => state_6 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_7 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => action_1 lexbuf
 |  #"b" => state_9 lexbuf
 |  #"a" => state_8 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_8 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_11 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_9 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"b" => state_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_11 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"c" => action_1 lexbuf
 |  #"a" => state_8 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_3, action_2, action_1, action_0];

end
