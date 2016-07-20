{
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

}

rule Token = parse
    [` ` `\t` `\n` `\r`]
                               { Token lexbuf }

  | `a``b`* | `a`*`b` |(`a``b`)*`c`
							   {String (getLexeme lexbuf) }

  | eof                        { EOF }
  | _ { lexerError lexbuf ("Lexical error at input `"^getLexeme lexbuf^ "`") }

;