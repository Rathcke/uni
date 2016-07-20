{
  (* Lexer definition for Fasto language *)

  (* boilerplate code for all lexer files... *)
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun resetPos () = (currentLine :=1; lineStartPos := [0])

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)
 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 and showPos (l,c) = makestring l ^ "," ^ makestring c

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

(* This one is language specific, yet very common. Alternative would 
   be to encode every keyword as a regexp. This one is much easier. *)
 fun keyword (s, pos) =
     case s of
        "program"       => LL1Parser.TProgram   pos
       | "function"     => LL1Parser.TFunction  pos
       | "procedure"    => LL1Parser.TProcedure pos
       | "var"          => LL1Parser.TVar       pos
       | "begin"        => LL1Parser.TBegin     pos
       | "end"          => LL1Parser.TEnd       pos
       | "if"           => LL1Parser.TIf     pos
       | "then"         => LL1Parser.TThen   pos
       | "else"         => LL1Parser.TElse   pos
       | "while"        => LL1Parser.TWhile  pos
       | "do"           => LL1Parser.TDo     pos
       | "return"       => LL1Parser.TReturn pos
       | "array"        => LL1Parser.TArray  pos
       | "of"           => LL1Parser.TOf     pos
       | "int"          => LL1Parser.TInt    pos
       | "bool"         => LL1Parser.TBool   pos
       | "char"         => LL1Parser.TChar   pos

       | "true"         => LL1Parser.TBLit (true, pos)
       | "false"        => LL1Parser.TBLit (false, pos)

       | _              => LL1Parser.TId (s, pos)

   (* "lex" will later be the generated function "Token" *)
   fun repeat lex b 
              = let val res = lex b
                in case res of 
                         LL1Parser.TEOF _ => [] 
                       | other => other :: repeat lex b
                end

   fun Scan lex s = let val buf = createLexerString s
                    in repeat lex buf
                    end
        handle LexicalError (msg,pos) 
           => (TextIO.output (TextIO.stdErr, msg ^ showPos pos ^"\n");[])

}

rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
  | "//" [^`\n`]*	{ Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)

  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => LL1Parser.TNLit (i, getPos lexbuf) }

  | `'` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`]) `'`
                        { LL1Parser.TCLit
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad char constant"
			     | SOME s => String.sub(s,1)),
			     getPos lexbuf) }
  | `"` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`])* `"`
                        { LL1Parser.TSLit
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad string constant"
			     | SOME s => String.substring(s,1,
							  String.size s - 2)),
			     getPos lexbuf) }

  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }

  | ":="                { LL1Parser.TAssign   (getPos lexbuf) }
  | `+`                 { LL1Parser.TPlus     (getPos lexbuf) }
  | `-`                 { LL1Parser.TMinus    (getPos lexbuf) }
  | `*`                 { LL1Parser.TTimes    (getPos lexbuf) }
  | `/`                 { LL1Parser.TSlash    (getPos lexbuf) }
  | `=`                 { LL1Parser.TEq       (getPos lexbuf) }
  | `<`                 { LL1Parser.TLess     (getPos lexbuf) }
  | `&`                 { LL1Parser.TAnd      (getPos lexbuf) }

  | `(`                 { LL1Parser.TLParen   (getPos lexbuf) }
  | `)`                 { LL1Parser.TRParen   (getPos lexbuf) }
  | `[`                 { LL1Parser.TLBracket (getPos lexbuf) }
  | `]`                 { LL1Parser.TRBracket (getPos lexbuf) }
  | `{`                 { LL1Parser.TLCurly   (getPos lexbuf) }
  | `}`                 { LL1Parser.TRCurly   (getPos lexbuf) }

  | `,`                 { LL1Parser.TComma    (getPos lexbuf) }
  | `;`                 { LL1Parser.TSemi     (getPos lexbuf) }
  | `:`                 { LL1Parser.TColon    (getPos lexbuf) }

  | eof                 { LL1Parser.TEOF      (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
