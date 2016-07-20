{
  (* Lexer definition for Paladim language *)

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
        "program"       => Parser.TProgram   pos
       | "function"     => Parser.TFunction  pos
       | "procedure"    => Parser.TProcedure pos
       | "var"          => Parser.TVar       pos
       | "begin"        => Parser.TBegin     pos
       | "end"          => Parser.TEnd       pos
       | "if"           => Parser.TIf     pos
       | "then"         => Parser.TThen   pos
       | "else"         => Parser.TElse   pos
       | "while"        => Parser.TWhile  pos
       | "do"           => Parser.TDo     pos
       | "return"       => Parser.TReturn pos
       | "array"        => Parser.TArray  pos
       | "of"           => Parser.TOf     pos
       | "int"          => Parser.TInt    pos
       | "bool"         => Parser.TBool   pos
       | "char"         => Parser.TChar   pos
       | "and"          => Parser.TAnd    pos
       | "or"           => Parser.TOr     pos
       | "not"          => Parser.TNot    pos
       | "true"         => Parser.TBLit (true, pos)
       | "false"        => Parser.TBLit (false, pos)

       | _              => Parser.TId (s, pos)

   (* "lex" will later be the generated function "Token" *)
   fun repeat lex b
              = let val res = lex b
                in case res of
                         Parser.TEOF _ => []
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
                             | SOME i => Parser.TNLit (i, getPos lexbuf) }

  | `'` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`]) `'`
                        { Parser.TCLit
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad char constant"
			     | SOME s => String.sub(s,1)),
			     getPos lexbuf) }
  | `"` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`])* `"`
                        { Parser.TSLit
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad string constant"
			     | SOME s => String.substring(s,1,
							  String.size s - 2)),
			     getPos lexbuf) }

  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }

  | ":="                { Parser.TAssign   (getPos lexbuf) }
  | `+`                 { Parser.TPlus     (getPos lexbuf) }
  | `-`                 { Parser.TMinus    (getPos lexbuf) }
  | `*`                 { Parser.TTimes    (getPos lexbuf) }
  | `/`                 { Parser.TSlash    (getPos lexbuf) }
  | `=`                 { Parser.TEq       (getPos lexbuf) }
  | `<`                 { Parser.TLess     (getPos lexbuf) }
  | `(`                 { Parser.TLParen   (getPos lexbuf) }
  | `)`                 { Parser.TRParen   (getPos lexbuf) }
  | `[`                 { Parser.TLBracket (getPos lexbuf) }
  | `]`                 { Parser.TRBracket (getPos lexbuf) }
  | `{`                 { Parser.TLCurly   (getPos lexbuf) }
  | `}`                 { Parser.TRCurly   (getPos lexbuf) }

  | `,`                 { Parser.TComma    (getPos lexbuf) }
  | `;`                 { Parser.TSemi     (getPos lexbuf) }
  | `:`                 { Parser.TColon    (getPos lexbuf) }
  | eof                 { Parser.TEOF      (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
