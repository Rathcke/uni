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
         "program"      => Parser.PROG   pos
       | "function"     => Parser.FUN    pos
       | "procedure"    => Parser.PROC   pos
       | "var"          => Parser.VAR    pos
       | "begin"        => Parser.BEG    pos
       | "end"          => Parser.END    pos
       | "if"           => Parser.IF     pos
       | "then"         => Parser.THEN   pos
       | "else"         => Parser.ELSE   pos
       | "while"        => Parser.WHILE  pos
       | "do"           => Parser.DO     pos
       | "return"       => Parser.RETURN pos
       | "array"        => Parser.ARR    pos
       | "of"           => Parser.OF     pos
       | "int"          => Parser.INT    pos
       | "bool"         => Parser.BOOL   pos
       | "char"         => Parser.CHAR   pos
       | "and"          => Parser.AND    pos
       | "or"           => Parser.OR     pos
       | "not"          => Parser.NOT    pos
       | "true"         => Parser.TRUE   pos
       | "false"        => Parser.FALSE  pos

       | _              => Parser.ID (s, pos)

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
                             | SOME i => Parser.NUM (i, getPos lexbuf) }

  | `'` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`]) `'`
                        { Parser.CHARLIT
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad char constant"
			     | SOME s => String.sub(s,1)),
			     getPos lexbuf) }
  | `"` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`])* `"`
                        { Parser.STRINGLIT
			    ((case String.fromCString (getLexeme lexbuf) of
			       NONE => lexerError lexbuf "Bad string constant"
			     | SOME s => String.substring(s,1,
							  String.size s - 2)),
			     getPos lexbuf) }

  | [`a`-`z` `A`-`Z`] [`a`-`z` `A`-`Z` `0`-`9` `_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }

  | ":="                { Parser.ASSIGN   (getPos lexbuf) }
  | `+`                 { Parser.PLUS     (getPos lexbuf) }
  | `-`                 { Parser.MINUS    (getPos lexbuf) }
  | `*`                 { Parser.TIMES    (getPos lexbuf) }
  | `/`                 { Parser.DIV      (getPos lexbuf) }
  | `=`                 { Parser.EQ       (getPos lexbuf) }
  | `<`                 { Parser.LTH      (getPos lexbuf) }
  | `(`                 { Parser.LPAR     (getPos lexbuf) }
  | `)`                 { Parser.RPAR     (getPos lexbuf) }
  | `[`                 { Parser.LBRACKET (getPos lexbuf) }
  | `]`                 { Parser.RBRACKET (getPos lexbuf) }
  | `{`                 { Parser.LCURLY   (getPos lexbuf) }
  | `}`                 { Parser.RCURLY   (getPos lexbuf) }
  | `,`                 { Parser.COMMA    (getPos lexbuf) }
  | `;`                 { Parser.SEMICOL  (getPos lexbuf) }
  | `:`                 { Parser.COLON    (getPos lexbuf) }
  | eof                 { Parser.EOF      (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
