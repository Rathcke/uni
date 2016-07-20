load "Parsing";
load "Lexing";
load "Nonstdio";
load "OS";

load "AbSyn";
load "Parser";
load "Lexer";
load "Type";
load "TpAbSyn";
load "Mips";
load "Compiler";

fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n);

fun errorMess s = (Lexer.resetPos();
                   TextIO.output (TextIO.stdErr,s ^ "\n");
                   [])

(* "lex" will later be the generated function "Token" *)
fun repeat lex b = let val res = lex b
                   in case res of
                          Parser.EOF _ => []
                        | other => other :: repeat lex b
                   end

fun tokenise f = let val buf = createLexerStream (BasicIO.open_in f)
                     val res = repeat Lexer.Token buf
                 in (Lexer.resetPos(); res)
                 end
    handle Lexer.LexicalError (msg,(line,col))
           => errorMess ("Lexical error " ^ msg ^ " at line " ^ makestring line
                         ^ ", column " ^ makestring col  ^ ".")
         | SysErr (action,err) => let val msg = case err of
                                                    SOME e => OS.errorMsg e
                                                  | NONE   => "NONE"
                                  in errorMess ("SysErr in "^action ^": " ^ msg)
                                  end

fun showsyntax f =
  let
    val lexbuf = createLexerStream (BasicIO.open_in f)
  in let val result = Parser.Prog Lexer.Token lexbuf
         (* more phases will go here later... *)
     in result
     end
     handle Parsing.yyexit x => errorMess "Parser exit"
          | Parsing.ParseError x =>
            let val (line,col) = Lexer.getPos lexbuf
            in errorMess ("Parse error at line " ^ makestring line
                          ^ ", column " ^ makestring col  ^ ".")
            end
          | Lexer.LexicalError (msg,(line,col))
            => errorMess ("Lexical error " ^ msg ^ " at line " ^ makestring line
                          ^ ", column " ^ makestring col  ^ ".")
  end
  handle SysErr (action,err) => let val msg = case err of
                                                  SOME e => OS.errorMsg e
                                                | NONE   => "NONE"
                                in errorMess ("SysErr in "^action ^": " ^ msg)
                                end

exception MyError of string

val pDefault = AbSyn.Block ([],[AbSyn.Return (NONE,(0,0))])
fun parseBlock s =
    let val dummyProg = "program parseBlock;\n" ^
                        "procedure main() return;\n" ^
                        "procedure parseMe() " ^ s
        val lexbuf  = Lexing.createLexerString dummyProg
    in let val ts = Parser.Prog Lexer.Token lexbuf
           fun extractBlock [] = raise MyError ("unexpected: block not found")
             | extractBlock (x::xs)
               = (case x of
                      AbSyn.Proc ("parseMe",[], block, _) => block
                    | other => extractBlock xs)
       in case ts of
              []   => raise MyError ("not understood: " ^ s)
            | list => extractBlock list
       end
    handle MyError msg => (errorMess msg; pDefault)
         | Parsing.yyexit x => (errorMess "Parser exit"; pDefault)
         | Parsing.ParseError x
           => let val (line,col) = Lexer.getPos lexbuf
              in (errorMess ("Parse error at line " ^ makestring line
                             ^ ", column " ^ makestring col  ^ "."); pDefault)
              end
         | Lexer.LexicalError (mess,(lin,col))
           => (errorMess ("Lexical error: " ^mess^ " at line "
                          ^ makestring lin ^ ", column "
                          ^ makestring col); pDefault)
    end

val ftab =[ ("ord",  ([TpAbSyn.BType TpAbSyn.Char], SOME (TpAbSyn.BType TpAbSyn.Int )))
          , ("chr",  ([TpAbSyn.BType TpAbSyn.Int ], SOME (TpAbSyn.BType TpAbSyn.Char)))
          , ("len",  ([TpAbSyn.Array(3,TpAbSyn.Int)], SOME (TpAbSyn.BType TpAbSyn.Int )))
          , ("read", ([], SOME (TpAbSyn.BType TpAbSyn.Int )))
          , ("write",([], NONE             ))
          , ("new",  ([], SOME (TpAbSyn.BType TpAbSyn.Int )))
          ]
val tcDefault   = TpAbSyn.Block ([],[])
fun tcBlock s =
    let val block     = parseBlock s
        val dummyProg
          = [AbSyn.Proc ("main",[],AbSyn.Block([],[AbSyn.Return(NONE,(1,42))]),(1,0)),
             AbSyn.Proc ("checkMe", [], block, (3,0))]
        fun extractBlock [] = raise MyError ("unexpected: block not found")
          | extractBlock (x::xs)
               = (case x of
                      TpAbSyn.Proc ("checkMe",[], block, _) => block
                    | other => extractBlock xs)
    in (Type.functionTable := ftab;
        extractBlock(Type.typeCheckPgm dummyProg))
    end
    handle MyError msg => (errorMess msg; tcDefault)
         | Type.Error (msg,(l,c))
           => (errorMess ("Type error: " ^ msg ^ " at line " ^ makestring l ^
                          ", column " ^ makestring c); tcDefault)

val cDefault = [ Mips.COMMENT "compilation error" ]
fun compileBlock s =
    let val block   = tcBlock s
    in Compiler.compileStmts block [] "__EXITING__"
    end
    handle Compiler.Error (msg,(l,c))
           => (errorMess ("Compilation error: " ^ msg ^ " at line " ^
                          makestring l ^ ", column " ^ makestring c); cDefault)
