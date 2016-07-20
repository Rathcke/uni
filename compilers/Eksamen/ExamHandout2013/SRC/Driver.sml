(* Compiler Driver *)

(* This program is intended to be compiled to ../BIN/Paladim
 
   The compiler can be run in one of three ways. Assume that the
   current working directory is SRC/ for the following commands:

   $ ../BIN/Paladim -ti ../DATA/program.pal    runs the typed interpreter
   $ ../BIN/Paladim -c  ../DATA/program.pal    produces program.asm for Mars

   # If no parameter is specified, it defaults to compilation.  *)

structure Driver =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n )

  fun errorMsg s = TextIO.output (TextIO.stdErr,s ^ "\n")

  fun errorMsgAt s (line, column) = errorMsg
    (s ^ "\nLine " ^ makestring line ^ ", column " ^ makestring column ^ ".")

  fun typeCheck       pgm =
        let val () = () (* print "\n\nBefore Type Checking\n\n" *)
        in  Type.typeCheckPgm pgm
        end

  fun typedInterpret  pgm =
    let val () = () (* print "\n\nAfter Type Checking\n\n" *)
        val tp_absyn_str = TpAbSyn.prettyPrint pgm
        val () = print ("Program is:\n\n" ^ tp_absyn_str ^ "\n\nInput/Output:\n")
        val _  = TpInterpret.execPgm pgm
    in  ()
    end

  fun applyAll fs x = foldr (fn (f, y) => f y) x fs

  fun compileNormal pgm outpath =
    let val code = Compiler.compile (Type.typeCheckPgm pgm)
        val outfile = TextIO.openOut outpath
    in
      TextIO.output (outfile, Mips.pp_mips_list code);
      TextIO.closeOut outfile
    end

  fun compile arg path =
    let
      val inpath = path
      val outpath= Path.base path ^ ".asm"
      val lexbuf = createLexerStream (BasicIO.open_in inpath)
    in
      let
        (* val pgm = LL1Parser.parse Lexer.Token lexbuf *)
        (* COMMENT LINE ABOVE AND UNCOMMENT  *)
        (* THE LINE BELOW TO USE YOUR PARSER *)
        val pgm = Parser.Prog Lexer.Token lexbuf
      in case arg of
        "-ti" => typedInterpret (typeCheck pgm)
      | "-c"  => compileNormal pgm outpath
      | other => print ("'" ^ other ^ "': Unknown mode of operation.\n")
      end
      handle
        Parsing.yyexit ob => errorMsg "Parser-exit\n"
      | Parsing.ParseError ob =>
          errorMsgAt "Parsing error" (Lexer.getPos lexbuf)

      | LL1Parser.Error s =>
         errorMsgAt ("Parse error: " ^ s) (Lexer.getPos lexbuf)

      | Lexer.LexicalError (mess, pos) =>
          errorMsgAt ("Lexing error: "  ^ mess) pos

      | TpInterpret.Error (mess, pos) =>
          errorMsgAt ("Typed Interpreter error: " ^ mess) pos

      | Type.Error (mess, pos) =>
          errorMsgAt ("Type Checking error: " ^ mess) pos

      | SymTab.Duplicate (mess) =>
          errorMsg ("Symbol table error: " ^ mess)

      | Compiler.Error (mess, pos) =>
          errorMsgAt ("Compilation error: " ^ mess) pos

      | SysErr (s,_) => errorMsg ("Exception: " ^ s)
    end

  fun printHelp ()
    = print (
      "Usage: Paladim [option] program.pal\n\n"
      ^ "Options:\n"
      ^ "  -c    Compile program.pal to program.asm (default action)\n"
      ^ "  -ti   Interpret program.pal\n"
      )
    
  val _ =
    let
      val argv = Mosml.argv()
    in
        if List.exists (fn x => x = "-h" orelse x = "--help") argv
        then printHelp ()
        else case argv of
                 [_, arg, path] => compile arg path
               | [_, path] => compile "-c" path
               | _ => printHelp ()
    end
end
