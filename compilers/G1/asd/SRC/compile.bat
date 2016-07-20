



:: builds the Symbol Table
mosmlc -c SymTab.sml

:: generates the SML sources for the lexer
mosmllex Lexer.lex

:: generates the SML sources for the parser
:: this is only done if the grm file is newer than the sml file
FOR /F %%i IN ('DIR /B /O:D Parser.grm Parser.sml') DO SET NEWER=%%i
IF "%NEWER%"=="Parser.grm" mosmlyac -v Parser.grm
:: file date comparison as suggested on http://stackoverflow.com/questions/1687014/how-do-i-compare-timestamps-of-files-in-a-dos-batch-script

:: builds abstract syntax tree (data type) modules
mosmlc -c AbSyn.sml
mosmlc -c TpAbSyn.sml

:: builds the type checker
mosmlc -c Type.sig Type.sml

:: builds the parser (-liberal to avoid a "compliance warning")
mosmlc -liberal -c Parser.sig Parser.sml

:: builds the LL1 versions of the Parser
mosmlc -c LL1Parser.sml

:: builds the Lexer
mosmlc -c Lexer.sml

:: builds MIPS
mosmlc -c Mips.sml

:: builds the Interpreter
mosmlc -c TpInterpret.sig TpInterpret.sml

:: builds MIPS
mosmlc -c Mips.sml

:: build Register Allocator
mosmlc -c RegAlloc.sig RegAlloc.sml

:: builds the Compiler
mosmlc -c Compiler.sml

:: builds the compiler binary Paladim, and puts it
:: in the bin folder (creates it if it does not exist)

if not exist ..\BIN mkdir ..\BIN

mosmlc -o ..\BIN\Paladim.exe Driver.sml

:: clean up
:: del Compiler.u*
:: del Driver.u*
:: del Interpret.u*
:: del Mips.u*
:: del RegAlloc.u*
:: del SymTab.u*
:: del Type.u*
type compile.bat
:::::::::::::::::::::::::::::::::::::::::::::::::::::
:: to run a program in Mars (MIPS simulator)
:: cd ..
:: BIN\Paladim DATA\program.pal
:: java -jar Mars4_4.jar DATA\program.asm
:::::::::::::::::::::::::::::::::::::::::::::::::::::
