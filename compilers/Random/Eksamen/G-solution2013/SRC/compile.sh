#!/bin/sh
#
# Compile the compiler.

set -e # Die on first error.
set -v # Output commands as they are executed.
# enters the SRC directory
cd $(dirname "$0")

# builds the Symbol Table
mosmlc -c SymTab.sml

# generates the SML sources for the lexer  
mosmllex Lexer.lex

# generates the SML sources for the parser if the .grm file is newer
# than the .sml file
if test "Parser.grm" -nt "Parser.sml"; then
    mosmlyac -v Parser.grm
fi

# builds abstract syntax tree (data type) modules
mosmlc -c AbSyn.sml
mosmlc -c TpAbSyn.sml

# builds the type checker
mosmlc -c Type.sig Type.sml

# builds the parser (-liberal to avoid a "compliance warning")
mosmlc -liberal -c Parser.sig Parser.sml

# builds the LL1 versions of the Parser
mosmlc -c LL1Parser.sml

# builds the Lexer
mosmlc -c Lexer.sml

# builds the Interpreter
mosmlc -c TpInterpret.sig TpInterpret.sml

# builds MIPS
mosmlc -c Mips.sml

# build Register Allocator
mosmlc -c RegAlloc.sig RegAlloc.sml

# builds the Compiler
mosmlc -c Compiler.sml

# builds the compiler binary Paladim, and puts it in the BIN folder
# (creates it if it does not exist)
mkdir -p ../BIN
mosmlc -o ../BIN/Paladim Driver.sml

######################################################################
## To run a program in Mars (MIPS simulator) (run in the root directory):
## $ ./BIN/Paladim DATA/program.pal
## $ ./SRC/mars.sh DATA/program.asm
######################################################################
## To remove the compiled code (run in the root directory):
## $ ./SRC/clean.sh
######################################################################
