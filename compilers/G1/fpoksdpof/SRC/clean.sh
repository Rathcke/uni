#!/bin/sh
#
# Removes compiled code.

set -e # Die on first error.
set -v # Output commands as they are executed.
# enters the SRC directory
cd $(dirname "$0")

# removes files
rm -f SymTab.u*
rm -f Lexer.sml Lexer.u*
rm -f Parser.u*
rm -f Absyn.u*
rm -f TpAbsyn.u*
rm -f Type.u*
rm -f LL1Lexer.u*
rm -f LL1Parser.u*
rm -f TpInterpret.u*
rm -f Mips.u*
rm -f RegAlloc.u*
rm -f Compiler.u*
rm -f Driver.u*
rm -f ../BIN/Paladim
rm -f *~
