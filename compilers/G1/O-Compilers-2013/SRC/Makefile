# Makefile for Compilers lecture code
# here: source code (directory SRC)x

BINDIR=../BIN

MLC=mosmlc
MLI=mosml
MLLEX=mosmllex
MLYACC=mosmlyac -v

SMLSRC=Driver.sml SymTab.sml TpInterpret.sml Type.sml AbSyn.sml TpAbSyn.sml LL1Parser.sml Lexer.sml Mips.sml RegAlloc.sml Compiler.sml
SIGSRC=TpInterpret.sig RegAlloc.sig Compiler.sig Type.sig
EXTRASRC=Lexer.lex Parser.grm TestSyntax.sml

UIS=$(patsubst %.sig,%.ui,$(SIGSRC)) Parser.ui Lexer.ui
UOS=$(patsubst %.sml,%.uo,$(SMLSRC)) Parser.uo Lexer.uo

# precious generated files: Parser.sml, Parser.sig, Parser.output,
# 			    Lexer.sml, and the binary
PRECIOUS=$(BINDIR)/Paladim Parser.sml Parser.sig Parser.output Lexer.sml

all	: parser tpinterpreter typecheck main

main	: Driver

Driver 	: Driver.uo ${UOS}
	mkdir -p $(BINDIR)
	$(MLC) Driver.uo -o $(BINDIR)/Paladim

parser :  Lexer.uo Lexer.ui Parser.ui Parser.uo

compiler : Compiler.uo

tpinterpreter : TpInterpret.uo

typecheck : Type.uo

# Parser
Parser.sml Parser.sig: Parser.grm
	$(MLYACC) $<

#Lexer
Lexer.sml : Lexer.lex
	$(MLLEX) $<

# suffix rule for foo.sml -> foo.uo
%.uo	: %.sml
	$(MLC) -c $<
# suffix rule for foo.sml -> foo.uo
%.ui	: %.sig
	$(MLC) -c $<
# special rule for Parser.ui (-liberal to avoid a compliance warning)
Parser.ui: Parser.sig AbSyn.uo
	$(MLC) -liberal -c $<

wipe	:
	rm -f $(UIS) $(UOS)

.PHONY	: clean
clean	:
	rm -f $(UIS) $(UOS)
	# ask back ("-i") for precious files
	rm -i $(PRECIOUS)

########### generated dependencies (camlrunm $MOSML/tools/mosmldep)
Type.uo: Type.ui AbSyn.uo SymTab.uo TpAbSyn.uo
Type.ui: AbSyn.uo TpAbSyn.uo
TpInterpret.uo: TpInterpret.ui SymTab.uo TpAbSyn.uo
TpInterpret.ui: TpAbSyn.uo
TpAbSyn.uo: AbSyn.uo
RegAlloc.uo: RegAlloc.ui Mips.uo
RegAlloc.ui: Mips.uo
Parser.uo: Parser.ui AbSyn.uo
Parser.ui: AbSyn.uo
LL1Parser.uo: AbSyn.uo
# edited. Parser added artificially
Lexer.uo: LL1Parser.uo Parser.ui
# edited. AbSyn.uo not detected, Parser added artificially
Driver.uo: SymTab.uo Type.ui Mips.uo Lexer.uo Compiler.uo TpInterpret.ui \
    LL1Parser.uo TpAbSyn.uo AbSyn.uo Parser.uo
Compiler.uo: Mips.uo SymTab.uo RegAlloc.ui TpAbSyn.uo
