.PHONY: all
all: SelfLexer.sml

SelfLexer.sml: BootstrapLexer
	./BootstrapLexer > SelfLexer.sml

BootstrapLexer: BootstrapLexer.mlb NipoLexers.sml BootstrapLexer.sml
	mlton BootstrapLexer.mlb

.PHONY: clean
clean:
	rm -f BootstrapLexer SelfLexer.sml

