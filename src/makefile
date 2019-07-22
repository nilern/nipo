.PHONY: all
all: nipo

nipo: main.mlb Input.sml Grammar.sml nipo.sml NipoLexers.sml
	mlton -output nipo main.mlb

SelfLexer.sml: BootstrapLexer
	./BootstrapLexer > SelfLexer.sml

BootstrapLexer: BootstrapLexer.mlb Input/input.sml Grammar.sml nipo.sml NipoLexers.sml BootstrapLexer.sml
	mlton BootstrapLexer.mlb

.PHONY: clean
clean:
	rm -f BootstrapLexer SelfLexer.sml nipo

