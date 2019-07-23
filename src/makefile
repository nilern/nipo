.PHONY: all
all: nipo

nipo: main.mlb Lexeme.sml Grammar.sml TokenSet.sml Parsers.sml Lexers.sml NipoTokens.sml SelfLexer.sml main.sml
	mlton -output nipo main.mlb

SelfLexer.sml: BootstrapLexer
	./BootstrapLexer > SelfLexer.sml

BootstrapLexer: BootstrapLexer.mlb Lexeme.sml Grammar.sml TokenSet.sml Parsers.sml Lexers.sml BootstrapLexer.sml
	mlton BootstrapLexer.mlb

.PHONY: clean
clean:
	rm -f BootstrapLexer SelfLexer.sml nipo

