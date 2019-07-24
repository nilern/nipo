.PHONY: all
all: nipo

nipo: main.mlb Lexeme.sml Grammar.sml TokenSet.sml Parsers.sml Lexers.sml NipoTokens.sml SelfLexer.sml SelfParser.sml main.sml
	mlton -const 'Exn.keepHistory true' -output nipo main.mlb

SelfParser.sml: BootstrapParser
	./BootstrapParser > SelfParser.sml

BootstrapParser: BootstrapParser.mlb Lexeme.sml Grammar.sml TokenSet.sml Parsers.sml BootstrapParser.sml
	mlton -const 'Exn.keepHistory true' BootstrapParser.mlb

SelfLexer.sml: BootstrapLexer
	./BootstrapLexer > SelfLexer.sml

BootstrapLexer: BootstrapLexer.mlb Lexeme.sml Grammar.sml TokenSet.sml Parsers.sml Lexers.sml BootstrapLexer.sml
	mlton -const 'Exn.keepHistory true' BootstrapLexer.mlb

.PHONY: clean
clean:
	rm -f BootstrapLexer SelfLexer.sml nipo

