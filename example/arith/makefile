arith: arith.mlb Lexer.sml Parser.sml arith.sml
	mlton -const 'Exn.keepHistory true' arith.mlb

Parser.sml: parser.nipo
	../../src/nipo $< > $@

Lexer.sml: lexer.nipo
	../../src/nipo $< > $@

.PHONY: run
run: arith
	./arith

.PHONY: clean
clean:
	rm -f Lexer.sml Parser.sml arith

