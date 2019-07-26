arith: arith.mlb Lexer.sml arith.sml
	mlton -const 'Exn.keepHistory true' arith.mlb

Lexer.sml: lexer.nipo
	../../src/nipo $< > $@

.PHONY: run
run: arith
	./arith

.PHONY: clean
clean:
	rm -f Lexer.sml arith

