arith: arith.sml arith.mlb ../../src/nipo.sml
	mlton -const 'Exn.keepHistory true' arith.mlb

.PHONY: run
run: arith
	./arith

.PHONY: clean
clean:
	rm -f arith

