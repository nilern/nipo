aa: aa.sml aa.mlb ../../src/nipo.sml
	mlton -const 'Exn.keepHistory true' aa.mlb

.PHONY: run
run: aa
	./aa

.PHONY: clean
clean:
	rm -f aa

