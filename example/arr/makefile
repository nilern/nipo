arr: arr.sml arr.mlb ../../src/nipo.sml
	mlton -const 'Exn.keepHistory true' arr.mlb

.PHONY: run
run: arr
	./arr

.PHONY: clean
clean:
	rm -f arr

