NIPO = ../../src/nipo

ArrLexer.sml: lexer.nipo
	$(NIPO) $< > $@

ArrParser.sml: parser.nipo
	$(NIPO) $< > $@

.PHONY clean:
clean:
	rm -rf ArrLexer.sml ArrParser.sml

