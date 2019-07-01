.PHONY: all clean repl test

all:
	dune build
	dune build src/toploop-main/ddpa_toploop.exe
	dune build src/translator-main/translator.exe
	rm -f ddpa_toploop
	rm -f translator
	ln -s _build/default/src/toploop-main/ddpa_toploop.exe ddpa_toploop
	ln -s _build/default/src/translator-main/translator.exe translator

sandbox:
	dune build test/sandbox.exe

repl:
	dune utop src -- -require pdr-programming

test:
	dune runtest -f

clean:
	dune clean
	rm -f ddpa_toploop
	rm -f translator
