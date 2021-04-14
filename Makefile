.PHONY: all clean repl sandbox test benchmark dbmc logclean

all:
	dune build
	dune build src/toploop-main/ddpa_toploop.exe
	dune build src/test-generation-main/test_generator.exe
	dune build src/translator-main/translator.exe
	rm -f ddpa_toploop
	rm -f translator
	rm -f test_generator
	ln -s _build/default/src/toploop-main/ddpa_toploop.exe ddpa_toploop
	ln -s _build/default/src/test-generation-main/test_generator.exe test_generator
	ln -s _build/default/src/translator-main/translator.exe translator

dbmc:
	dune build src/test-generation-main/test_generator.exe
	rm -f test_generator
	ln -s _build/default/src/test-generation-main/test_generator.exe test_generator

dtest:
	dune exec test/dbmc/test_dbmc.exe

sandbox:
	dune build test/sandbox/sandbox.exe
	rm -f sandbox
	ln -s _build/default/test/sandbox/sandbox.exe sandbox

repl:
	dune utop src -- -require pdr-programming

test:
	dune build test/unittest/test.exe
	_build/default/test/unittest/test.exe

clean:
	dune clean
	rm -f ddpa_toploop
	rm -f translator
	rm -f sandbox

logclean:
	rm -f dot/*
	rm -f logs/*

benchmark:
	dune exec benchmark-test-generation/benchmark.exe
