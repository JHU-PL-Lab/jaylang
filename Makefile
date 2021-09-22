.PHONY: all clean repl sandbox test benchmark dbmc logclean dtest

all:
	dune build
	dune build src/toploop-main/ddpa_toploop.exe
	dune build src/test-generation-main/test_generator.exe src/test-generation-main/test_dbmc.exe
	dune build src/translator-main/translator.exe
	rm -f ddpa_toploop
	rm -f translator
	rm -f sandbox
	rm -f test_generator
	rm -f test_dbmc
	rm -f dbmc_top
	rm -f dtest
	ln -s _build/default/src/toploop-main/ddpa_toploop.exe ddpa_toploop
	ln -s _build/default/src/test-generation-main/test_generator.exe test_generator
	ln -s _build/default/src/translator-main/translator.exe translator
	ln -s _build/default/src/test-generation-main/test_dbmc.exe test_dbmc
	ln -s _build/default/src/dbmc-top/dbmc_top.exe dbmc_top

dbmc:
	dune build src/test-generation-main src/dbmc-top
	rm -f test_generator
	rm -f test_dbmc
	rm -f dbmc_top
	ln -s _build/default/src/test-generation-main/test_generator.exe test_generator
	ln -s _build/default/src/test-generation-main/test_dbmc.exe test_dbmc
	ln -s _build/default/src/dbmc-top/dbmc_top.exe dbmc_top

dtest:
	rm -f dtest
	dune build test/dbmc/test_dbmc.exe 
	ln -s _build/default/test/dbmc/test_dbmc.exe dtest

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
	rm -f test_dbmc
	rm -f test_generator
	rm -f dbmc_top

logclean:
	rm -f dot/*
	rm -f logs/*

benchmark:
	dune exec benchmark-test-generation/benchmark.exe

land3:
	OCAML_LANDMARKS=on,output="callgraph3.ansi" ./test_dbmc -t target test2/loop/sum3.odefa

land50:
	OCAML_LANDMARKS=on,output="callgraph50.ansi" ./test_dbmc -t target test2/loop/_sum50.odefa

land100:
	OCAML_LANDMARKS=on,output="callgraph100.ansi" ./test_dbmc -t target test2/loop/_sum100.odefa