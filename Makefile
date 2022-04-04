.PHONY: all clean repl test benchmark dbmc logclean dtest

all: dbmc

dbmc:
	dune build src/dbmc-top/dbmc_top.exe
	ln -s -f _build/default/src/dbmc-top/dbmc_top.exe dbmc_top

dtest:
	dune build test/dbmc/test_dbmc.exe 
	ln -s -f _build/default/test/dbmc/test_dbmc.exe dtest
	./dtest

ddpa:
	dune build src/ddpa-toploop/toploop-main/ddpa_toploop.exe
	ln -s -f _build/default/src/ddpa-toploop/toploop-main/ddpa_toploop.exe ddpa_toploop

translator:
	dune build src/translator-main/translator.exe
	ln -s -f _build/default/src/translator-main/translator.exe translator

clean:
	dune clean
	rm -f ddpa_toploop
	rm -f translator
	rm -f test_dbmc
	rm -f dbmc_top

logclean:
	rm -f dot/*
	rm -f logs/*

test-z3:
	dune exec test/sudu/test_sudu_z3.exe -- --verbose

test-rstack:
	dune runtest test/dbmc/inline-expect
	dune promote

# profiling

land100:
	OCAML_LANDMARKS=on,output="profiling/callgraph100.ansi" time ./dbmc_top -t target test-sources/loop/_sum100.odefa

land200:
	OCAML_LANDMARKS=on,output="profiling/callgraph200.ansi" time ./dbmc_top -t target -s 200 test-sources/loop/_sum200.odefa

land500:
	OCAML_LANDMARKS=on,output="profiling/callgraph500.ansi" time ./dbmc_top -t target -s 200 test-sources/loop/_sum500.odefa

benchmark:
	dune exec benchmark-test-generation/benchmark.exe

# extra

one:
	dune exec src/dbmc-top/analysis_top.exe -- test-sources/_syntax/one.odefa

# old targets
test:
	dune build test/unittest/test.exe
	_build/default/test/unittest/test.exe

repl:
	dune utop src -- -require pdr-programming

echo:
	dune exec src/langdk/examples/echo_repl.exe