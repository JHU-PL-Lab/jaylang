.PHONY: all clean dbmc dj sato translator jil logclean benchmark \
				repl test dbmc-test sato-test dtest dtest-ddse dtest-all 

BUILD = _build/default
BUILD_SRC = _build/default/src
BUILD_BIN = _build/default/src/bin
BUILD_TEST = _build/default/src-test
TEST_D = test/dbmc


all: dbmc sato translator

# executables

dj:
	dune build src/bin/dj.exe

dbmc: dj dbmc-test

sato:
	dune build src/bin/sato.exe

translator:
	dune build src/bin/translator.exe

jil:
	dune build src/bin/jil.exe


# clean up

clean:
	dune clean
	rm -f ddpa_toploop
	rm -f translator
	rm -f test_dbmc
	rm -f dj

logclean:
	rm -f dot/*
	rm -f logs/*


# testing

dbmc-test: 
	dune build src-test/dbmc/test_dbmc.exe 
	ln -s -f $(BUILD_TEST)/dbmc/test_dbmc.exe dtest

sato-test:
	dune build src-test/sato/test_sato.exe
	ln -s -f $(BUILD_TEST)/sato/test_sato.exe stest

stest: sato-test
	./stest

dtest: dbmc-test
	./dtest 

dtest-ins:dbmc-test
	./dtest --ta

dtest-ddse: dbmc-test
	./dtest --te ddse


test-z3:
	dune exec src-test/sudu/test_sudu_z3.exe -- --verbose

test-z3-heavy:
	dune exec src-test/sudu/test_heavy.exe 

test-rstack:
	dune runtest src-test/dbmc/inline-expect
	dune promote


# profiling

profile:
	mkdir -p profiling
	dune build --workspace dune-workspace.profile src/bin/dj.exe
# dune exec --workspace dune-workspace.profiling --context profiling src/bin/dj.exe -- -t target $(TEST_D)/loop/_sum100.jil

land100:
	OCAML_LANDMARKS=auto,output="profiling/callgraph100-ddse.ansi" time ./dj.exe -t target -ls2 debug $(TEST_D)/loop/_sum100.jil

land200:
	OCAML_LANDMARKS=on,output="profiling/callgraph200.ansi" time ./dj.exe -t target -ls2 debug $(TEST_D)/loop/_sum200.jil

land500:
	OCAML_LANDMARKS=auto,output="profiling/callgraph500.ansi" time ./dj.exe -t target -ls2 debug $(TEST_D)/loop/_sum500.jil

ll:
	OCAML_LANDMARKS=on,output="profiling/fold.ansi" time ./dj.exe -t target -e ddse -m 3 $(TEST_D)/benchmark/icfp20/_smbc/smbc_fold0s.natodefa

# benchmark

benchmark:
	dune exec benchmark/benchmark.exe -- -e dbmc
# dune exec benchmark/benchmark.exe -- -e ddse

benchmark-icfp-artifact:
	dune exec benchmark/benchmark.exe -- -e dbmc -f benchmark/icfp20-artifact.s
# dune exec benchmark/benchmark.exe -- -e ddse -f benchmark/icfp20-artifact.s

b1:
	dune exec benchmark/benchmark.exe -- -e dbmc -f benchmark/neo.s

b2:
	dune exec benchmark/benchmark.exe -- -e ddse -f benchmark/neo.s


# legacy

ddpa:
	dune build src/bin/ddpa_toploop.exe
	ln -s -f $(BUILD_BIN)/ddpa_toploop.exe ddpa_toploop

test:
	dune build src-test/unittest/test.exe
	$(BUILD_TEST)/unittest/test.exe

repl:
	dune utop src -- -require pdr-programming

echo:
	dune exec src/langdk/examples/echo_repl.exe