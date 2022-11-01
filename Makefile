.PHONY: all clean dbmc dj sato translator jil logclean benchmark \
				repl test dbmc-test sato-test dtest dtest-ddse dtest-all 

BUILD = _build/default
BUILD_SRC = _build/default/src
BUILD_BIN = _build/default/src/bin
BUILD_TEST = _build/default/src-test


all: dbmc sato translator


# executables

dj:
	dune build src/bin/dj.exe
	ln -s -f $(BUILD_BIN)/dj.exe dj
	ln -s -f $(BUILD_BIN)/dj.exe dj

dbmc: dj dbmc-test

sato:
	dune build src/bin/sato.exe
	ln -s -f $(BUILD_BIN)/sato.exe sato

translator:
	dune build src/translator-main/translator.exe
	ln -s -f $(BUILD_SRC)/translator-main/translator.exe translator

jil:
	dune build src/bin/jil.exe
	ln -s -f $(BUILD_BIN)/jil.exe jil


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
	dune exec test/sudu/test_sudu_z3.exe -- --verbose

test-z3-heavy:
	dune exec test/sudu/test_heavy.exe 

test-rstack:
	dune runtest test/dbmc/inline-expect
	dune promote


# profiling

profile:
	dune build --workspace dune-workspace.profile src/bin/dj.exe
	ln -s -f _build/profile/src/bin/dj.exe dj
# dune exec --workspace dune-workspace.profiling --context profiling src/bin/dj.exe -- -t target test-sources/loop/_sum100.odefa

land100:
	OCAML_LANDMARKS=auto,output="profiling/callgraph100-ddse.ansi" time ./dj -t target -ls2 debug test-sources/loop/_sum100.odefa

land200:
	OCAML_LANDMARKS=on,output="profiling/callgraph200.ansi" time ./dj -t target -ls2 debug test-sources/loop/_sum200.odefa

land500:
	OCAML_LANDMARKS=auto,output="profiling/callgraph500.ansi" time ./dj -t target -ls2 debug test-sources/loop/_sum500.odefa

ll:
	OCAML_LANDMARKS=on,output="profiling/fold.ansi" time ./dj -t target -e ddse  -m 3 test-sources/benchmark/icfp20/_smbc/smbc_fold0s.natodefa


# benchmark

benchmark:
	dune exec benchmark/benchmark.exe -- -e dbmc
	dune exec benchmark/benchmark.exe -- -e ddse

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