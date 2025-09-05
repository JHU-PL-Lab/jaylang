.PHONY: all always clean translator jil logclean cbenchmark repl sctest 

BUILD = _build/default
BUILD_SRC = _build/default/src
BUILD_BIN = _build/default/src/bin
BUILD_TEST = _build/default/src-test
BENCH_C = benchmark/concolic

dune-build: always
	dune build

docker-build: always
	docker build -t jaylang:latest .

all: ceval interp bjy-cloc ft

# executables

ceval:
	dune build src/bin/ceval.exe

interp:
	dune build src/bin/interp.exe

bjy-cloc:
	dune build src/bin/bjy_cloc.exe

ft:
	dune build src/tables/test_features/tagger.exe
	
# clean up

clean:
	dune clean

logclean:
	rm -f dot/*
	rm -f logs/*

# testing

# run the fast concolic tests (the ill-typed programs)
test-fast:
	dune exec -- src-test/concolic/test_concolic.exe -q

# run the slow concolic tests (all programs, where well-typed run a long time)
test-all:
	dune exec -- src-test/concolic/test_concolic.exe

# run the interpreter on all test files
test-interp:
	dune exec -- src-test/interp/test_interp.exe

# run the deferred interpreter on dedicated deferred tests (both fast and slow)
test-deferred:
	dune exec -- src-test/deferred/test_deferred.exe

test-cdeval:
	dune exec -- src-test/deferred-concolic/test_deferred_concolic.exe -q

test-cdeval-all:
	dune exec -- src-test/deferred-concolic/test_deferred_concolic.exe

test-tools:
	dune exec -- src-test/tools/test_tools.exe

sandbox:
	dune build src-sandbox/sandbox/sandbox.exe

# benchmark

cbenchmark:
	dune exec --profile=release $(BENCH_C)/cbenchmark.exe -- $(ARGS)

cdbenchmark:
	dune exec --profile=release $(BENCH_C)/cdbenchmark.exe -- $(ARGS)
