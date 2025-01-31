.PHONY: all clean translator jil logclean cbenchmark repl sctest 

BUILD = _build/default
BUILD_SRC = _build/default/src
BUILD_BIN = _build/default/src/bin
BUILD_TEST = _build/default/src-test
BENCH_C = benchmark/concolic

docker-build:
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

test-concolic:
	dune exec src-test/concolic/test_concolic.exe

# benchmark

cbenchmark:
	dune exec --profile=release $(BENCH_C)/cbenchmark.exe
