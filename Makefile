.PHONY: all clean translator jil logclean cbenchmark repl sctest 

BUILD = _build/default
BUILD_SRC = _build/default/src
BUILD_BIN = _build/default/src/bin
BUILD_TEST = _build/default/src-test
BENCH_C = benchmark/concolic

docker-build:
	docker build -t jaylang:latest .


all: sc translator cj fuzzer

# executables

sc:
	dune build src/bin/sato_concolic.exe
	cp ./sato_concolic.exe ./sc.exe

translator:
	dune build src/bin/translator.exe

cj:
	dune build src/bin/cj.exe

fuzzer:
	dune build src/bin/fuzzer.exe

jil:
	dune build src/bin/jil.exe

jay:
	dune build src/bin/jay.exe

ft:
	dune build src-test/tables/test_features/tagger.exe
	
# clean up

clean:
	dune clean
	rm -f translator

logclean:
	rm -f dot/*
	rm -f logs/*


# testing

sc-test:
	dune build src-test/sato-concolic/test_sc.exe
	ln -s -f $(BUILD_TEST)/sato-concolic/test_sc.exe sctest

sctest: sc-test
	./sctest

test-concolic:
	dune exec src-test/concolic/test_concolic.exe

# benchmark

cbenchmark:
	dune exec --profile=release $(BENCH_C)/cbenchmark.exe

# legacy

repl:
	dune utop src -- -require pdr-programming

echo:
	dune exec src/langdk/examples/echo_repl.exe