.PHONY: all clean repl test

all:
	dune build

sandbox:
	dune build test/sandbox.exe

repl:
	dune utop src -- -require pdr-programming

test:
	dune runtest -f

clean:
	dune clean
