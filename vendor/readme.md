# icfp20-artifact

used in icfp20-ddse paper

# one paper on HOBMC

# rosette/benchmark

copy from sympro's virtualbox image

# smbc

# hopv

https://github.com/hopv/MoCHi
used by [Naoki Kobayashi](https://www-kb.is.s.u-tokyo.ac.jp/~koba/)'s papers.
See references of _10 Years of the Higher-Order Model Checking Project_
e.g. [Combining Higher-Order Model Checking with Refinement Type Inference](https://www-kb.is.s.u-tokyo.ac.jp/~ryosuke/papers/pepm2019.pdf)

see also:
- https://github.com/hopv/benchmarks
Functional program verification problems, as caml programs and as Horn clauses.
- https://github.com/hopv/muhfl
A full-HFLz solver

# soft-contract

https://github.com/philnguyen/soft-contract

Higher-order symbolic execution for contract verification and refutation 2016.
The paper unifies and expands
- Soft contract verification, ICFP 2014
- Relatively complete counterexamples for higherorder programs, PLDI 2015
subsumes
- Higher-order symbolic execution via contracts, OOPSLA 2012

Soft Contract Verification for Higher-order Stateful Programs, POPL 2018
https://github.com/philnguyen/soft-contract/tree/popl18-ae

Corpse Reviver: Sound and Efficient Gradual Typing via Contract Verification
https://github.com/camoy/corpse-reviver
We use the benchmark suite first developed by Takikawa et al. [2016] (Is Sound Gradual
Typing Dead?) and expanded by Greenman et al. [2019].6 Our evaluation pits SCV-CR against Typed Racket on 12 of the 20 programs in the
benchmark suite.

(not relevant)
https://github.com/philnguyen/termination
Size-change termination as a contract: dynamically and statically enforcing termination for higher-order programs, PLDI 2019




# preparing `smbc`

```console
opam switch create ./
opam pin 'https://github.com/c-cube/smbc.git#master'
opam install smbc
smbc --help

```

# preparing hopv's `mochi`

```console
(install rust at https://www.rust-lang.org/learn/get-started)
git clone https://github.com/hopv/hoice
mkdir hoice
cd hoice
cargo install --git https://github.com/hopv/hoice

```


```console
git clone https://github.com/hopv/horsat2
cd horsat2
make horsat2

(add this path to path)


```

```console
git clone https://github.com/hopv/MoCHi.git
opam switch create ./ 4.11.2
opam install z3 dune batteries ocamlfind ppx_deriving yojson camlp5 zarith apron menhir

sudo apt install libglpk-dev
./build
./src/mochi.exe -help
```

```console
git clone https://github.com/hopv/benchmarks
cd benchmarks
../MoCHi/src/mochi.exe caml/lia/mochi/ack.ml
```

Q: What is the difference between `lia` and `dorder_lia`
A: No answer yet.
Searching `dorder` in `hopv` suggests an APLOS'17 paper, but no one matches after checking.
I asked on it on GitHub.

The `vender/hopv/diff_lia_dorder_lia.diff` shows the `dorder` versions

1. The majority is the same.
2. `dorder` annotates a few more types.
3. `if` clause and `assert` clause always comes with a `else ()`.

# Dijstra Monad

# Higher-order test generation

from https://patricegodefroid.github.io/