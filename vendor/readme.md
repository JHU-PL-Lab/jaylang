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

https://github.com/philnguyen
/soft-contract
https://github.com/camoy/corpse-reviver

# preparing `sbmc`

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
cd benchmark
../MoCHi/src/mochi.exe caml/lia/mochi/ack.ml
```