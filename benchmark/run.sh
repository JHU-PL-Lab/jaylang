#!/usr/bin/env bash -x

declare -A TESTS=(
  [ack]=1
  [blur]=1
  [church]=1
  [cpstak]=1
  [deriv]=1
  [eta]=1
  [facehugger]=1
  [flatten]=1
  [kcfa-2]=5
  [kcfa-3]=7
  [loop2]=1
  [map]=1
  [mj09]=1
  [primtest]=1
  [regex]=1
  [rsa]=1
  [sat-1]=4
  [sat-2]=4
  [sat-3]=4
  [state]=1
  [tak]=1
)
TRIALS=5
TIMEOUT=30m
HERE=$(cd $(dirname $0) && pwd)
DDPA=$HERE/../toploop_main.native
SCHEME_TO_ODEFA=$HERE/../scheme-front-end/scheme-to-odefa.rkt
P4F=$HERE/../../p4f

( cd $HERE && git rev-parse HEAD)
ocaml -version
opam --version
racket --version
(cd $P4F && git rev-parse HEAD)
java -version
sbt sbtVersion
scala -version

mkdir $HERE/results

for TRIAL in $(seq 1 $TRIALS)
do
  for TEST in "${!TESTS[@]}"
  do
    SOURCE=$HERE/cases/$TEST.scm

    EXPERIMENT=baseline
    K=0

    ANALYSIS=ddpa
    RESULT=$HERE/results/$(date --iso-8601=seconds)--experiment=$EXPERIMENT--analysis=$ANALYSIS--test=$TEST--k=$K.txt
    cat $SOURCE | racket $SCHEME_TO_ODEFA | /usr/bin/timeout $TIMEOUT /usr/bin/time -v $DDPA --select-context-stack=${K}ddpa --analyze-variables=all --report-sizes --disable-evaluation --disable-inconsistency-check &>> $RESULT

    ANALYSIS=p4f
    RESULT=$HERE/results/$(date --iso-8601=seconds)--experiment=$EXPERIMENT--analysis=$ANALYSIS--test=$TEST--k=$K.txt
    (cd $P4F && rm -rf statistics/ && /usr/bin/timeout $TIMEOUT /usr/bin/time -v sbt "runMain org.ucombinator.cfa.RunCFA --k $K --kalloc p4f --gc --dump-statistics --pdcfa $SOURCE" &>> $RESULT && cat statistics/$TEST/stat-$K-pdcfa-gc.txt &>> RESULT)

    EXPERIMENT=polyvariance

    ANALYSIS=ddpa
    K=${TESTS[$TEST]}
    RESULT=$HERE/results/$(date --iso-8601=seconds)--experiment=$EXPERIMENT--analysis=$ANALYSIS--test=$TEST--k=$K.txt
    cat $SOURCE | racket $SCHEME_TO_ODEFA | /usr/bin/timeout $TIMEOUT /usr/bin/time -v $DDPA --select-context-stack=${K}ddpa --analyze-variables=all --report-sizes --disable-evaluation --disable-inconsistency-check &>> $RESULT

    ANALYSIS=p4f
    K=1
    RESULT=$HERE/results/$(date --iso-8601=seconds)--experiment=$EXPERIMENT--analysis=$ANALYSIS--test=$TEST--k=$K.txt
    (cd $P4F && rm -rf statistics/ && /usr/bin/timeout $TIMEOUT /usr/bin/time -v sbt "runMain org.ucombinator.cfa.RunCFA --k $K --kalloc p4f --gc --dump-statistics --pdcfa $SOURCE" &>> $RESULT && cat statistics/$TEST/stat-$K-pdcfa-gc.txt &>> RESULT)
  done
done
