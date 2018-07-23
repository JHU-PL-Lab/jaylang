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
DDPA=$HERE/..
DDPA_TOPLOOP=$DDPA/toploop_main.native
SCHEME_TO_ODEFA=$DDPA/../scheme-front-end/scheme-to-odefa.rkt
P4F=$DDPA/../../p4f
P4F_CLASSPATH=$P4F/target/scala-2.11/classes
P4F_STATISTICS=$P4F/statistics

(cd $DDPA && git rev-parse HEAD)
ocaml -version
opam --version
racket --version
(cd $P4F && git rev-parse HEAD)
java -version
sbt sbtVersion
scala -version

mkdir $HERE/results

function result {
  RESULT=$HERE/results/$(date --iso-8601=seconds)--experiment=$EXPERIMENT--analysis=$ANALYSIS--test=$TEST--k=$K.txt
}

function ddpa {
  ANALYSIS=ddpa
  result
  cat $SOURCE | racket $SCHEME_TO_ODEFA | /usr/bin/time -v /usr/bin/timeout --foreground $TIMEOUT $DDPA_TOPLOOP --select-context-stack=${K}ddpa --analyze-variables=all --report-sizes --disable-evaluation --disable-inconsistency-check &>> $RESULT
}

function p4f {
  ANALYSIS=p4f
  result
  rm -rf $P4F_STATISTICS
  /usr/bin/time -v /usr/bin/timeout --foreground $TIMEOUT scala -cp $P4F_CLASSPATH org.ucombinator.cfa.RunCFA --k $K --kalloc p4f --gc --dump-statistics --pdcfa $SOURCE &>> $RESULT
  if $?
  then
    cat $P4F_STATISTICS/$TEST/stat-$K-pdcfa-gc.txt &>> RESULT
  else
    pkill java
  fi
}

for TRIAL in $(seq 1 $TRIALS)
do
  for TEST in "${!TESTS[@]}"
  do
    SOURCE=$HERE/cases/$TEST.scm
    EXPERIMENT=baseline
    K=0
    ddpa
    p4f
    EXPERIMENT=polyvariance
    K=${TESTS[$TEST]}
    ddpa
    K=1
    p4f
  done
done
