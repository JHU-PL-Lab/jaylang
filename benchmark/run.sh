#!/usr/bin/env bash -x

declare -A CASES=(
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
CASES_PATH=$HERE/cases
RESULTS_PATH=$HERE/results
DDPA=$HERE/..
DDPA_TOPLOOP=$DDPA/toploop_main.native
SCHEME_TO_ODEFA=$DDPA/scheme-front-end/scheme-to-odefa.rkt
P4F=$DDPA/../p4f
P4F_CLASSPATH=$P4F/target/scala-2.11/classes
P4F_STATISTICS=$P4F/statistics
function result {
  RESULT=$RESULTS_PATH/experiment=$EXPERIMENT--case=$CASE--analysis=$ANALYSIS--k=$K--$(date --iso-8601=seconds).txt
}

(cd $DDPA && git rev-parse HEAD)
ocaml -version
opam --version
racket --version
(cd $P4F && git rev-parse HEAD)
java -version
sbt sbtVersion
scala -version

function ddpa {
  ANALYSIS=ddpa
  result
  cat $SOURCE | racket $SCHEME_TO_ODEFA | /usr/bin/time -v /usr/bin/timeout --foreground $TIMEOUT $DDPA_TOPLOOP --select-context-stack=${K}ddpa --analyze-variables=all --report-sizes --disable-evaluation --disable-inconsistency-check &>> $RESULT
}

function p4f {
  ANALYSIS=p4f
  result
  rm -rf $P4F_STATISTICS
  (cd $PF4 && /usr/bin/time -v /usr/bin/timeout --foreground $TIMEOUT scala -cp $P4F_CLASSPATH org.ucombinator.cfa.RunCFA --k $K --kalloc p4f --gc --dump-statistics --pdcfa $SOURCE &>> $RESULT)
  if [[ $? ]]
  then
    cat $P4F_STATISTICS/$CASE/stat-$K-pdcfa-gc.txt &>> $RESULT
  else
    pkill java
  fi
}

mkdir $RESULTS_PATH

for TRIAL in $(seq 1 $TRIALS)
do
  for CASE in "${!CASES[@]}"
  do
    SOURCE=$CASES_PATH/$CASE.scm
    EXPERIMENT=baseline
    K=0
    ddpa
    p4f
    EXPERIMENT=polyvariance
    K=${CASES[$CASE]}
    ddpa
    K=1
    p4f
  done
done
