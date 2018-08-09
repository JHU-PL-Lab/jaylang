#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset
set -o xtrace

echo "START"

TRIALS=10
TIMEOUT=2h
declare -A CASES=(
  [ack]=1
  [blur]=1
  # [church]=1 # Triggers P4F implementation bug. P4F throws exception if non-function appears as operator. Minimal working example: ‘((if (< 1 2) (lambda () 'anything) #t))’.
  [cpstak]=1
  [deriv]=1
  [eta]=1
  [facehugger]=1
  [flatten]=1
  [kcfa-2]=5
  [kcfa-3]=7
  [loop2-1]=1
  [map]=1
  [mj09]=1
  [primtest]=1
  [regex]=1
  [rsa]=1
  [sat-1]=4
  [sat-2]=14
  [sat-3]=14
  # [state]=1 # Boxes aren’t supported by P4F.
  [tak]=1
)

HERE="$(cd "$(dirname $0)" && pwd)"
CASES_PATH="${HERE}/cases"
RESULTS_PATH="${HERE}/results"
DDPA="${HERE}/.."
DDPA_TOPLOOP="${DDPA}/toploop_main.native"
SCHEME_TO_ODEFA="${DDPA}/scheme-front-end/scheme-to-odefa.rkt"
P4F="${DDPA}/../p4f"
P4F_CLASSPATH="${P4F}/target/scala-2.11/classes"
P4F_STATISTICS="${P4F}/statistics"
function result {
  RESULT="${RESULTS_PATH}/experiment=${EXPERIMENT}--case=${CASE}--analysis=${ANALYSIS}--k=${K}--$(date --iso-8601=seconds).txt"
  uptime &>> "${RESULT}"
}

lscpu
cat /proc/cpuinfo
free -m
cat /proc/meminfo
uname -a
lsb_release -a
(cd "${DDPA}" && git rev-parse HEAD)
ocaml -version
opam --version
racket --version
(cd "${P4F}" && git rev-parse HEAD)
java -version
sbt sbtVersion
scala -version

function ddpa {
  ANALYSIS=ddpa
  result
  cat "${SOURCE}" | racket "${SCHEME_TO_ODEFA}" | /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" "${DDPA_TOPLOOP}" --select-context-stack="${K}"ddpa --analyze-variables=all --report-sizes --report-source-statistics --disable-evaluation --disable-inconsistency-check &>> "${RESULT}" || true
}

function p4f {
  ANALYSIS=p4f
  result
  rm -rf "${P4F_STATISTICS}"
  if (cd "${P4F}" && /usr/bin/time -v /usr/bin/timeout --foreground "${TIMEOUT}" scala -J-Xmx7g -J-Xss256m -cp "${P4F_CLASSPATH}" org.ucombinator.cfa.RunCFA --kcfa --k "${K}" --kalloc p4f --dump-statistics "${SOURCE}" &>> "${RESULT}")
  then
    cat "${P4F_STATISTICS}/${CASE}/stat-${K}-p4f.txt" &>> "${RESULT}"
  else
    pkill sbt scala java || true
  fi
}

mkdir -p "${RESULTS_PATH}"
(cd "${DDPA}" && make)
raco make "${SCHEME_TO_ODEFA}"
(cd "${P4F}" && sbt compile)

for TRIAL in $(seq 1 "${TRIALS}")
do
  for CASE in "${!CASES[@]}"
  do
    SOURCE="${CASES_PATH}/${CASE}.scm"
    EXPERIMENT=monovariant
    K=0
    ddpa
    p4f
    EXPERIMENT=polyvariant
    K="${CASES[${CASE}]}"
    ddpa
    K=1
    p4f
  done
done

echo "END"
