#lang racket
(require racket/runtime-path "experiments.rkt" gregor)

(define repetitions (make-parameter 5))

(define timeout (make-parameter "30m"))

(define-runtime-path path/benchmark "./")

(define path/results (~a path/benchmark "results"))

(define path/ddpa/cases (~a path/benchmark "cases/odefa"))
(define path/ddpa/analysis (make-parameter (~a path/benchmark "../toploop_main.native")))

(define path/p4f/cases (~a path/benchmark "cases/scheme"))
(define path/p4f/analysis (make-parameter (~a path/benchmark "../../p4f/")))

(module+ main

  (command-line
   #:once-each
   ["--repetitions"
    a-repetitions
    ((~a "How many times to run the benchmark (default: ‘" (repetitions) "’)"))
    (repetitions (or (string->number a-repetitions)
                     (raise-user-error (~a "Given ‘repetitions’ not a number: " a-repetitions))))]
   ["--timeout"
    a-timeout
    ((~a "Timeout (as understood by ‘timeout(1)’) (default: ‘" (timeout) "’)"))
    (timeout a-timeout)]
   ["--ddpa"
    path
    ((~a "Path to DDPA program (default: ‘" (path/ddpa/analysis) "’)"))
    (path/ddpa/analysis path)]
   ["--p4f"
    path
    ((~a "Path to P4F’s analysis (default: ‘" (path/p4f/analysis) "’)"))
    (path/p4f/analysis path)])

  (make-directory* path/results)

  (for* ([repetition (in-range (repetitions))]
         #:when (displayln (~a "Repetition " repetition " of " (repetitions) " (non-inclusive)"))
         [experiment (in-list experiments)]
         [test (in-list (Experiment-tests experiment))]
         [case (in-list (Test-cases test))])
    (match-define (Experiment name/experiment tests) experiment)
    (match-define (Test name/test cases) test)
    (match-define (Case source (Subject analysis k) result) case)
    (define path/result
      (~a path/results "/"
          (moment->iso8601 (now/moment))
          "--repetition=" repetition
          "--experiment=" name/experiment
          "--analysis=" analysis
          "--test=" name/test
          "--k=" k
          ".txt"))
    (define-values (path/source command)
      (match analysis
        ['ddpa
         (define path/source (~a path/ddpa/cases "/" source ".odefa"))
         (values
          path/source
          (~a "timeout " (timeout) " "
              "/usr/bin/time -v "
              (path/ddpa/analysis) " "
              "--select-context-stack=" k "ddpa "
              "--analyze-variables=all --report-sizes --disable-evaluation "
              "--disable-inconsistency-check "
              "< " path/source " "
              ">> " path/result " "
              "2>&1"))]
        ['p4f
         (define path/source (~a path/p4f/cases "/" source ".scm"))
         (define path/statistics/directory (~a "statistics/" source))
         (define path/statistics/file (~a path/statistics/directory "/stat-" k "-pdcfa-gc.txt"))
         (values
          path/source
          (~a "cd " (path/p4f/analysis) " && "
              "rm -rf '" path/statistics/directory "' && "
              "timeout " (timeout) " "
              "/usr/bin/time -v "
              "sbt 'runMain org.ucombinator.cfa.RunCFA "
              "--k " k " "
              "--kalloc p4f --gc --dump-statistics --pdcfa "
              (simplify-path path/source) "' "
              ">> " path/result " "
              "2>&1 && "
              "cat " path/statistics/file " "
              ">> " path/result " "
              "2>&1"))]))
    (cond
      [(file-exists? path/source)
       (display (~a command "..."))
       (flush-output)
       (with-output-to-file path/result (thunk (system "uptime") (displayln (~a "$ " command))))
       (system command)
       (displayln " done")
       (flush-output)]
      [else (displayln (~a "Cannot find test case at ‘" path/source "’.") (current-error-port))])))
