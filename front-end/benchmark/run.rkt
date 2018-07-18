#lang racket
(require "suite.rkt" gregor)

(module+ main
  (define timeout "30m")

  (define path/benchmark/results
    "/home/lfacchi2/demand-driven-program-analysis--scheme/benchmark/results")

  (define path/ddpa/cases
    "/home/lfacchi2/demand-driven-program-analysis--scheme/benchmark/cases/odefa")
  (define path/ddpa/analysis "/home/lfacchi2/odefa/core_toploop_main.native")

  (define path/p4f/cases
    "/home/lfacchi2/demand-driven-program-analysis--scheme/benchmark/cases/scheme")
  (define path/p4f/sbt "/home/lfacchi2/sbt/bin/sbt")
  (define path/p4f/working-directory "/home/lfacchi2/p4f-prototype")

  (make-directory* path/benchmark/results)

  (for ([test suite])
    (match-define (Test name test-cases) test)
    (for ([(analysis test-case) (in-dict test-cases)])
      (match-define (TestCase k source) test-case)
      (define path/benchmark/result
        (~a path/benchmark/results "/"
            (moment->iso8601 (now/moment)) "--analysis=" analysis "--name=" name "--k=" k ".txt"))
      (define command
        (case analysis
          [(ddpa)
           (~a "time -p "
               "timeout " timeout " "
               path/ddpa/analysis " "
               "--select-context-stack=" k "ddpa "
               "--analyze-variables=all --report-sizes --disable-evaluation "
               "--disable-inconsistency-check "
               "< " path/ddpa/cases "/" source ".odefa "
               ">> " path/benchmark/result " "
               "2>&1")]
          [(p4f)
           (~a "cd " path/p4f/working-directory " && "
               "time -p "
               path/p4f/sbt " 'run-main org.ucombinator.cfa.RunCFA "
               "--k " k " "
               "--kalloc p4f --gc --dump-statistics --pdcfa "
               path/p4f/cases "/" source ".scm' "
               ">> " path/benchmark/result " "
               "2>&1 && "
               "cat statistics/" source "/stat-" k "-pdcfa-gc.txt "
               ">> " path/benchmark/result " "
               "2>&1")]
          [else (raise-user-error "Unknown analysis: " test)]))
      (display (~a command "..."))
      (flush-output)
      (with-output-to-file path/benchmark/result (λ () (displayln (~a "$ " command))))
      (system command)
      (displayln " done")
      (flush-output))))