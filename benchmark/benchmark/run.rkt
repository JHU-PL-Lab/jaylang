#lang racket
(require "experiments.rkt" gregor)

(module+ main

  ;; -------------------------------------------------------------------------------------------------

  ;; PARAMETERS

  (define timeout (make-parameter "30m"))

  (define path/benchmark/results (make-parameter "benchmark/results"))

  (define path/ddpa/cases (make-parameter "benchmark/cases/odefa"))
  (define path/ddpa/analysis (make-parameter "../ddpa/core_toploop_main.native"))

  (define path/little-store/cases (make-parameter "benchmark/cases/odefa"))
  (define path/little-store/analysis (make-parameter "../little-store/core_toploop_main.native"))

  (define path/p4f/cases (make-parameter "benchmark/cases/scheme"))
  (define path/p4f/sbt (make-parameter "../sbt/bin/sbt"))
  (define path/p4f/working-directory (make-parameter "../p4f-prototype"))

  (command-line
   #:once-each
   ["--timeout"
    a-timeout
    ((~a "Timeout (as understood by “timeout(1)”) (default: “" (timeout) "”)"))
    (timeout a-timeout)]
   ["--path-benchmark-results"
    path
    ((~a "Path in which to store benchmark results (default: “" (path/benchmark/results) "”)"))
    (path/benchmark/results path)]
   ["--path-ddpa-cases"
    path
    ((~a "Path to DDPA test cases (default: “" (path/ddpa/cases) "”)"))
    (path/ddpa/cases path)]
   ["--path-ddpa-analysis"
    path
    ((~a "Path to DDPA program (default: “" (path/ddpa/analysis) "”)"))
    (path/ddpa/analysis path)]
   ["--path-little-store-cases"
    path
    ((~a "Path to Little Store test cases (default: “" (path/little-store/cases) "”)"))
    (path/little-store/cases path)]
   ["--path-little-store-analysis"
    path
    ((~a "Path to Little Store program (default: “" (path/little-store/analysis) "”)"))
    (path/little-store/analysis path)]
   ["--path-p4f-cases"
    path
    ((~a "Path to P4F test cases (default: “" (path/p4f/cases) "”)"))
    (path/p4f/cases path)]
   ["--path-p4f-sbt"
    path
    ((~a "Path to P4F’s instance of SBT (default: “" (path/p4f/sbt) "”)"))
    (path/p4f/sbt path)]
   ["--path-p4f-working-directory"
    path
    ((~a "Path to P4F’s working directory (default: “" (path/p4f/working-directory) "”)"))
    (path/p4f/working-directory path)])

  (define analyses
    (remove-duplicates
     (flatten
      (for/list ([experiment (in-list experiments)])
        (match-define (Experiment name/experiment tests) experiment)
        (for/list ([test (in-list tests)])
          (match-define (Test name/test cases) test)
          (for/list ([a-case (in-list cases)])
            (match-define (Case source (Subject analysis k) result) a-case)
            analysis))))))

  (when (member 'ddpa analyses)
    (unless (directory-exists? (path/ddpa/cases))
      (raise-user-error
       (~a "Cannot find path to DDPA test cases at “" (path/ddpa/cases) "”.")))
    (unless (file-exists? (path/ddpa/analysis))
      (raise-user-error
       (~a "Cannot find path to DDPA analysis at “" (path/ddpa/analysis) "”."))))

  (when (member 'little-store analyses)
    (unless (directory-exists? (path/little-store/cases))
      (raise-user-error
       (~a "Cannot find path to Little Store test cases at “" (path/little-store/cases) "”.")))
    (unless (file-exists? (path/little-store/analysis))
      (raise-user-error
       (~a "Cannot find path to Little Store analysis at “" (path/little-store/analysis) "”."))))

  (when (member 'p4f analyses)
    (unless (directory-exists? (path/p4f/cases))
      (raise-user-error
       (~a "Cannot find path to P4F test cases at “" (path/p4f/cases) "”.")))
    (unless (file-exists? (path/p4f/sbt))
      (raise-user-error
       (~a "Cannot find path to P4F’s instance of SBT at “" (path/p4f/sbt) "”.")))
    (unless (directory-exists? (path/p4f/working-directory))
      (raise-user-error
       (~a "Cannot find path to P4F’s working directory at “" (path/p4f/working-directory) "”."))))

  ;; -------------------------------------------------------------------------------------------------

  ;; MAIN

  (make-directory* (path/benchmark/results))

  (for ([experiment experiments])
    (match-define (Experiment name/experiment tests) experiment)
    (for ([test tests])
      (match-define (Test name/test cases) test)
      (for ([a-case cases])
        (match-define (Case source (Subject analysis k) result) a-case)
        (define path/benchmark/result
          (~a (path/benchmark/results) "/"
              (moment->iso8601 (now/moment))
              "--experiment=" name/experiment
              "--analysis=" analysis
              "--test=" name/test
              "--k=" k
              ".txt"))
        (define-values (path/benchmark/source command)
          (case analysis
            [(ddpa)
             (define path/benchmark/source (~a (path/ddpa/cases) "/" source ".odefa"))
             (values
              path/benchmark/source
              (~a "time -p "
                  "timeout " (timeout) " "
                  (path/ddpa/analysis) " "
                  "--select-context-stack=" k "ddpa "
                  "--analyze-variables=all --report-sizes --disable-evaluation "
                  "--disable-inconsistency-check "
                  "< " path/benchmark/source " "
                  ">> " path/benchmark/result " "
                  "2>&1"))]
            [(little-store)
             (define path/benchmark/source (~a (path/little-store/cases) "/" source ".odefa"))
             (values
              path/benchmark/source
              (~a "time -p "
                  "timeout " (timeout) " "
                  (path/little-store/analysis) " "
                  "--stack-delta-size=" k " "
                  "--analyze-variables=all --report-sizes --disable-evaluation "
                  "--disable-inconsistency-check "
                  "< " path/benchmark/source " "
                  ">> " path/benchmark/result " "
                  "2>&1"))]
            [(p4f)
             (define path/benchmark/source (~a (path/p4f/cases) "/" source ".scm"))
             (define path/statistics/directory (~a "statistics/" source))
             (define path/statistics/file (~a path/statistics/directory "/stat-" k "-pdcfa-gc.txt"))
             (values
              path/benchmark/source
              (~a "cd " (path/p4f/working-directory) " && "
                  "rm -rf '" path/statistics/directory "' && "
                  "time -p "
                  (path/p4f/sbt) " 'run-main org.ucombinator.cfa.RunCFA "
                  "--k " k " "
                  "--kalloc p4f --gc --dump-statistics --pdcfa "
                  path/benchmark/source "' "
                  ">> " path/benchmark/result " "
                  "2>&1 && "
                  "cat " path/statistics/file " "
                  ">> " path/benchmark/result " "
                  "2>&1"))]
            [else
             (raise-user-error
              (~a "Unknown analysis;\n"
                  " Case: " a-case "\n"
                  " Test: " test "\n"
                  " Experiment: " experiment))]))
        (cond
          [(file-exists? path/benchmark/source)
           (display (~a command "..."))
           (flush-output)
           (with-output-to-file path/benchmark/result (λ () (displayln (~a "$ " command))))
           (system command)
           (displayln " done")
           (flush-output)]
          [else
           (displayln
            (~a "Cannot find test case at “" path/benchmark/source "”.") (current-error-port))])))))