#lang racket
(require "experiments.rkt" gregor)

(module+ main

  ;; -------------------------------------------------------------------------------------------------

  ;; PARAMETERS

  (define repetitions (make-parameter 5))

  (define timeout (make-parameter "30m"))

  (define path/results (make-parameter "results"))

  (define path/ddpa/cases (make-parameter "cases/odefa"))
  (define path/ddpa/analysis (make-parameter "../toploop_main.native"))

  (define path/p4f/cases (make-parameter "../odefa/benchmark/cases/scheme"))
  (define path/p4f/analysis (make-parameter "../../p4f/"))

  (command-line
   #:once-each
   ["--repetitions"
    a-repetitions
    ((~a "How many times to run the benchmark (default: “" (repetitions) "”)"))
    (repetitions (or (string->number a-repetitions)
                     (raise-user-error (~a "Given ‘repetitions’ not a number: " a-repetitions))))]
   ["--timeout"
    a-timeout
    ((~a "Timeout (as understood by “timeout(1)”) (default: “" (timeout) "”)"))
    (timeout a-timeout)]
   ["--path-results"
    path
    ((~a "Path in which to store results (default: “" (path/results) "”)"))
    (path/results path)]
   ["--path-ddpa-cases"
    path
    ((~a "Path to DDPA test cases (default: “" (path/ddpa/cases) "”)"))
    (path/ddpa/cases path)]
   ["--path-ddpa-analysis"
    path
    ((~a "Path to DDPA program (default: “" (path/ddpa/analysis) "”)"))
    (path/ddpa/analysis path)]
   ["--path-p4f-cases"
    path
    ((~a "Path to P4F test cases (default: “" (path/p4f/cases) "”)"))
    (path/p4f/cases path)]
   ["--path-p4f-analysis"
    path
    ((~a "Path to P4F’s working directory (default: “" (path/p4f/analysis) "”)"))
    (path/p4f/analysis path)])

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

  (when (member 'p4f analyses)
    (unless (directory-exists? (path/p4f/cases))
      (raise-user-error
       (~a "Cannot find path to P4F test cases at “" (path/p4f/cases) "”.")))
    (unless (directory-exists? (path/p4f/analysis))
      (raise-user-error
       (~a "Cannot find path to P4F’s analysis at “" (path/p4f/analysis) "”."))))

  ;; -------------------------------------------------------------------------------------------------

  ;; MAIN

  (make-directory* (path/results))

  (for ([repetition (in-range (repetitions))])
    (displayln (~a "Repetition " repetition " of " (repetitions)))
    (for ([experiment experiments])
      (match-define (Experiment name/experiment tests) experiment)
      (for ([test tests])
        (match-define (Test name/test cases) test)
        (for ([a-case cases])
          (match-define (Case source (Subject analysis k) result) a-case)
          (define path/result
            (~a (path/results) "/"
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
               (define path/source (~a (path/ddpa/cases) "/" source ".odefa"))
               (values
                path/source
                (~a "/usr/bin/time -v "
                    "timeout " (timeout) " "
                    (path/ddpa/analysis) " "
                    "--select-context-stack=" k "ddpa "
                    "--analyze-variables=all --report-sizes --disable-evaluation "
                    "--disable-inconsistency-check "
                    "< " path/source " "
                    ">> " path/result " "
                    "2>&1"))]
              ['p4f
               (define path/source (~a (path/p4f/cases) "/" source ".scm"))
               (define path/statistics/directory (~a "statistics/" source))
               (define path/statistics/file (~a path/statistics/directory "/stat-" k "-pdcfa-gc.txt"))
               (values
                path/source
                (~a "cd " (path/p4f/analysis) " && "
                    "rm -rf '" path/statistics/directory "' && "
                    "/usr/bin/time -v "
                    "timeout " (timeout) " "
                    "sbt 'runMain org.ucombinator.cfa.RunCFA "
                    "--k " k " "
                    "--kalloc p4f --gc --dump-statistics --pdcfa "
                    path/source "' "
                    ">> " path/result " "
                    "2>&1 && "
                    "cat " path/statistics/file " "
                    ">> " path/result " "
                    "2>&1"))]))
          (cond
            [(file-exists? path/source)
             (display (~a command "..."))
             (flush-output)
             (with-output-to-file path/result (λ () (system "uptime") (displayln (~a "$ " command))))
             (system command)
             (displayln " done")
             (flush-output)]
            [else
             (displayln
              (~a "Cannot find test case at “" path/source "”.") (current-error-port))]))))))
