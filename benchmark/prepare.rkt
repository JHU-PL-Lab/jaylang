#lang racket
(require "experiments.rkt")

(module+ main

  ;; -------------------------------------------------------------------------------------------------

  ;; PARAMETERS

  (define path/scheme-to-odefa (make-parameter "scheme-to-odefa.rkt"))
  (define path/odefa/cases (make-parameter "benchmark/cases/odefa"))
  (define path/scheme/cases (make-parameter "benchmark/cases/scheme"))

  (command-line
   #:once-each
   ["--path-scheme-to-odefa"
    path
    ((~a "Path to “scheme-to-odefa.rkt” program (default: “" (path/scheme-to-odefa) "”)"))
    (path/scheme-to-odefa path)]
   ["--path-odefa-cases"
    path
    ((~a "Path to DDPA test cases (default: “" (path/odefa/cases) "”)"))
    (path/odefa/cases path)]
   ["--path-scheme-cases"
    path
    ((~a "Path to Scheme test cases (default: “" (path/scheme/cases) "”)"))
    (path/scheme/cases path)])

  (unless (file-exists? (path/scheme-to-odefa))
    (raise-user-error
     (~a "Cannot find “scheme-to-odefa.rkt” program at “" (path/scheme-to-odefa) "”.")))
  (unless (directory-exists? (path/odefa/cases))
    (raise-user-error
     (~a "Cannot find path to DDPA test cases at “" (path/odefa/cases) "”.")))
  (unless (directory-exists? (path/scheme/cases))
    (raise-user-error
     (~a "Cannot find path to Scheme test cases at “" (path/scheme/cases) "”.")))

  ;; -------------------------------------------------------------------------------------------------

  ;; MAIN

  (void (system (~a "raco make " (path/scheme-to-odefa))))

  (define sources
    (filter
     values
     (remove-duplicates
      (flatten
       (for/list ([experiment (in-list experiments)])
         (match-define (Experiment name/experiment tests) experiment)
         (for/list ([test (in-list tests)])
           (match-define (Test name/test cases) test)
           (for/list ([a-case (in-list cases)])
             (match-define (Case source (Subject analysis k) result) a-case)
             (case analysis
               [(ddpa little-store) source]
               [(p4f) #f]
               [else
                (raise-user-error
                 (~a "Unknown analysis;\n"
                     " Case: " a-case "\n"
                     " Test: " test "\n"
                     " Experiment: " experiment))]))))))))

  (for ([source (in-list sources)])
    (define path/odefa/target (~a (path/odefa/cases) "/" source ".odefa"))
    (define path/scheme/source (~a (path/scheme/cases) "/" source ".scm"))
    (cond
      [(file-exists? path/scheme/source)
       (define command
         (~a "racket " (path/scheme-to-odefa) " "
             "< " path/scheme/source " "
             "> " path/odefa/target))
       (display (~a command "..."))
       (flush-output)
       (system command)
       (displayln " done")
       (flush-output)]
      [else
       (displayln (~a "Cannot find test case at “" path/scheme/source "”.") (current-error-port))])))