#lang racket
(require racket/runtime-path "experiments.rkt")

(define-runtime-path path/benchmark "./")

(define path/scheme-to-odefa (~a path/benchmark "../scheme-front-end/scheme-to-odefa.rkt"))
(define path/odefa/cases (~a path/benchmark "cases/odefa"))
(define path/scheme/cases (~a path/benchmark "cases/scheme"))

(define sources
  (remove-duplicates
   (for*/list ([experiment (in-list experiments)]
               [test (in-list (Experiment-tests experiment))]
               [case (in-list (Test-cases test))]
               #:when (equal? (Subject-analysis (Case-subject case)) 'ddpa))
     (Case-source case))))

(module+ main

  (command-line)

  (void (system (~a "raco make " path/scheme-to-odefa)))

  (for ([source (in-list sources)])
    (define path/odefa/target (~a path/odefa/cases "/" source ".odefa"))
    (define path/scheme/source (~a path/scheme/cases "/" source ".scm"))
    (cond
      [(file-exists? path/scheme/source)
       (define command
         (~a "racket " path/scheme-to-odefa " < " path/scheme/source " > " path/odefa/target))
       (display (~a command "..."))
       (flush-output)
       (system command)
       (displayln " done")
       (flush-output)]
      [else
       (displayln (~a "Cannot find test case at ‘" path/scheme/source "’.") (current-error-port))])))
