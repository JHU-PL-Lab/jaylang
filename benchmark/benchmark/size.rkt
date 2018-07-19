#lang racket
(require "experiments.rkt")

(module+ main

  ;; -------------------------------------------------------------------------------------------------

  ;; PARAMETERS

  (define path/odefa/cases (make-parameter "benchmark/cases/odefa"))

  (command-line
   #:once-each
   ["--path-odefa-cases"
    path
    ((~a "Path to DDPA test cases (default: “" (path/odefa/cases) "”)"))
    (path/odefa/cases path)])

  (unless (directory-exists? (path/odefa/cases))
    (raise-user-error
     (~a "Cannot find path to DDPA test cases at “" (path/odefa/cases) "”.")))

  ;; -------------------------------------------------------------------------------------------------

  ;; MAIN

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

  (define (count substring subject)
    (length (regexp-match* substring subject)))

  (define (program-point-count source)
    (- (count " = " source)
       (+ (count "{" source)
          (count "," source))))

  (for ([source (in-list sources)])
    (define path/odefa/target (~a (path/odefa/cases) "/" source ".odefa"))
    (cond
      [(file-exists? path/odefa/target)
       (define source (file->string path/odefa/target))
       (define size (program-point-count source))
       (displayln (~a path/odefa/target "," size))
       (flush-output)]
      [else
       (displayln (~a "Cannot find test case at “" path/odefa/target "”.") (current-error-port))])))