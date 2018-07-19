#lang racket
(require "experiments.rkt" file/glob plot gregor)

(module+ main

  ;; -------------------------------------------------------------------------------------------------

  ;; PARAMETERS

  (define path/benchmark/results (make-parameter "benchmark/results"))

  (define timeout (make-parameter 1800))
  (define test-case/width (make-parameter 50))

  (command-line
   #:once-each
   ["--timeout"
    a-timeout
    ((~a "Timeout (in seconds) (default: “" (timeout) "”)"))
    (timeout a-timeout)]
   ["--path-benchmark-results"
    path
    ((~a "Path in which to find stored benchmark results (default: “" (path/benchmark/results) "”)"))
    (path/benchmark/results path)]
   ["--width"
    width
    ((~a "Width of the columns in the plotted graph (default: “" (test-case/width) "”)"))
    (test-case/width width)])

  (unless (directory-exists? (path/benchmark/results))
    (raise-user-error
     (~a "Cannot find path to benchmark results at “" (path/benchmark/results) "”.")))

  ;; -------------------------------------------------------------------------------------------------

  ;; MAIN

  (define experiments/with-results
    (for/list ([experiment (in-list experiments)])
      (match-define (Experiment name/experiment tests) experiment)
      (Experiment
       name/experiment
       (for/list ([test (in-list tests)])
         (match-define (Test name/test cases) test)
         (Test
          name/test
          (for/list ([a-case (in-list cases)])
            (match-define (Case source (Subject analysis k) result) a-case)
            (define path/benchmark/result
              (~a (path/benchmark/results) "/"
                  "*"
                  "--experiment=" name/experiment
                  "--analysis=" analysis
                  "--test=" name/test
                  "--k=" k
                  ".txt"))
            (define results/files
              (sort (map path->string (glob path/benchmark/result)) string<?))
            (when (empty? results/files)
              (raise-user-error
               (~a "Cannot find result;\n"
                   " Case: " a-case "\n"
                   " Test: " test "\n"
                   " Experiment: " experiment)))
            (define result/text (file->string (last results/files)))
            (define running-time/raw
              (case analysis
                [(ddpa little-store)
                 (string->number
                  (car (regexp-match* #px"(?m:^real (.*)$)" result/text #:match-select cadr)))]
                [(p4f)
                 (/ (string->number
                     (car (regexp-match* #px"(?m:^Analysis run for: (.*) milliseconds$)"
                                         result/text
                                         #:match-select cadr)))
                    #i1000)]
                [else
                 (raise-user-error
                  (~a "Unknown analysis;\n"
                      " Case: " a-case "\n"
                      " Test: " test "\n"
                      " Experiment: " experiment))]))
            (define running-time (min running-time/raw (timeout)))
            (Case source (Subject analysis k) (Result running-time))))))))

  (for ([experiment (in-list experiments/with-results)])
    (match-define (Experiment name/experiment tests) experiment)
    (define path/base
      (~a (path/benchmark/results) "/"
          (moment->iso8601 (now/moment)) "--"
          name/experiment))
    (define path/text (~a path/base ".txt"))
    (define path/plot (~a path/base ".pdf"))

    (with-output-to-file path/text
      (λ ()
        (pretty-display experiments/with-results)))

    (define subjects
      (remove-duplicates
       (flatten
        (for/list ([test (in-list tests)])
          (match-define (Test name/test cases) test)
          (for/list ([a-case (in-list cases)])
            (match-define (Case source subject result) a-case)
            subject)))))
    (define render-tree
      (for/list ([subject (in-list subjects)]
                 [x-count (in-naturals)]
                 [color (in-naturals 1)])
        (discrete-histogram
         #:label (~a subject)
         #:skip 2.5 #:x-min x-count #:color color #:line-color color
         (for/list ([test (in-list tests)])
           (match-define (Test name/test cases) test)
           (match-define (Case source subject/case (Result running-time))
             (findf
              (match-lambda
                [(Case source subject/case (Result running-time))
                 (equal? subject subject/case)])
              cases))
           `#(,name/test ,running-time)))))

    (plot-file
     #:title (~a name/experiment)
     #:x-label "Test Case" #:y-label "Running Time (in seconds)"
     #:width (* (length tests) (test-case/width))
     render-tree path/plot)))