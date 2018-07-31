#lang racket

(require racket/runtime-path gregor file/glob math/statistics)

;; ---------------------------------------------------------------------------------------------------
;; RESULTS PATH

(define-runtime-path results/path "results")

(unless (directory-exists? results/path)
  (raise-user-error "Can’t find results at ~a" results/path))

;; ---------------------------------------------------------------------------------------------------
;; DATA STRUCTURES

(struct result (experiment
                case
                analysis
                k
                when

                load-average

                running-time
                memory-use

                program-points
                function-definitions
                function-calls
                variable-references
                non-local-variable-references
                lexical-depth)
  #:transparent)

;; ---------------------------------------------------------------------------------------------------
;; HELPERS

(define (extract pattern input [converter values] [default 'undefined])
  (define match-result (regexp-match* pattern input #:match-select second))
  (match match-result
    ['() default]
    [_ (converter (first match-result))]))

(define (time->milliseconds time)
  (for/sum ([(accessor multiplier) (in-dict `((,->hours . ,(* 60 60 1000))
                                              (,->minutes . ,(* 60 1000))
                                              (,->seconds . 1000)
                                              (,->milliseconds . 1)))])
    (* (accessor time) multiplier)))

;; ---------------------------------------------------------------------------------------------------
;; COLLECT RESULTS

(define results
  (for/list ([file (in-list (glob (build-path results/path "experiment*.txt")))])
    (define output (file->string file))
    (result
     (extract #px"experiment=(.*?)--.*" file string->symbol)
     (extract #px".*--case=(.*?)--.*" file string->symbol)
     (extract #px".*--analysis=(.*?)--.*" file string->symbol)
     (extract #px".*--k=(.*?)--.*" file string->number)
     (extract #px".*--(.*?)\\.txt" file iso8601->moment)

     (extract #px"load average: (.*?)," output string->number)

     (extract #px"Elapsed \\(wall clock\\) time \\(h:mm:ss or m:ss\\): (.*?)\n" output
              (λ (time)
                (time->milliseconds
                 (with-handlers ([exn:gregor:parse? (λ _ (parse-time time "m:s.S"))])
                   (parse-time time "h:m:s.S")))))
     (extract #px"Maximum resident set size \\(kbytes\\): (.*?)\n" output string->number)

     (extract #px"source file program points: (\\d+)" output string->number)
     (extract #px"source file function definitions: (\\d+)" output string->number)
     (extract #px"source file function calls: (\\d+)" output string->number)
     (extract #px"source file variable references: (\\d+)" output string->number)
     (extract #px"source file non-local variable references: (\\d+)" output string->number)
     (extract #px"source file maximum lexical depth: (\\d+)" output string->number))))

;; ---------------------------------------------------------------------------------------------------
;; RENDER TABLE

(for ([case (in-list (remove-duplicates (map result-case results)))])
  (display (~a case ",,"))

  (define results/case (filter (λ (result) (equal? case (result-case result))) results))

  (for* ([a-ddpa-result (in-value (findf (λ (result) (equal? 'ddpa (result-analysis result)))
                                         results/case))]
         [selector (in-list (list result-program-points
                                  result-function-definitions
                                  result-function-calls
                                  result-variable-references
                                  result-non-local-variable-references
                                  result-lexical-depth))])
    (display (~a "," (selector a-ddpa-result))))

  (for ([experiment (in-list '(baseline polyvariance))])
    (define results/experiment
      (filter (λ (result) (equal? experiment (result-experiment result))) results/case))
    (define results/p4f
      (filter (λ (result) (equal? 'p4f (result-analysis result))) results/experiment))
    (define results/ddpa
      (filter (λ (result) (equal? 'ddpa (result-analysis result))) results/experiment))

    (display (~a "," (result-k (first results/p4f)) "," (result-k (first results/ddpa))))

    (for ([selector (in-list (list result-running-time
                                   result-memory-use
                                   result-load-average))])
      (define p4f/results (map selector results/p4f))
      (define p4f/mean (mean p4f/results))
      (define p4f/standard-deviation (stddev p4f/results))
      (define ddpa/results (map selector results/ddpa))
      (define ddpa/mean (mean ddpa/results))
      (define ddpa/standard-deviation (stddev ddpa/results))
      (define ddpa/p4f (/ ddpa/mean p4f/mean))
      (display (~a "," (exact->inexact p4f/mean) "," (exact->inexact p4f/standard-deviation)
                   "," (exact->inexact ddpa/mean) "," (exact->inexact ddpa/standard-deviation)
                   "," (exact->inexact ddpa/p4f)))))

  (display "\n"))
