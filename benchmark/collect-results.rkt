#lang racket

(require racket/runtime-path file/glob gregor)

(define-runtime-path results/path "results")

(unless (directory-exists? results/path)
  (raise-user-error "Can’t find results at ~a" results/path))

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

(define (extract pattern input [converter values] [default 'undefined])
  (define match-result (regexp-match* pattern input #:match-select second))
  (match match-result
    ['() default]
    [_ (converter (first match-result))]))

(define results
  (for/list ([file (in-list (glob (build-path results/path "*analysis=ddpa*")))])
    (define output (file->string file))
    (result
     (extract #px"experiment=(.*?)--.*" file string->symbol)
     (extract #px".*--case=(.*?)--.*" file string->symbol)
     (extract #px".*--analysis=(.*?)--.*" file string->symbol)
     (extract #px".*--k=(.*?)--.*" file string->number)
     (extract #px".*--(.*?)\\.txt" file iso8601->moment)

     (extract #px"load average: (.*?)\n" output
              (λ (averages) (map string->number (string-split averages ", "))))

     (extract #px"Elapsed \\(wall clock\\) time \\(h:mm:ss or m:ss\\): (.*?)\n" output
              (λ (time)
                (->milliseconds
                 (with-handlers ([exn:gregor:parse? (λ _ (parse-time time "m:s.S"))])
                   (parse-time time "h:m:s.S")))))
     (extract #px"Maximum resident set size \\(kbytes\\): (.*?)\n" output string->number)

     (extract #px"source file program points: (\\d+)" output string->number)
     (extract #px"source file function definitions: (\\d+)" output string->number)
     (extract #px"source file function calls: (\\d+)" output string->number)
     (extract #px"source file variable references: (\\d+)" output string->number)
     (extract #px"source file non-local variable references: (\\d+)" output string->number)
     (extract #px"source file maximum lexical depth: (\\d+)" output string->number))))
