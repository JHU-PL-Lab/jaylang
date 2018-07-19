#lang typed/racket/base
(module+ main
  (require racket/cmdline racket/list
           "private/scheme/parser.rkt" "private/scheme/generator.rkt" "private/odefa/generator.rkt")

  (command-line
   #:program "Scheme → Odefa")

  (define program
    (let loop : Sexp ([sexp : (Listof Sexp) empty])
      (define a-sexp (read))
      (if (eof-object? a-sexp)
          sexp
          (loop `(,@sexp ,(cast a-sexp Sexp))))))
  (displayln (Core->Odefa (Scheme->Core (parse program)))))
