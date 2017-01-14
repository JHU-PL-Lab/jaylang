#lang typed/racket/base
(require syntax/parse/define)

(provide scheme)

(define-simple-macro (scheme any ...) `(any ...))