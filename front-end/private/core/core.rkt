#lang typed/racket/base
(require syntax/parse/define)

(provide core)

(define-simple-macro (core any ...) `(any ...))