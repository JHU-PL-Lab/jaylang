#lang typed/racket/base
(require (for-syntax typed/racket/base racket/list racket/syntax)
         racket/match syntax/parse/define racket/format)

(provide define/match/extensible)

(define-syntax-parser define/match/extensible
  [(_ (~and form (function:identifier argument:identifier ...)) (~datum :) type:expr
      clause/original:expr ...)
   (with-syntax ([current-function (format-id #'function "current-~a" #'function)]
                 [define/match/extension/function
                   (format-id #'function "define/match/extension/~a" #'function)])
     #`(begin
         (: function type)
         (define form
           ((current-function) argument ...))

         (define-simple-macro (define/match/extension/function clause:expr (... ...+))
           (current-function
            (let ([rest-function (current-function)])
              (: function type)
              (define/match form
                clause (... ...)
                [(argument ...) (rest-function argument ...)])
              function)))

         (: current-function (Parameterof type))
         (define current-function
           (make-parameter
            (let ()
              (: function type)
              (define form
                (apply raise-arguments-error 'function
                       "match/extensible function not defined for arguments"
                       (append `(,(~a 'argument) ,argument) ...)))
              function)))

         #,@(if (empty? (syntax->list #'(clause/original ...)))
                #'()
                #'((define/match/extension/function clause/original ...)))))])