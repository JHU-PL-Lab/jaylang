#lang racket/base
(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class program
  (pattern (block:block)))

(define-splicing-syntax-class block
  (pattern (~seq clause:clause ...+)))

(define-splicing-syntax-class clause
  (pattern (~seq assigned-variable:keyword expression:expression)))

(define-syntax-class expression
  (pattern expression:expression/value)
  (pattern expression:operation)
  (pattern expression:conditional))

(define-syntax-class expression/value
  (pattern value:value))

(define-syntax-class operation
  (pattern operation:operation/unary)
  (pattern operation:operation/binary))

(define-syntax-class conditional
  (pattern ((~datum ~) subject:variable pattern:pattern* match:function anti-match:function)))

(define-syntax-class value
  (pattern value:primitive)
  (pattern value:function)
  (pattern value:record)
  (pattern value:pointer))

(define-syntax-class primitive
  (pattern primitive:integer)
  (pattern primitive:boolean)
  (pattern primitive:str))

(define-syntax-class function
  (pattern ((~datum λ) parameter:variable body:block)))

(define-syntax-class record
  (pattern [(~seq label:record/label variable:variable) ...]))

(define-syntax-class pointer
  (pattern ((~datum &) box:variable)))

(define-syntax-class operation/unary
  (pattern operation:operation/unary/variable/lookup)
  (pattern operation:operation/unary/record/projection)
  (pattern (operator:operation/unary/operator operand:variable)))

(define-syntax-class operation/unary/variable/lookup
  (pattern variable:variable))

(define-syntax-class operation/unary/record/projection
  (pattern (operand:variable label:record/label)))

(define-syntax-class operation/unary/operator
  (pattern (~datum not))
  (pattern (~datum *)))

(define-syntax-class operation/binary
  (pattern operation:operation/binary/function/application)
  (pattern (operator:operation/binary/operator operand/left:variable operand/right:variable)))

(define-syntax-class operation/binary/function/application
  (pattern (function:variable argument:variable)))

(define-syntax-class operation/binary/operator
  (pattern (~or (~datum ←) (~datum <-)))
  (pattern (~datum +/integer))
  (pattern (~datum -/integer))
  (pattern (~datum </integer))
  (pattern (~datum <=/integer))
  (pattern (~datum =/integer))
  (pattern (~datum and))
  (pattern (~datum or))
  (pattern (~datum =/boolean))
  (pattern (~datum +/string))
  (pattern (~datum @/string))
  (pattern (~datum =/string)))

(define-syntax-class pattern*
  (pattern (~datum any))
  (pattern (~datum integer))
  (pattern pattern:boolean)
  (pattern (~datum string))
  (pattern (~datum function))
  (pattern pattern:pattern/record)
  (pattern (~datum pointer)))

(define-syntax-class pattern/record
  (pattern [(~seq label:record/label pattern:pattern*) ...]))

(define-syntax-class variable
  (pattern variable:identifier))

(define-syntax-class record/label
  (pattern record/label:keyword))