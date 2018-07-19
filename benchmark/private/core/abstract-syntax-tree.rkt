#lang typed/racket/base
(provide (all-defined-out))

(struct Core () #:transparent)

(struct Program Core
  ([block : Block])
  #:transparent)

(struct Block Core
  ([clauses : (Listof Clause)])
  #:transparent)

(struct Clause Core
  ([assigned-variable : Variable]
   [expression : Expression])
  #:transparent)

;; ---------------------------------------------------------------------------------------------------

(struct Expression Core () #:transparent)

(struct Expression-Value Expression
  ([value : Value])
  #:transparent)

(struct Operation Expression () #:transparent)

(struct Conditional Expression
  ([subject : Variable]
   [pattern : Pattern]
   [match : Function]
   [anti-match : Function])
  #:transparent)

;; ---------------------------------------------------------------------------------------------------

(struct Value Core () #:transparent)

(struct Primitive Value () #:transparent)

(struct Primitive-Integer Primitive
  ([primitive : Integer])
  #:transparent)

(struct Primitive-Boolean Primitive
  ([primitive : Boolean])
  #:transparent)

(struct Primitive-String Primitive
  ([primitive : String])
  #:transparent)

(struct Function Value
  ([parameter : Variable]
   [body : Block])
  #:transparent)

(struct Record Value
  ([fields : (Listof (Pairof Record-Label Variable))])
  #:transparent)

(struct Pointer Value
  ([box : Variable])
  #:transparent)

;; ---------------------------------------------------------------------------------------------------

(struct Operation-Unary Operation
  ([operator : Operation-Unary-Operator]
   [operand : Variable])
  #:transparent)

(struct Operation-Unary-Record-Projection Operation-Unary
  ([label : Record-Label])
  #:transparent)

(struct Operation-Unary-Operator Core () #:transparent)

(struct Operation-Unary-Operator-Variable-Lookup Operation-Unary-Operator () #:transparent)

(struct Operation-Unary-Operator-Primitive-Boolean-Not Operation-Unary-Operator () #:transparent)

(struct Operation-Unary-Operator-Record-Projection Operation-Unary-Operator () #:transparent)

(struct Operation-Unary-Operator-Pointer-Dereference Operation-Unary-Operator () #:transparent)

(struct Operation-Binary Operation
  ([operator : Operation-Binary-Operator]
   [operand/left : Variable]
   [operand/right : Variable])
  #:transparent)

(struct Operation-Binary-Operator Core () #:transparent)

(struct Operation-Binary-Operator-Function-Application Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Pointer-Update Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Primitive-Integer-Addition Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-Integer-Subtraction Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-Integer-LessThan Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-Integer-Equal Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Primitive-Boolean-And Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Primitive-Boolean-Or Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Primitive-Boolean-Equal Operation-Binary-Operator () #:transparent)

(struct Operation-Binary-Operator-Primitive-String-Concatenation Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-String-Indexing Operation-Binary-Operator
  () #:transparent)

(struct Operation-Binary-Operator-Primitive-String-Equal Operation-Binary-Operator () #:transparent)

;; ---------------------------------------------------------------------------------------------------

(struct Pattern Core () #:transparent)

(struct Pattern-Any Pattern () #:transparent)

(struct Pattern-Integer Pattern () #:transparent)

(struct Pattern-Boolean Pattern
  ([boolean : Boolean])
  #:transparent)

(struct Pattern-String Pattern () #:transparent)

(struct Pattern-Function Pattern () #:transparent)

(struct Pattern-Record Pattern
  ([fields : (Listof (Pairof Record-Label Pattern))])
  #:transparent)

(struct Pattern-Pointer Pattern () #:transparent)

;; ---------------------------------------------------------------------------------------------------

(struct Variable Core
  ([name : Symbol])
  #:transparent)

(struct Record-Label Core
  ([label : Symbol])
  #:transparent)
