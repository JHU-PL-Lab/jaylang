#lang typed/racket/base
(provide (all-defined-out))

(struct Scheme () #:transparent)

(struct Program Scheme
  ([forms : (Listof Form)])
  #:transparent)

(struct Form Scheme () #:transparent)

(struct Definition Form
  ([identifiers : (Listof Variable)]
   [expression : Expression])
  #:transparent)

(struct Expression Form () #:transparent)

(struct Expression-Datum Expression
  ([datum : Sexp])
  #:transparent)

(struct Expression-Variable-Lookup Expression
  ([variable : Variable])
  #:transparent)

(struct Expression-Function-Definition Expression
  ([parameters : (Listof Variable)]
   [parameter/rest : (Option Variable)]
   [body : (Listof Expression)])
  #:transparent)

(struct Expression-Conditional Expression
  ([condition : Expression]
   [then : Expression]
   [else : Expression])
  #:transparent)

(struct Expression-Sequencing Expression
  ([expressions : (Listof Expression)])
  #:transparent)

(struct Expression-Sequencing-Return-First Expression
  ([expressions : (Listof Expression)])
  #:transparent)

(struct Expression-Bindings Expression
  ([bindings : (Listof (Pairof (Listof Variable) Expression))]
   [body : (Listof Expression)])
  #:transparent)

(struct Expression-Bindings-Recursive Expression
  ([bindings : (Listof (Pairof (Listof Variable) Expression))]
   [body : (Listof Expression)])
  #:transparent)

(struct Expression-Assignment Expression
  ([variable : Variable]
   [expression : Expression])
  #:transparent)

(struct Expression-Function-Application Expression
  ([function : Expression]
   [arguments : (Listof Expression)])
  #:transparent)

(struct Variable Scheme
  ([name : Symbol])
  #:transparent)

(struct Variable-Scoped Variable
  ([scope : Identifier])
  #:transparent)