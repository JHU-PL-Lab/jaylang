#lang typed/racket/base
(require racket/match racket/list racket/format
         "abstract-syntax-tree.rkt" "core.rkt" "../utils/define-match-extensible.rkt")

(provide Core->core define/match/extension/Core->core)

(define/match/extensible (Core->core expression) : (-> Core Sexp))

(define/match/extension/Core->core
  [((Program block)) (Core->core block)])

(define/match/extension/Core->core
  [((Block clauses))
   (append (for/list : (Listof (Listof Sexp))
             ([clause/core (map Core->core clauses)])
             (match clause/core
               [(? list?) clause/core])))])

(define/match/extension/Core->core
  [((Clause assigned-variable expression))
   (core ,(string->keyword (~a (Core->core assigned-variable)))
         ,(Core->core expression))])

(define/match/extension/Core->core
  [((Expression-Value value)) (Core->core value)])

(define/match/extension/Core->core
  [((Conditional subject pattern match anti-match))
   (core ~ ,(Core->core subject) ,(Core->core pattern)
         ,(Core->core match) ,(Core->core anti-match))])

(define/match/extension/Core->core
  [((Primitive-Integer primitive)) primitive]
  [((Primitive-Boolean primitive)) primitive]
  [((Primitive-String primitive)) primitive])

(define/match/extension/Core->core
  [((Function parameter body))
   (core λ ,(Core->core parameter) ,@(Core->core body))])

(define/match/extension/Core->core
  [((Record fields))
   (apply append
          (for/list : (Listof (Listof Sexp)) ([field fields])
            (core ,(Core->core (car field))
                  ,(Core->core (cdr field)))))])

(define/match/extension/Core->core
  [((Pointer box)) (core & ,(Core->core box))])

(define/match/extension/Core->core
  [((Operation-Unary (Operation-Unary-Operator-Variable-Lookup) operand)) (Core->core operand)]
  [((Operation-Unary operator operand))
   (core ,(Core->core operator) ,(Core->core operand))])

(define/match/extension/Core->core
  [((Operation-Unary-Record-Projection (Operation-Unary-Operator-Record-Projection) operand label))
   (core ,(Core->core operand) ,(Core->core label))])

(define/match/extension/Core->core
  [((Operation-Unary-Operator-Primitive-Boolean-Not)) 'not]
  [((Operation-Unary-Operator-Pointer-Dereference)) '*])

(define/match/extension/Core->core
  [((Operation-Binary (Operation-Binary-Operator-Function-Application) operand/left operand/right))
   (core ,(Core->core operand/left) ,(Core->core operand/right))]
  [((Operation-Binary operator operand/left operand/right))
   (core ,(Core->core operator)
         ,(Core->core operand/left) ,(Core->core operand/right))])

(define/match/extension/Core->core
  [((Operation-Binary-Operator-Pointer-Update)) '←]
  [((Operation-Binary-Operator-Primitive-Integer-Addition)) '+/integer]
  [((Operation-Binary-Operator-Primitive-Integer-Subtraction)) '-/integer]
  [((Operation-Binary-Operator-Primitive-Integer-LessThan)) '</integer]
  [((Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)) '<=/integer]
  [((Operation-Binary-Operator-Primitive-Integer-Equal)) '=/integer]
  [((Operation-Binary-Operator-Primitive-Boolean-And)) 'and]
  [((Operation-Binary-Operator-Primitive-Boolean-Or)) 'or]
  [((Operation-Binary-Operator-Primitive-Boolean-Equal)) '=/boolean]
  [((Operation-Binary-Operator-Primitive-String-Concatenation)) '+/integer]
  [((Operation-Binary-Operator-Primitive-String-Indexing)) '@/string]
  [((Operation-Binary-Operator-Primitive-String-Equal)) '=/string])

(define/match/extension/Core->core
  [((Pattern-Any)) 'any]
  [((Pattern-Integer)) 'integer]
  [((Pattern-Boolean boolean)) boolean]
  [((Pattern-String)) 'string]
  [((Pattern-Function)) 'function]
  [((? Pattern-Record? pattern)) (Core->core pattern)]
  [((Pattern-Pointer)) 'pointer])

(define/match/extension/Core->core
  [((Pattern-Record fields))
   (apply append
          (for/list : (Listof (Listof Sexp)) ([field fields])
            (core ,(Core->core (car field))
                  ,(Core->core (cdr field)))))])

(define/match/extension/Core->core
  [((Variable name)) name])

(define/match/extension/Core->core
  [((Record-Label label)) (string->keyword (~a label))])
