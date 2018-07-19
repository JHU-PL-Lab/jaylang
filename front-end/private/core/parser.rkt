#lang typed/racket/base

(module parser racket/base
  (require syntax/parse "concrete-syntax-tree.rkt" "abstract-syntax-tree.rkt")

  (provide parse)

  (define (parse program)
    (parse/program program))

  (define parse/program
    (syntax-parser
      [program:program (Program (parse/block #'program.block))]))

  (define parse/block
    (syntax-parser
      [(block:block)
       (Block (map parse/clause (syntax->list #'(block.clause ...))))]))

  (define parse/clause
    (syntax-parser
      [(clause:clause)
       (Clause
        (parse/variable (string->symbol (keyword->string (syntax->datum #'clause.assigned-variable))))
        (parse/expression #'clause.expression))]))

  (define parse/expression
    (syntax-parser
      [expression:expression/value (parse/expression/value #'expression)]
      [expression:operation (parse/operation #'expression)]
      [expression:conditional (parse/conditional #'expression)]))

  (define parse/expression/value
    (syntax-parser
      [expression:expression/value (Expression-Value (parse/value #'expression))]))

  (define parse/operation
    (syntax-parser
      [operation:operation/unary (parse/operation/unary #'operation)]
      [operation:operation/binary (parse/operation/binary #'operation)]))

  (define parse/conditional
    (syntax-parser
      [conditional:conditional
       (Conditional (parse/variable #'conditional.subject)
                    (parse/pattern #'conditional.pattern)
                    (parse/function #'conditional.match)
                    (parse/function #'conditional.anti-match))]))

  (define parse/value
    (syntax-parser
      [value:primitive (parse/primitive #'value)]
      [value:function (parse/function #'value)]
      [value:record (parse/record #'value)]
      [value:pointer (parse/pointer #'value)]))

  (define parse/primitive
    (syntax-parser
      [primitive:integer (Primitive-Integer (syntax->datum #'primitive))]
      [primitive:boolean (Primitive-Boolean (syntax->datum #'primitive))]
      [primitive:str (Primitive-String (syntax->datum #'primitive))]))

  (define parse/function
    (syntax-parser
      [function:function
       (Function (parse/variable #'function.parameter) (parse/block #'function.body))]))

  (define parse/record
    (syntax-parser
      [record:record
       (Record
        (map cons
             (map parse/record/label (syntax->datum #'(record.label ...)))
             (map parse/variable (syntax->datum #'(record.variable ...)))))]))

  (define parse/pointer
    (syntax-parser
      [pointer:pointer (Pointer (parse/variable #'pointer.box))]))

  (define parse/operation/unary
    (syntax-parser
      [operation:operation/unary/variable/lookup (parse/operation/unary/variable/lookup #'operation)]
      [operation:operation/unary/record/projection
       (parse/operation/unary/record/projection #'operation)]
      [(operator:operation/unary/operator operand:variable)
       (Operation-Unary
        (parse/operation/unary/operator #'operator) (parse/variable #'operand))]))

  (define parse/operation/unary/variable/lookup
    (syntax-parser
      [variable:variable
       (Operation-Unary (Operation-Unary-Operator-Variable-Lookup) (parse/variable #'variable))]))

  (define parse/operation/unary/record/projection
    (syntax-parser
      [operation:operation/unary/record/projection
       (Operation-Unary-Record-Projection
        (Operation-Unary-Operator-Record-Projection)
        (parse/variable #'operation.operand)
        (parse/record/label #'operation.label))]))

  (define parse/operation/unary/operator
    (syntax-parser
      [(~datum not) (Operation-Unary-Operator-Primitive-Boolean-Not)]
      [(~datum *) (Operation-Unary-Operator-Pointer-Dereference)]))

  (define parse/operation/binary
    (syntax-parser
      [operation:operation/binary/function/application
       (parse/operation/binary/function/application #'operation)]
      [(operator:operation/binary/operator operand/left:variable operand/right:variable)
       (Operation-Binary
        (parse/operation/binary/operator #'operator)
        (parse/variable #'operand/left)
        (parse/variable #'operand/right))]))

  (define parse/operation/binary/function/application
    (syntax-parser
      [operation:operation/binary/function/application
       (Operation-Binary (Operation-Binary-Operator-Function-Application)
                         (parse/variable #'operation.function)
                         (parse/variable #'operation.argument))]))

  (define parse/operation/binary/operator
    (syntax-parser
      [(~or (~datum ←) (~datum <-)) (Operation-Binary-Operator-Pointer-Update)]
      [(~datum +/integer) (Operation-Binary-Operator-Primitive-Integer-Addition)]
      [(~datum -/integer) (Operation-Binary-Operator-Primitive-Integer-Subtraction)]
      [(~datum </integer) (Operation-Binary-Operator-Primitive-Integer-LessThan)]
      [(~datum <=/integer) (Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)]
      [(~datum =/integer) (Operation-Binary-Operator-Primitive-Integer-Equal)]
      [(~datum and) (Operation-Binary-Operator-Primitive-Boolean-And)]
      [(~datum or) (Operation-Binary-Operator-Primitive-Boolean-Or)]
      [(~datum =/boolean) (Operation-Binary-Operator-Primitive-Boolean-Equal)]
      [(~datum +/string) (Operation-Binary-Operator-Primitive-String-Concatenation)]
      [(~datum @/string) (Operation-Binary-Operator-Primitive-String-Indexing)]
      [(~datum =/string) (Operation-Binary-Operator-Primitive-String-Equal)]))
  
  (define parse/pattern
    (syntax-parser
      [(~datum any) (Pattern-Any)]
      [(~datum integer) (Pattern-Integer)]
      [pattern:boolean (Pattern-Boolean (syntax->datum #'pattern))]
      [(~datum string) (Pattern-String)]
      [(~datum function) (Pattern-Function)]
      [pattern:pattern/record (parse/pattern/record #'pattern)]
      [(~datum pointer) (Pattern-Pointer)]))

  (define parse/pattern/record
    (syntax-parser
      [pattern:pattern/record
       (Pattern-Record
        (map cons
             (map parse/record/label (syntax->datum #'(pattern.label ...)))
             (map parse/pattern (syntax->datum #'(pattern.pattern ...)))))]))
  
  (define parse/variable
    (syntax-parser
      [variable:variable (Variable (syntax->datum #'variable))]))
  
  (define parse/record/label
    (syntax-parser
      [record/label:record/label
       (Record-Label (string->symbol (keyword->string (syntax->datum #'record/label))))])))

(require "abstract-syntax-tree.rkt")

(require/typed/provide 'parser [parse (-> Sexp Program)])
