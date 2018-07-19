#lang typed/racket/base
(require (for-syntax typed/racket/base) racket/match racket/list racket/format racket/stxparam
         syntax/parse/define
         (prefix-in Scheme: "abstract-syntax-tree.rkt")
         (prefix-in Core: "../core/abstract-syntax-tree.rkt"))

(provide Scheme->Core)

(: Scheme->Core (-> Scheme:Program Core:Program))
(define/match (Scheme->Core scheme)
  [((Scheme:Program forms))
   (parameterize* ([current-variable-index 0]
                   [current-variable-mappings empty]
                   [current-forward-references-beyond-basic-block empty]
                   [current-available-forward-references (available-forward-references forms)])
     (Core:Program (Core:Block (append-map Scheme:Form->Core:Clauses forms))))])

(: Scheme:Form->Core:Clauses (-> Scheme:Form (Listof Core:Clause)))
(define/match (Scheme:Form->Core:Clauses form)
  [((? Scheme:Definition? definition)) (Scheme:Definition->Core:Clauses definition)]
  [((? Scheme:Expression? expression)) (Scheme:Expression->Core:Clauses expression)])

(: Scheme:Definition->Core:Clauses (-> Scheme:Definition (Listof Core:Clause)))
(define/match (Scheme:Definition->Core:Clauses form)
  [((Scheme:Definition `(,variable) expression))
   (define Core:variable (Scheme:Variable->Core:Variable variable))
   (begin0
     (Scheme:Expression->Core:Clauses expression Core:variable)
     (current-available-forward-references
      (remove* `(,Core:variable) (current-available-forward-references))))])

(: Scheme:Expression->Core:Clauses (->* (Scheme:Expression) (Core:Variable) (Listof Core:Clause)))
(define (Scheme:Expression->Core:Clauses expression [return (fresh-variable)])
  (match expression
    [(Scheme:Expression-Datum datum) (Sexp->Core:Clauses datum return)]
    [(Scheme:Expression-Variable-Lookup variable)
     `(,(Core:Clause return
                     (Core:Operation-Unary (Core:Operation-Unary-Operator-Variable-Lookup)
                                           (Scheme:Variable->Core:Variable variable))))]
    [(Scheme:Expression-Function-Definition _ _ body)
     (with-inner-scope (available-forward-references body)
       `(,(Core:Clause return
                       (Core:Expression-Value
                        (Scheme:Expression-Function-Definition->Core:Function expression)))))]
    [(Scheme:Expression-Conditional condition then else)
     (define-values (condition/clauses condition/return)
       (Scheme:Expression->Core:Clauses/inline condition))
     `(,@condition/clauses
       ,(Core:Clause
         return
         (Core:Conditional condition/return
                           (Core:Pattern-Boolean #f)
                           (Core:Function (fresh-variable)
                                          (Core:Block (Scheme:Expression->Core:Clauses else)))
                           (Core:Function (fresh-variable)
                                          (Core:Block (Scheme:Expression->Core:Clauses then))))))]
    [(Scheme:Expression-Sequencing expressions)
     (define-values (expressions/rest expressions/last) (split-at-right expressions 1))
     `(,@(append-map Scheme:Expression->Core:Clauses expressions/rest)
       ,@(Scheme:Expression->Core:Clauses (first expressions/last) return))]
    [(Scheme:Expression-Sequencing-Return-First expressions)
     (define clauses (append-map Scheme:Expression->Core:Clauses expressions))
     (match-define (Core:Clause first/return _) (first clauses))
     `(,@clauses
       ,(Core:Clause return (Core:Operation-Unary (Core:Operation-Unary-Operator-Variable-Lookup)
                                                  first/return)))]
    [(Scheme:Expression-Bindings bindings body)
     (define-values (body/rest body/last) (split-at-right body 1))
     `(,@(apply
          append
          (for/list : (Listof (Listof Core:Clause)) ([binding bindings])
            (match-define `((,bound-name) . ,expression) binding)
            (Scheme:Expression->Core:Clauses expression (Scheme:Variable->Core:Variable bound-name))))
       ,@(append-map Scheme:Expression->Core:Clauses body/rest)
       ,@(Scheme:Expression->Core:Clauses (first body/last) return))]
    [(Scheme:Expression-Bindings-Recursive bindings body)
     (define bound-variables
       (apply append
              (for/list : (Listof (Listof Core:Variable)) ([binding bindings])
                (map Scheme:Variable->Core:Variable (car binding)))))
     (parameterize ([current-available-forward-references
                     (append (current-available-forward-references) bound-variables)])
       (define-values (body/rest body/last) (split-at-right body 1))
       `(,@(apply
            append
            (for/list : (Listof (Listof Core:Clause)) ([binding bindings])
              (match-define `((,bound-name) . ,expression) binding)
              (define Core:bound-name (Scheme:Variable->Core:Variable bound-name))
              (begin0
                (Scheme:Expression->Core:Clauses expression Core:bound-name)
                (current-available-forward-references
                 (remove* `(,Core:bound-name) (current-available-forward-references))))))
         ,@(append-map Scheme:Expression->Core:Clauses body/rest)
         ,@(Scheme:Expression->Core:Clauses (first body/last) return)))]
    [(Scheme:Expression-Function-Application
      (Scheme:Expression-Variable-Lookup (Scheme:Variable function)) arguments)
     #:when (assoc function (current-primitives))
     (define primitive/pair (assoc function (current-primitives)))
     (if primitive/pair ((cdr primitive/pair) arguments return) (error 'invariant))]
    [(Scheme:Expression-Function-Application function `(,argument))
     (define-values (function/clauses function/return)
       (Scheme:Expression->Core:Clauses/inline function))
     (define-values (argument/clauses argument/return)
       (Scheme:Expression->Core:Clauses/inline argument))
     `(,@function/clauses
       ,@argument/clauses
       ,(Core:Clause return (Core:Operation-Binary
                             (Core:Operation-Binary-Operator-Function-Application)
                             function/return
                             argument/return)))]
    [(Scheme:Expression-Function-Application function arguments)
     (define arguments/record (fresh-variable))
     (define-values (function/clauses function/return)
       (Scheme:Expression->Core:Clauses/inline function))
     (define-values (arguments/clauses arguments/return)
       (for/fold ([arguments/clauses : (Listof Core:Clause) empty]
                  [arguments/return : (Listof Core:Variable) empty])
                 ([argument arguments])
         (define-values (argument/clauses argument/return)
           (Scheme:Expression->Core:Clauses/inline argument))
         (values `(,@arguments/clauses ,@argument/clauses)
                 `(,@arguments/return ,argument/return))))
     `(,@function/clauses
       ,@arguments/clauses
       ,(Core:Clause
         arguments/record
         (Core:Expression-Value
          (Core:Record
           (for/list : (Listof (Pairof Core:Record-Label Core:Variable))
             ([argument/return arguments/return]
              [index (in-naturals)])
             `(,(record-label-for index) . ,argument/return)))))
       ,(Core:Clause return (Core:Operation-Binary
                             (Core:Operation-Binary-Operator-Function-Application)
                             function/return
                             arguments/record)))]))

(: Scheme:Expression->Core:Clauses/inline
   (-> Scheme:Expression (Values (Listof Core:Clause) Core:Variable)))
(define/match (Scheme:Expression->Core:Clauses/inline expression)
  [((Scheme:Expression-Variable-Lookup variable))
   (values empty (Scheme:Variable->Core:Variable variable))]
  [(expression)
   (define result/clauses (Scheme:Expression->Core:Clauses expression))
   (values result/clauses (return-variable result/clauses))])

(: Sexp->Core:Clauses (->* (Sexp) (Core:Variable) (Listof Core:Clause)))
(define (Sexp->Core:Clauses datum [return (fresh-variable)])
  (match datum
    [(? exact-integer? integer)
     `(,(Core:Clause return (Core:Expression-Value (Core:Primitive-Integer integer))))]
    [(? boolean? boolean)
     `(,(Core:Clause return (Core:Expression-Value (Core:Primitive-Boolean boolean))))]
    [(? string? string)
     `(,(Core:Clause return (Core:Expression-Value (Core:Primitive-String string))))]
    [(? char? char)
     `(,(Core:Clause return (Core:Expression-Value (Core:Primitive-String (string char)))))]
    [(? symbol? symbol)
     (define-values (name/clauses name/return)
       (Scheme:Expression->Core:Clauses/inline (Scheme:Expression-Datum (symbol->string symbol))))
     `(,@name/clauses
       ,(Core:Clause return
                     (Core:Expression-Value
                      (Core:Record `((,record-label/symbol . ,name/return))))))]
    ['()
     (define empty-record (fresh-variable))
     `(,(Core:Clause empty-record (Core:Expression-Value (Core:Record empty)))
       ,(Core:Clause return
                     (Core:Expression-Value (Core:Record `((,record-label/null . ,empty-record))))))]
    [`(,head . ,tail)
     (define-values (head/clauses head/return)
       (Scheme:Expression->Core:Clauses/inline (Scheme:Expression-Datum head)))
     (define-values (tail/clauses tail/return)
       (Scheme:Expression->Core:Clauses/inline (Scheme:Expression-Datum tail)))
     `(,@head/clauses
       ,@tail/clauses
       ,(Core:Clause return
                     (Core:Expression-Value
                      (Core:Record `((,record-label/head . ,head/return)
                                     (,record-label/tail . ,tail/return))))))]))

(: Scheme:Expression-Function-Definition->Core:Function
   (-> Scheme:Expression-Function-Definition Core:Function))
(define/match (Scheme:Expression-Function-Definition->Core:Function expression)
  [((Scheme:Expression-Function-Definition `(,variable) #f body))
   (Core:Function (Scheme:Variable->Core:Variable variable)
                  (Core:Block (append-map Scheme:Expression->Core:Clauses body)))]
  [((Scheme:Expression-Function-Definition variables #f body))
   (define parameter (fresh-variable))
   (Core:Function
    parameter
    (Core:Block
     `(,@(for/list : (Listof Core:Clause) ([variable variables]
                                           [index (in-naturals)])
           (Core:Clause (Scheme:Variable->Core:Variable variable)
                        (Core:Operation-Unary-Record-Projection
                         (Core:Operation-Unary-Operator-Record-Projection)
                         parameter (record-label-for index))))
       ,@(append-map Scheme:Expression->Core:Clauses body))))])

(: Scheme:Variable->Core:Variable (-> Scheme:Variable Core:Variable))
(define/match (Scheme:Variable->Core:Variable variable)
  [((Scheme:Variable-Scoped name scope))
   (define mapping/maybe (assoc scope (current-variable-mappings) free-identifier=?))
   (cond
     [mapping/maybe (cdr mapping/maybe)]
     [else
      (define mapped (fresh-variable))
      (current-variable-mappings
       (cons `(,scope . ,mapped)
             (current-variable-mappings)))
      mapped])])

;; ---------------------------------------------------------------------------------------------------

(: current-primitives
   (Parameterof
    (Listof (Pairof Symbol (-> (Listof Scheme:Expression) Core:Variable (Listof Core:Clause))))))
(define current-primitives (make-parameter empty))

(define-simple-macro (define/primitive primitive:identifier
                       [(pattern:expr ...) body:expr ...+] ...+)
  (current-primitives
   (cons
    `(primitive
      .
      ,(λ ([arguments : (Listof Scheme:Expression)]
           [current-return : Core:Variable])
         (syntax-parameterize
             ([return (make-rename-transformer #'current-return)])
           (match arguments
             [(list pattern ...) body ...]
             ...
             [_ (raise-user-error (~a "Unsupported use of primitive “" 'primitive "”."))]))))
    (current-primitives))))

(define-syntax-parameter return
  (λ (stx) (raise-syntax-error #f "use of `return' not in `define/primitive'" stx)))

(define-simple-macro (define/primitive/unary-operator name:identifier operator:identifier)
  (define/primitive name
    [(operand)
     (define-values (operand/clauses operand/return)
       (Scheme:Expression->Core:Clauses/inline operand))
     `(,@operand/clauses
       ,(Core:Clause return (Core:Operation-Unary (operator) operand/return)))]))

(define-simple-macro (define/primitive/binary-operator name:identifier operator:identifier)
  (define/primitive name
    [(operand/left operand/right)
     (define-values (operand/left/clauses operand/left/return)
       (Scheme:Expression->Core:Clauses/inline operand/left))
     (define-values (operand/right/clauses operand/right/return)
       (Scheme:Expression->Core:Clauses/inline operand/right))
     `(,@operand/left/clauses
       ,@operand/right/clauses
       ,(Core:Clause return
                     (Core:Operation-Binary (operator)
                                            operand/left/return operand/right/return)))]
    [(operand/first operand/second operand/rest (... ...))
     (Scheme:Expression->Core:Clauses
      (Scheme:Expression-Function-Application
       (Scheme:Expression-Variable-Lookup (Scheme:Variable 'name))
       `(,(Scheme:Expression-Function-Application
           (Scheme:Expression-Variable-Lookup (Scheme:Variable 'name))
           `(,operand/first ,operand/second))
         ,@operand/rest))
      return)]))

(define-simple-macro (define/primitive/binary-operator/inverted name:identifier operator:identifier)
  (define/primitive name
    [(operand/left operand/right)
     (define-values (operand/left/clauses operand/left/return)
       (Scheme:Expression->Core:Clauses/inline operand/left))
     (define-values (operand/right/clauses operand/right/return)
       (Scheme:Expression->Core:Clauses/inline operand/right))
     `(,@operand/left/clauses
       ,@operand/right/clauses
       ,(Core:Clause return
                     (Core:Operation-Binary (operator)
                                            operand/right/return operand/left/return)))]))

;; ---------------------------------------------------------------------------------------------------

(define/primitive call-with-values
  [((Scheme:Expression-Function-Definition _ _ body) receiver)
   (define-values (body/rest body/last) (split-at-right body 1))
   `(,@(append-map Scheme:Expression->Core:Clauses body/rest)
     ,@(Scheme:Expression->Core:Clauses
        (Scheme:Expression-Function-Application receiver body/last) return))])

(define/primitive print-values
  [((Scheme:Expression-Variable-Lookup _))
   empty]
  [(argument)
   (Scheme:Expression->Core:Clauses argument return)])

(define/primitive/unary-operator not Core:Operation-Unary-Operator-Primitive-Boolean-Not)
(define/primitive add1
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable '+))
     `(,operand
       ,(Scheme:Expression-Datum 1)))
    return)])
(define/primitive sub1
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable '-))
     `(,operand
       ,(Scheme:Expression-Datum 1)))
    return)])
(define/primitive even?
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'zero?))
     `(,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable 'modulo))
         `(,operand
           ,(Scheme:Expression-Datum 2)))))
    return)])
(define/primitive odd?
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
     `(,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable 'modulo))
         `(,operand
           ,(Scheme:Expression-Datum 2)))
       ,(Scheme:Expression-Datum 1)))
    return)])
(define/primitive zero?
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
     `(,operand
       ,(Scheme:Expression-Datum 0)))
    return)])
(define/primitive pair?
  [(operand)
   (define-values (operand/clauses operand/return)
     (Scheme:Expression->Core:Clauses/inline operand))
   `(,@operand/clauses
     ,(Core:Clause
       return
       (Core:Conditional
        operand/return
        (Core:Pattern-Record
         `((,record-label/head . ,(Core:Pattern-Any))
           (,record-label/tail . ,(Core:Pattern-Any))))
        (Core:Function (fresh-variable) (Core:Block (Sexp->Core:Clauses #t)))
        (Core:Function (fresh-variable) (Core:Block (Sexp->Core:Clauses #f))))))])
(define/primitive null?
  [(operand)
   (define-values (operand/clauses operand/return)
     (Scheme:Expression->Core:Clauses/inline operand))
   `(,@operand/clauses
     ,(Core:Clause
       return
       (Core:Conditional
        operand/return
        (Core:Pattern-Record
         `((,record-label/null . ,(Core:Pattern-Any))))
        (Core:Function (fresh-variable) (Core:Block (Sexp->Core:Clauses #t)))
        (Core:Function (fresh-variable) (Core:Block (Sexp->Core:Clauses #f))))))])

;; FIXME: Typewise, these are correct, but it’s a hack to work around Odefa limitations.
(define/primitive random
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'add1))
     `(,operand))
    return)])
(define/primitive floor
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'add1))
     `(,operand))
    return)])
(define/primitive ceiling
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'add1))
     `(,operand))
    return)])
(define/primitive log
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'add1))
     `(,operand))
    return)])
(define/primitive char?
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing
     `(,operand
       ,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
         `(,(Scheme:Expression-Datum 0) ,(Scheme:Expression-Datum 0)))))
    return)])
(define/primitive symbol?
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing
     `(,operand
       ,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
         `(,(Scheme:Expression-Datum 0) ,(Scheme:Expression-Datum 0)))))
    return)])
(define/primitive string-length
  [(string)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing `(,string ,(Scheme:Expression-Datum 42)))
    return)])

(define/primitive/binary-operator + Core:Operation-Binary-Operator-Primitive-Integer-Addition)
(define/primitive/binary-operator - Core:Operation-Binary-Operator-Primitive-Integer-Subtraction)
(define/primitive/binary-operator < Core:Operation-Binary-Operator-Primitive-Integer-LessThan)
(define/primitive/binary-operator/inverted >
  Core:Operation-Binary-Operator-Primitive-Integer-LessThan)
(define/primitive/binary-operator <=
  Core:Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)
(define/primitive/binary-operator/inverted >=
  Core:Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)
(define/primitive/binary-operator = Core:Operation-Binary-Operator-Primitive-Integer-Equal)
;; “and” and “or” are short-circuiting. So they are macros, not functions.
;; The parser expands them away.
(define/primitive/binary-operator string-append
  Core:Operation-Binary-Operator-Primitive-String-Concatenation)
(define/primitive/binary-operator string-ref
  Core:Operation-Binary-Operator-Primitive-String-Indexing)

;; FIXME: Typewise, these are correct, but it’s a hack to work around Odefa limitations.
(define/primitive/binary-operator * Core:Operation-Binary-Operator-Primitive-Integer-Addition)
(define/primitive/binary-operator / Core:Operation-Binary-Operator-Primitive-Integer-Addition)
(define/primitive/binary-operator quotient Core:Operation-Binary-Operator-Primitive-Integer-Addition)
(define/primitive/binary-operator gcd Core:Operation-Binary-Operator-Primitive-Integer-Addition)
(define/primitive/binary-operator modulo Core:Operation-Binary-Operator-Primitive-Integer-Addition)

;; FIXME: Typewise, these are correct, but it’s a hack to work around Odefa limitations.
(define/primitive equal?
  [(operand/left operand/right)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing
     `(,operand/left
       ,operand/right
       ,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
         `(,(Scheme:Expression-Datum 0) ,(Scheme:Expression-Datum 0)))))
    return)])
(define/primitive eq?
  [(operand/left operand/right)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing
     `(,operand/left
       ,operand/right
       ,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable '=))
         `(,(Scheme:Expression-Datum 0) ,(Scheme:Expression-Datum 0)))))
    return)])
(define/primitive newline
  [() (Scheme:Expression->Core:Clauses (Scheme:Expression-Datum "<NEWLINE>") return)])
(define/primitive display
  [(operand)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing `(,operand ,(Scheme:Expression-Datum "<DISPLAY>")))
    return)])

;; ---------------------------------------------------------------------------------------------------

(define/primitive cons
  [(operand/left operand/right)
   (define-values (operand/left/clauses operand/left/return)
     (Scheme:Expression->Core:Clauses/inline operand/left))
   (define-values (operand/right/clauses operand/right/return)
     (Scheme:Expression->Core:Clauses/inline operand/right))
   `(,@operand/left/clauses
     ,@operand/right/clauses
     ,(Core:Clause return
                   (Core:Expression-Value
                    (Core:Record `((,record-label/head . ,operand/left/return)
                                   (,record-label/tail . ,operand/right/return))))))])

(define/primitive list
  [()
   (Scheme:Expression->Core:Clauses (Scheme:Expression-Datum empty) return)]
  [(head tail ...)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'cons))
     `(,head
       ,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable 'list))
         tail)))
    return)])

(define/primitive car
  [(list)
   (define-values (list/clauses list/return) (Scheme:Expression->Core:Clauses/inline list))
   `(,@list/clauses
     ,(Core:Clause
       return
       (Core:Operation-Unary-Record-Projection
        (Core:Operation-Unary-Operator-Record-Projection)
        list/return record-label/head)))])

(define/primitive cdr
  [(list)
   (define-values (list/clauses list/return) (Scheme:Expression->Core:Clauses/inline list))
   `(,@list/clauses
     ,(Core:Clause
       return
       (Core:Operation-Unary-Record-Projection
        (Core:Operation-Unary-Operator-Record-Projection)
        list/return record-label/tail)))])

(define/primitive cadr
  [(list)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'car))
     `(,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable 'cdr))
         `(,list))))
    return)])

(define/primitive caddr
  [(list)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Function-Application
     (Scheme:Expression-Variable-Lookup (Scheme:Variable 'car))
     `(,(Scheme:Expression-Function-Application
         (Scheme:Expression-Variable-Lookup (Scheme:Variable 'cdr))
         `(,(Scheme:Expression-Function-Application
             (Scheme:Expression-Variable-Lookup (Scheme:Variable 'cdr))
             `(,list))))))
    return)])

(define/primitive map
  [(function list)
   (define function/clauses (Scheme:Expression->Core:Clauses function))
   (define function/return (return-variable function/clauses))
   (define-values (list/clauses list/return)
     (Scheme:Expression->Core:Clauses/inline list))
   (define mapper (fresh-variable))
   (define mapper/parameter (fresh-variable))
   (define mapper/parameter/match (fresh-variable))
   (define head (fresh-variable))
   (define tail (fresh-variable))
   (define head/return (fresh-variable))
   (define tail/return (fresh-variable))
   `(,@function/clauses
     ,@list/clauses
     ,(Core:Clause
       mapper
       (Core:Expression-Value
        (Core:Function
         mapper/parameter
         (Core:Block
          `(,(Core:Clause
              (fresh-variable)
              (Core:Conditional
               mapper/parameter
               (Core:Pattern-Record
                `((,record-label/head . ,(Core:Pattern-Any))
                  (,record-label/tail . ,(Core:Pattern-Any))))
               (Core:Function
                mapper/parameter/match
                (Core:Block
                 `(,(Core:Clause
                     head
                     (Core:Operation-Unary-Record-Projection
                      (Core:Operation-Unary-Operator-Record-Projection)
                      mapper/parameter/match record-label/head))
                   ,(Core:Clause
                     tail
                     (Core:Operation-Unary-Record-Projection
                      (Core:Operation-Unary-Operator-Record-Projection)
                      mapper/parameter/match record-label/tail))
                   ,(Core:Clause
                     head/return
                     (Core:Operation-Binary
                      (Core:Operation-Binary-Operator-Function-Application)
                      function/return head))
                   ,(Core:Clause
                     tail/return
                     (Core:Operation-Binary
                      (Core:Operation-Binary-Operator-Function-Application)
                      mapper tail))
                   ,(Core:Clause
                     (fresh-variable)
                     (Core:Expression-Value
                      (Core:Record
                       `((,record-label/head . ,head/return)
                         (,record-label/tail . ,tail/return))))))))
               (Core:Function
                (fresh-variable)
                (Core:Block (Sexp->Core:Clauses '()))))))))))
     ,(Core:Clause
       return
       (Core:Operation-Binary (Core:Operation-Binary-Operator-Function-Application)
                              mapper list/return)))])

(define/primitive append
  [(list/left list/right)
   (define-values (list/left/clauses list/left/return)
     (Scheme:Expression->Core:Clauses/inline list/left))
   (define list/right/clauses (Scheme:Expression->Core:Clauses list/right))
   (define list/right/return (return-variable list/right/clauses))
   (define appender (fresh-variable))
   (define appender/parameter (fresh-variable))
   (define appender/parameter/match (fresh-variable))
   (define head (fresh-variable))
   (define tail (fresh-variable))
   (define head/return (fresh-variable))
   (define tail/return (fresh-variable))
   `(,@list/left/clauses
     ,@list/right/clauses
     ,(Core:Clause
       appender
       (Core:Expression-Value
        (Core:Function
         appender/parameter
         (Core:Block
          `(,(Core:Clause
              (fresh-variable)
              (Core:Conditional
               appender/parameter
               (Core:Pattern-Record
                `((,record-label/head . ,(Core:Pattern-Any))
                  (,record-label/tail . ,(Core:Pattern-Any))))
               (Core:Function
                appender/parameter/match
                (Core:Block
                 `(,(Core:Clause
                     head
                     (Core:Operation-Unary-Record-Projection
                      (Core:Operation-Unary-Operator-Record-Projection)
                      appender/parameter/match record-label/head))
                   ,(Core:Clause
                     tail
                     (Core:Operation-Unary-Record-Projection
                      (Core:Operation-Unary-Operator-Record-Projection)
                      appender/parameter/match record-label/tail))
                   ,(Core:Clause
                     tail/return
                     (Core:Operation-Binary
                      (Core:Operation-Binary-Operator-Function-Application)
                      appender tail))
                   ,(Core:Clause
                     (fresh-variable)
                     (Core:Expression-Value
                      (Core:Record
                       `((,record-label/head . ,head)
                         (,record-label/tail . ,tail/return))))))))
               (Core:Function
                (fresh-variable)
                (Core:Block
                 `(,(Core:Clause
                     (fresh-variable)
                     (Core:Operation-Unary
                      (Core:Operation-Unary-Operator-Variable-Lookup)
                      list/right/return))))))))))))
     ,(Core:Clause
       return
       (Core:Operation-Binary (Core:Operation-Binary-Operator-Function-Application)
                              appender list/left/return)))])

;; FIXME: Typewise, these are correct, but it’s a hack to work around Odefa limitations.
(define/primitive length
  [(list)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing `(,list ,(Scheme:Expression-Datum 42)))
    return)])

;; ---------------------------------------------------------------------------------------------------

(define/primitive box
  [(value)
   (define-values (value/clauses value/return)
     (Scheme:Expression->Core:Clauses/inline value))
   (define Core:box (fresh-variable))
   `(,@value/clauses
     ,(Core:Clause Core:box
                   (Core:Operation-Unary (Core:Operation-Unary-Operator-Variable-Lookup)
                                         value/return))
     ,(Core:Clause return (Core:Expression-Value (Core:Pointer Core:box))))])

(define/primitive set-box!
  [(box value)
   (define-values (box/clauses box/return)
     (Scheme:Expression->Core:Clauses/inline box))
   (define-values (value/clauses value/return)
     (Scheme:Expression->Core:Clauses/inline value))
   `(,@box/clauses
     ,@value/clauses
     ,(Core:Clause return
                   (Core:Operation-Binary
                    (Core:Operation-Binary-Operator-Pointer-Update)
                    box/return value/return)))])

(define/primitive unbox
  [(box)
   (define-values (box/clauses box/return)
     (Scheme:Expression->Core:Clauses/inline box))
   `(,@box/clauses
     ,(Core:Clause return
                   (Core:Operation-Unary
                    (Core:Operation-Unary-Operator-Pointer-Dereference)
                    box/return)))])

;; ---------------------------------------------------------------------------------------------------

(define/primitive error
  [(payloads ...)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing `(,@payloads ,(Scheme:Expression-Datum "<ERROR>")))
    return)])

(define/primitive void
  [(payloads ...)
   (Scheme:Expression->Core:Clauses
    (Scheme:Expression-Sequencing `(,@payloads ,(Scheme:Expression-Datum "<VOID>")))
    return)])

;; ---------------------------------------------------------------------------------------------------

(: current-variable-index (Parameterof Integer))
(define current-variable-index (make-parameter 0))

(: fresh-variable (-> Core:Variable))
(define (fresh-variable)
  (begin0
    (Core:Variable (string->symbol (~a "variable" (current-variable-index))))
    (current-variable-index (add1 (current-variable-index)))))

(: return-variable (-> (Listof Core:Clause) Core:Variable))
(define (return-variable clauses)
  (match-define (Core:Clause return _) (last clauses))
  return)

(: current-variable-mappings (Parameterof (Listof (Pairof Identifier Core:Variable))))
(define current-variable-mappings (make-parameter empty))

(: record-label-for (-> Any Core:Record-Label))
(define (record-label-for index)
  (Core:Record-Label (string->symbol (~a "argument" index))))

(: current-available-forward-references (Parameterof (Listof Core:Variable)))
(define current-available-forward-references (make-parameter empty))

(: available-forward-references (-> (Listof Scheme:Form) (Listof Core:Variable)))
(define (available-forward-references forms)
  (for/list ([form forms]
             #:when (Scheme:Definition? form))
    (match-define (Scheme:Definition `(,variable) _) form)
    (Scheme:Variable->Core:Variable variable)))

(: current-forward-references-beyond-basic-block (Parameterof (Listof Core:Variable)))
(define current-forward-references-beyond-basic-block (make-parameter empty))

(define-simple-macro (with-inner-scope function-body/available-forward-references:expr
                       function/clauses/generator:expr
                       ...)
  (let ()
    (define current-variable-mappings-without-forward-references-beyond-basic-block
      (for/list : (Listof (Pairof Identifier Core:Variable))
        ([current-variable-mapping (current-variable-mappings)]
         #:unless (member (cdr current-variable-mapping)
                          (current-forward-references-beyond-basic-block)))
        current-variable-mapping))
    (define-values (function/clauses function-variable-mappings)
      (parameterize* ([current-variable-mappings
                       current-variable-mappings-without-forward-references-beyond-basic-block]
                      [current-forward-references-beyond-basic-block
                       `(,@(current-available-forward-references)
                         ,@(current-forward-references-beyond-basic-block))]
                      [current-available-forward-references
                       function-body/available-forward-references])
        (values (let () function/clauses/generator ...) (current-variable-mappings))))
    (define non-local-aliases
      (for/fold ([non-local-aliases : (Listof Core:Clause) empty])
                ([function-variable-mapping function-variable-mappings])
        (match (assoc (car function-variable-mapping) (current-variable-mappings) free-identifier=?)
          [`(,_ . ,original)
           #:when (not (equal? (cdr function-variable-mapping) original))
           `(,@non-local-aliases
             ,(Core:Clause (cdr function-variable-mapping)
                           (Core:Operation-Unary (Core:Operation-Unary-Operator-Variable-Lookup)
                                                 original)))]
          [_ non-local-aliases])))
    `(,@non-local-aliases
      ,@function/clauses)))

;; ---------------------------------------------------------------------------------------------------

(define record-label/head (Core:Record-Label 'head))
(define record-label/tail (Core:Record-Label 'tail))
(define record-label/null (Core:Record-Label 'null))
(define record-label/symbol (Core:Record-Label 'symbol))