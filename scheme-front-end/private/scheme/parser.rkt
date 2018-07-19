#lang typed/racket/base

(module parser racket/base
  (require "concrete-syntax-tree.rkt" "abstract-syntax-tree.rkt"
           (except-in syntax/parse expr) racket/list)

  (provide parse)

  (define (parse program)
    (parse/top-level-form
     (parameterize ([current-namespace (make-base-namespace)])
       (expand (datum->syntax #f `(module program scheme ,@program))))))

  (define parse/top-level-form
    (syntax-parser
      #:literal-sets (kernel-literals)
      [(module (~datum program) (~datum scheme)
         (#%plain-module-begin
          (module (~datum configure-runtime) _ ...)
          form:module-level-form ...))
       (Program (map parse/module-level-form (syntax->list #'(form ...))))]))

  (define parse/module-level-form
    (syntax-parser
      #:literal-sets (kernel-literals)
      [form:general-top-level-form (parse/general-top-level-form #'form)]))

  (define parse/general-top-level-form
    (syntax-parser
      #:literal-sets (kernel-literals)
      [expr:expr (parse/expr #'expr)]
      [(define-values (id:id ...) expr:expr)
       (Definition (for/list ([id/syntax (syntax->list #'(id ...))])
                     (Variable-Scoped (syntax->datum id/syntax) id/syntax))
                   (parse/expr #'expr))]))

  (define parse/expr
    (syntax-parser
      #:literal-sets (kernel-literals)
      [id:id (Expression-Variable-Lookup (Variable-Scoped (syntax->datum #'id) #'id))]
      [(#%plain-lambda formals:formals body:expr ...+)
       (define-values (parameters parameter/rest) (parse/formals #'formals))
       (Expression-Function-Definition parameters parameter/rest
                                       (map parse/expr (syntax->list #'(body ...))))]
      [(if test:expr then:expr else:expr)
       (Expression-Conditional (parse/expr #'test) (parse/expr #'then) (parse/expr #'else))]
      [(begin expr:expr ...+)
       (Expression-Sequencing (map parse/expr (syntax->list #'(expr ...))))]
      [(begin0 expr:expr ...+)
       (Expression-Sequencing-Return-First (map parse/expr (syntax->list #'(expr ...))))]
      [(let-values ([(id:id ...) val:expr] ...)
         body:expr ...+)
       (Expression-Bindings
        (for/list ([binding/syntax (syntax->list #'([(id ...) val] ...))])
          (define binding/list (syntax->list binding/syntax))
          (define binding/ids (syntax->list (first binding/list)))
          `(,(for/list ([binding/id binding/ids])
               (Variable-Scoped (syntax->datum binding/id) binding/id))
            . ,(parse/expr (second binding/list))))
        (map parse/expr (syntax->list #'(body ...))))]
      [(letrec-values ([(id:id ...) val:expr] ...)
         body:expr ...+)
       (Expression-Bindings-Recursive
        (for/list ([binding/syntax (syntax->list #'([(id ...) val] ...))])
          (define binding/list (syntax->list binding/syntax))
          (define binding/ids (syntax->list (first binding/list)))
          `(,(for/list ([binding/id binding/ids])
               (Variable-Scoped (syntax->datum binding/id) binding/id))
            . ,(parse/expr (second binding/list))))
        (map parse/expr (syntax->list #'(body ...))))]
      [(set! id:id expr:expr)
       (Expression-Assignment (Variable-Scoped (syntax->datum #'id) #'id) (parse/expr #'expr))]
      [(quote datum:datum) (Expression-Datum (syntax->datum #'datum))]
      [(#%plain-app proc:expr arg:expr ...)
       (Expression-Function-Application (parse/expr #'proc)
                                        (map parse/expr (syntax->list #'(arg ...))))]))

  (define parse/formals
    (syntax-parser
      #:literal-sets (kernel-literals)
      [(id:id ...)
       (values
        (for/list ([id (syntax->list #'(id ...))])
          (Variable-Scoped (syntax->datum id) id))
        #f)]
      [(id:id ...+ . rest:id)
       (values
        (for/list ([id (syntax->list #'(id ...))])
          (Variable-Scoped (syntax->datum id) id))
        (Variable-Scoped (syntax->datum #'rest) #'rest))]
      [rest:id
       (values empty (Variable-Scoped (syntax->datum #'rest) #'rest))])))

(require "abstract-syntax-tree.rkt")

(require/typed/provide 'parser [parse (-> Sexp Program)])
