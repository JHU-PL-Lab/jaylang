#lang racket/base
(require (rename-in syntax/parse [expr expr*]))

(provide (all-defined-out) expr*)

(define-syntax-class top-level-form
  #:literal-sets (kernel-literals)
  (pattern form:general-top-level-form)
  (pattern (#%expression expr:expr))
  (pattern (module id:id module-path:module-path
             (#%plain-module-begin
              form:module-level-form ...)))
  (pattern (begin form:top-level-form ...))
  (pattern (begin-for-syntax form:top-level-form ...)))

(define-syntax-class module-level-form
  #:literal-sets (kernel-literals)
  (pattern form:general-top-level-form)
  (pattern (#%provide raw-provide-spec:raw-provide-spec ...))
  (pattern (begin-for-syntax form:module-level-form ...))
  (pattern form:submodule-form)
  (pattern (#%declare declaration-keyword:declaration-keyword ...)))

(define-syntax-class submodule-form
  #:literal-sets (kernel-literals)
  (pattern (module id:id module-path:module-path
             (#%plain-module-begin
              form:module-level-form ...)))
  (pattern (module* id:id module-path:module-path
             (#%plain-module-begin
              form:module-level-form ...)))
  (pattern (module* id:id #f
             (#%plain-module-begin
              form:module-level-form ...))))

(define-syntax-class general-top-level-form
  #:literal-sets (kernel-literals)
  (pattern expr:expr)
  (pattern (define-values (id:id ...) expr:expr))
  (pattern (define-syntaxes (id:id ...) expr:expr))
  (pattern (#%require raw-require-spec:raw-require-spec ...)))

(define-syntax-class expr
  #:literal-sets (kernel-literals)
  (pattern id:id)
  (pattern (#%plain-lambda formals:formals body:expr ...+))
  (pattern (case-lambda (formals:formals body:expr ...+) ...))
  (pattern (if test:expr then:expr else:expr))
  (pattern (begin expr:expr ...+))
  (pattern (begin0 expr:expr ...+))
  (pattern (let-values ([(id:id ...) val:expr] ...)
             body:expr ...+))
  (pattern (letrec-values ([(id:id ...) val:expr] ...)
             body:expr ...+))
  (pattern (set! id:id expr:expr))
  (pattern (quote datum:datum))
  (pattern (quote-syntax datum:datum))
  (pattern (quote-syntax datum:datum #:local))
  (pattern (with-continuation-mark key:expr val:expr result:expr))
  (pattern (#%plain-app proc:expr arg:expr ...))
  (pattern (#%top . id:id))
  (pattern (#%variable-reference id:id))
  (pattern (#%variable-reference (#%top . id:id)))
  (pattern (#%variable-reference)))

(define-syntax-class formals
  #:literal-sets (kernel-literals)
  (pattern (id:id ...))
  (pattern (id:id ...+ . rest:id))
  (pattern rest:id))

;; ---------------------------------------------------------------------------------------------------

;; Overly simplified auxiliary definitions.

(define-syntax-class module-path
  #:literal-sets (kernel-literals)
  (pattern id:id))

(define-syntax-class raw-provide-spec
  #:literal-sets (kernel-literals)
  (pattern id:id))

(define-syntax-class declaration-keyword
  #:literal-sets (kernel-literals)
  (pattern keyword:keyword))

(define-syntax-class raw-require-spec
  #:literal-sets (kernel-literals)
  (pattern id:id))

(define-syntax-class datum
  #:literal-sets (kernel-literals)
  (pattern expr:expr*))