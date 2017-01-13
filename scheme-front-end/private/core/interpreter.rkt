#lang typed/racket/base
(require racket/match racket/list racket/function racket/set racket/format syntax/parse/define
         "generator.rkt" "abstract-syntax-tree.rkt" "core.rkt")

(provide evaluate)

(: evaluate (-> Program Program))
(define (evaluate program)
  (parameterize ([current-state (Program->state program)])
    (evaluate/state)
    (current-state->Program)))

(: evaluate/state (-> Void))
(define (evaluate/state)
  (match (current-state)
    [(state _ #f '() _ _) (void)]
    [_ (evaluate/state/step) (evaluate/state)]))

;; ---------------------------------------------------------------------------------------------------

(: evaluate/state/step (-> Void))
(define (evaluate/state/step)
  (match-define (state _ (Clause _ reduction-expression) _ _ _) (current-state))
  (match reduction-expression
    [(? Expression-Value?) (current-state/step #:literal? #t)]
    [(Conditional subject pattern match anti-match)
     (define subject/value (current-state/environment/lookup subject))
     (function/apply
      (if (Pattern/match? subject/value pattern) match anti-match)
      subject/value)]
    [(Operation-Unary (Operation-Unary-Operator-Variable-Lookup) variable)
     (current-state/reduced-value (current-state/environment/lookup variable))]
    [(Operation-Unary (Operation-Unary-Operator-Primitive-Boolean-Not) operand)
     (match-define (Primitive-Boolean boolean)
       (current-state/environment/lookup/typed operand Primitive-Boolean))
     (current-state/reduced-value (Primitive-Boolean (not boolean)))]
    [(Operation-Unary-Record-Projection (Operation-Unary-Operator-Record-Projection) record label)
     (current-state/reduced-value
      (current-state/environment/lookup
       (record/lookup (current-state/environment/lookup/typed record Record) label)))]
    [(Operation-Unary (Operation-Unary-Operator-Pointer-Dereference) pointer)
     (match-define (Pointer box) (current-state/environment/lookup/typed pointer Pointer))
     (current-state/reduced-value (current-state/environment/lookup box #:box #t))]
    [(Operation-Binary (Operation-Binary-Operator-Function-Application) function argument)
     (function/apply (current-state/environment/lookup/typed function Function)
                     (current-state/environment/lookup argument))]
    [(Operation-Binary (Operation-Binary-Operator-Pointer-Update) pointer argument)
     (match-define (Pointer box) (current-state/environment/lookup/typed pointer Pointer))
     (current-state/environment/update box (current-state/environment/lookup argument))
     (current-state/reduced-value (Record empty))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Integer-Addition)
                       operand/left operand/right)
     (match-define (Primitive-Integer integer/left)
       (current-state/environment/lookup/typed operand/left Primitive-Integer))
     (match-define (Primitive-Integer integer/right)
       (current-state/environment/lookup/typed operand/right Primitive-Integer))
     (current-state/reduced-value (Primitive-Integer (+ integer/left integer/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Integer-Subtraction)
                       operand/left operand/right)
     (match-define (Primitive-Integer integer/left)
       (current-state/environment/lookup/typed operand/left Primitive-Integer))
     (match-define (Primitive-Integer integer/right)
       (current-state/environment/lookup/typed operand/right Primitive-Integer))
     (current-state/reduced-value (Primitive-Integer (- integer/left integer/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Integer-LessThan)
                       operand/left operand/right)
     (match-define (Primitive-Integer integer/left)
       (current-state/environment/lookup/typed operand/left Primitive-Integer))
     (match-define (Primitive-Integer integer/right)
       (current-state/environment/lookup/typed operand/right Primitive-Integer))
     (current-state/reduced-value (Primitive-Boolean (< integer/left integer/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)
                       operand/left operand/right)
     (match-define (Primitive-Integer integer/left)
       (current-state/environment/lookup/typed operand/left Primitive-Integer))
     (match-define (Primitive-Integer integer/right)
       (current-state/environment/lookup/typed operand/right Primitive-Integer))
     (current-state/reduced-value (Primitive-Boolean (<= integer/left integer/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Integer-Equal)
                       operand/left operand/right)
     (match-define (Primitive-Integer integer/left)
       (current-state/environment/lookup/typed operand/left Primitive-Integer))
     (match-define (Primitive-Integer integer/right)
       (current-state/environment/lookup/typed operand/right Primitive-Integer))
     (current-state/reduced-value (Primitive-Boolean (= integer/left integer/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Boolean-And)
                       operand/left operand/right)
     (match-define (Primitive-Boolean boolean/left)
       (current-state/environment/lookup/typed operand/left Primitive-Boolean))
     (match-define (Primitive-Boolean boolean/right)
       (current-state/environment/lookup/typed operand/right Primitive-Boolean))
     (current-state/reduced-value (Primitive-Boolean (and boolean/left boolean/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Boolean-Or)
                       operand/left operand/right)
     (match-define (Primitive-Boolean boolean/left)
       (current-state/environment/lookup/typed operand/left Primitive-Boolean))
     (match-define (Primitive-Boolean boolean/right)
       (current-state/environment/lookup/typed operand/right Primitive-Boolean))
     (current-state/reduced-value (Primitive-Boolean (or boolean/left boolean/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-Boolean-Equal)
                       operand/left operand/right)
     (match-define (Primitive-Boolean boolean/left)
       (current-state/environment/lookup/typed operand/left Primitive-Boolean))
     (match-define (Primitive-Boolean boolean/right)
       (current-state/environment/lookup/typed operand/right Primitive-Boolean))
     (current-state/reduced-value (Primitive-Boolean (equal? boolean/left boolean/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-String-Concatenation)
                       operand/left operand/right)
     (match-define (Primitive-String string/left)
       (current-state/environment/lookup/typed operand/left Primitive-String))
     (match-define (Primitive-String string/right)
       (current-state/environment/lookup/typed operand/right Primitive-String))
     (current-state/reduced-value (Primitive-String (string-append string/left string/right)))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-String-Indexing)
                       subject/variable index/variable)
     (match-define (Primitive-String subject)
       (current-state/environment/lookup/typed subject/variable Primitive-String))
     (match-define (Primitive-Integer index)
       (current-state/environment/lookup/typed index/variable Primitive-Integer))
     (current-state/reduced-value (Primitive-String (string (string-ref subject index))))]
    [(Operation-Binary (Operation-Binary-Operator-Primitive-String-Equal)
                       operand/left operand/right)
     (match-define (Primitive-String string/left)
       (current-state/environment/lookup/typed operand/left Primitive-String))
     (match-define (Primitive-String string/right)
       (current-state/environment/lookup/typed operand/right Primitive-String))
     (current-state/reduced-value (Primitive-Boolean (equal? string/left string/right)))]))

;; ---------------------------------------------------------------------------------------------------

(: function/apply (-> Function Value Void))
(define (function/apply function argument)
  (match-define (state _ (and call-site (Clause assigned-variable _)) continuation _ _)
    (current-state))
  (match-define (Function parameter/freshened (Block body/clauses/freshened))
    (freshen function call-site))
  (match-define (Clause function/return _) (last body/clauses/freshened))
  (current-state/update
   [reduction-clause (Clause parameter/freshened (Expression-Value argument))]
   [continuation
    `(,@body/clauses/freshened
      ,(Clause assigned-variable
               (Operation-Unary (Operation-Unary-Operator-Variable-Lookup) function/return))
      ,@continuation)])
  (current-state/step))

(: record/lookup (-> Record Record-Label Variable))
(define (record/lookup record label)
  (match-define (Record fields) record)
  (define variable/maybe (assoc label fields))
  (unless variable/maybe
    (raise-evaluation-error
     (~a "record label not found\n"
         "  record: " (Core->core record) "\n"
         "  label: " (Core->core label))))
  (cdr variable/maybe))

(: record/unique-keys! (-> (U Record Pattern-Record) Void))
(define (record/unique-keys! record)
  (define fields
    (match record
      [(Record fields) fields]
      [(Pattern-Record fields) fields]))
  (define keys (map (inst car Record-Label Any) fields))
  (unless (= (length keys) (set-count (list->set keys)))
    (raise-evaluation-error
     (~a "record has repeated keys\n"
         "  record: " (Core->core record)))))

;; ---------------------------------------------------------------------------------------------------

(struct Variable-Freshened Variable
  ([freshened-at : (Listof Clause)])
  #:transparent)

(define/match/extension/Core->core
  [((Variable-Freshened name freshened-at))
   (core ,name #:freshened-at ,@(map Core->core freshened-at))])

(: freshen (-> Function Clause Function))
(define (freshen function at)
  (match-define (Function parameter (Block body/clauses)) function)
  (define bound-variables `(,parameter ,@(map Clause-assigned-variable body/clauses)))
  (define mapping
    (for/list : (Listof (Pairof Variable Variable)) ([from bound-variables])
      (define to
        (match from
          [(Variable-Freshened name freshened-at)
           (Variable-Freshened name (append freshened-at `(,at)))]
          [(Variable name) (Variable-Freshened name `(,at))]))
      `(,from . ,to)))
  (parameterize ([substitute/mapping mapping])
    (substitute/Function function)))

;; ---------------------------------------------------------------------------------------------------

(: Pattern/match? (-> Value Pattern Boolean))
(define (Pattern/match? value pattern)
  (match pattern
    [(? Pattern-Record?) (record/unique-keys! pattern)]
    [_ (void)])
  (match* (value pattern)
    [(_ (Pattern-Any)) #t]
    [((? Primitive-Integer?) (Pattern-Integer)) #t]
    [((Primitive-Boolean value/boolean) (Pattern-Boolean pattern/boolean))
     (equal? value/boolean pattern/boolean)]
    [((? Primitive-String?) (Pattern-String)) #t]
    [((? Function?) (Pattern-Function)) #t]
    [((Record value/fields) (Pattern-Record pattern/fields))
     (for/and ([pattern/field pattern/fields])
       (define value/field/maybe (assoc (car pattern/field) value/fields))
       (cond
         [value/field/maybe
          (Pattern/match? (current-state/environment/lookup (cdr value/field/maybe))
                          (cdr pattern/field))]
         [else #f]))]
    [((? Pointer?) (Pattern-Pointer)) #t]
    [(_ _) #f]))

;; ---------------------------------------------------------------------------------------------------

(struct state
  ([environment : (Listof (Pairof Variable Value))]
   [reduction-clause : (Option Clause)]
   [continuation : (Listof Clause)]
   [looked-up-variables : (Setof Variable)]
   [boxes : (Setof Variable)])
  #:transparent)

(define current-state : (Parameterof state) (make-parameter (state empty #f empty (set) (set))))

(: current-state->string (-> String))
(define (current-state->string)
  (match-define (state environment reduction-clause continuation looked-up-variables boxes)
    (current-state))
  (~a "  environment: " (for/list : (Listof (Listof Sexp))
                          ([binding environment])
                          `(,(Core->core (car binding)) ↦ ,(Core->core (cdr binding))))
      "\n"
      "  reduction-clause: " (and reduction-clause (Core->core reduction-clause)) "\n"
      "  continuation: " (map Core->core continuation) "\n"
      "  looked-up-variables: " (set-map looked-up-variables Core->core) "\n"
      "  boxes: " (set-map boxes Core->core)))

(: Program->state (-> Program state))
(define/match (Program->state program)
  [((Program (Block clauses)))
   (state empty (first clauses) (rest clauses) (set) (set))])

(: current-state->Program (-> Program))
(define (current-state->Program)
  (match-define (state environment #f '() _ _) (current-state))
  (Program (Block (for/list : (Listof Clause) ([binding environment])
                    (Clause (car binding) (Expression-Value (cdr binding)))))))

(define-simple-macro (current-state/update clause:expr ...)
  (current-state (struct-copy state (current-state) clause ...)))

(: current-state/step (-> [#:literal? Boolean] Void))
(define (current-state/step #:literal? [literal? #f])
  (match-define
    (state _ (Clause assigned-variable (Expression-Value value)) _ looked-up-variables boxes)
    (current-state))
  (when literal?
    (match value
      [(Record fields)
       (record/unique-keys! value)
       (define variables (map (inst cdr Any Variable) fields))
       (for ([variable variables])
         (current-state/environment/lookup variable))]
      [(Pointer box)
       (when (set-member? looked-up-variables box)
         (raise-evaluation-error
          (~a "tried to create pointer to non-box\n"
              "  non-box variable: " (Core->core box))))
       (when (set-member? boxes box)
         (raise-evaluation-error
          (~a "tried to create pointer to already used box\n"
              "  already-used box: " (Core->core box))))
       (current-state/update [boxes (set-add boxes box)])
       (current-state/environment/lookup box #:box #t)]
      [_ (void)]))
  (current-state/environment/add assigned-variable value)
  (current-state/update
   [reduction-clause (current-state/continuation/next)]
   [continuation (current-state/continuation/rest)]))

(: current-state/environment/add (-> Variable Value Void))
(define (current-state/environment/add assigned-variable value)
  (match-define (state environment _ _ _ _) (current-state))
  (when (current-state/environment/member? assigned-variable)
    (raise-evaluation-error
     (~a "variable already defined\n"
         "  variable: " (Core->core assigned-variable))))
  (current-state/update [environment (append environment `((,assigned-variable . ,value)))]))

(: current-state/environment/member? (-> Variable Boolean))
(define (current-state/environment/member? variable)
  (match-define (state environment _ _ _ _) (current-state))
  (and (assoc variable environment) #t))
  
(: current-state/environment/lookup (-> Variable [#:box Boolean] Value))
(define (current-state/environment/lookup variable #:box [box? #f])
  (match-define (state environment _ _ looked-up-variables boxes) (current-state))
  (define variable/box? (set-member? boxes variable))
  (when (and (not box?) variable/box?)
    (raise-evaluation-error
     (~a "looking up a box directly in the environment;\n"
         " should instead dereference the pointer to the box\n"
         "  variable: " (Core->core variable))))
  (when (and box? (not variable/box?))
    (raise-evaluation-error
     (~a "trying to dereference a non-box\n"
         "  non-box variable: " (Core->core variable))))
  (match (assoc variable environment)
    [`(,_ . ,value)
     (current-state/update [looked-up-variables (set-add looked-up-variables variable)])
     value]
    [#f
     (raise-evaluation-error
      (~a "variable not found\n"
          "  variable: " (Core->core variable)))]))

(define-simple-macro (current-state/environment/lookup/typed variable type)
  (match (current-state/environment/lookup variable)
    [(? (make-predicate type) value) value]
    [value
     (raise-evaluation-error
      (~a "type error\n"
          "  value: " (Core->core value) "\n"
          "  expected type: " 'type))]))

(: current-state/environment/update (-> Variable Value Void))
(define (current-state/environment/update box value)
  (match-define (state environment _ _ _ boxes) (current-state))
  (when (not (set-member? boxes box))
    (raise-evaluation-error
     (~a "updating in the environment something other than a box\n"
         "  non-box variable: " (Core->core box))))
  (define-values (environment/before environment/rest)
    (splitf-at environment
               (λ ([binding : (Pairof Variable Value)]) (not (equal? box (car binding))))))
  (when (empty? environment/rest)
    (raise-evaluation-error
     (~a "box not found\n"
         "  box: " (Core->core box))))
  (current-state/update
   [environment `(,@environment/before (,box . ,value) ,@(rest environment/rest))]))

(: current-state/reduced-value (-> Value Void))
(define (current-state/reduced-value reduced-value)
  (match-define (state _ (Clause assigned-variable _) _ _ _) (current-state))
  (current-state/update
   [reduction-clause (Clause assigned-variable (Expression-Value reduced-value))])
  (current-state/step))

(: current-state/continuation/next (-> (Option Clause)))
(define (current-state/continuation/next)
  (match (current-state)
    [(state _ _ '() _ _) #f]
    [(state _ _ continuation _ _) (first continuation)]))

(: current-state/continuation/rest (-> (Listof Clause)))
(define (current-state/continuation/rest)
  (match (current-state)
    [(state _ _ '() _ _) empty]
    [(state _ _ continuation _ _) (rest continuation)]))

;; ---------------------------------------------------------------------------------------------------

(: substitute/mapping (Parameterof (Listof (Pairof Variable Variable))))
(define substitute/mapping (make-parameter empty))

(: substitute/Block (-> Block Block))
(define/match (substitute/Block block)
  [((Block clauses)) (Block (map substitute/Clause clauses))])

(: substitute/Clause (-> Clause Clause))
(define/match (substitute/Clause clause)
  [((Clause assigned-variable expression))
   (Clause (substitute/Variable assigned-variable) (substitute/Expression expression))])

(: substitute/Expression (-> Expression Expression))
(define/match (substitute/Expression expression)
  [((? Expression-Value?)) (substitute/Expression-Value expression)]
  [((? Operation?)) (substitute/Operation expression)]
  [((? Conditional?)) (substitute/Conditional expression)])

(: substitute/Expression-Value (-> Expression-Value Expression-Value))
(define/match (substitute/Expression-Value expression/value)
  [((Expression-Value value)) (Expression-Value (substitute/Value value))])

(: substitute/Operation (-> Operation Operation))
(define/match (substitute/Operation operation)
  [((? Operation-Unary?)) (substitute/Operation-Unary operation)]
  [((? Operation-Binary?)) (substitute/Operation-Binary operation)])

(: substitute/Conditional (-> Conditional Conditional))
(define/match (substitute/Conditional conditional)
  [((Conditional subject pattern match anti-match))
   (Conditional (substitute/Variable subject) pattern
                (substitute/Function match) (substitute/Function anti-match))])

(: substitute/Value (-> Value Value))
(define/match (substitute/Value value)
  [((? Primitive?)) (substitute/Primitive value)]
  [((? Function?)) (substitute/Function value)]
  [((? Record?)) (substitute/Record value)]
  [((? Pointer?)) (substitute/Pointer value)])

(: substitute/Primitive (-> Primitive Primitive))
(define (substitute/Primitive primitive)
  primitive)

(: substitute/Function (-> Function Function))
(define/match (substitute/Function function)
  [((Function parameter body)) (Function (substitute/Variable parameter) (substitute/Block body))])

(: substitute/Record (-> Record Record))
(define/match (substitute/Record record)
  [((Record fields))
   (Record (for/list ([field fields]) `(,(car field) . ,(substitute/Variable (cdr field)))))])

(: substitute/Pointer (-> Pointer Pointer))
(define/match (substitute/Pointer pointer)
  [((Pointer box)) (Pointer (substitute/Variable box))])

(: substitute/Operation-Unary (-> Operation-Unary Operation-Unary))
(define/match (substitute/Operation-Unary operation/unary)
  [((? Operation-Unary-Record-Projection?))
   (substitute/Operation-Unary-Record-Projection operation/unary)]
  [((Operation-Unary operator operand)) (Operation-Unary operator (substitute/Variable operand))])

(: substitute/Operation-Unary-Record-Projection
   (-> Operation-Unary-Record-Projection Operation-Unary-Record-Projection))
(define/match (substitute/Operation-Unary-Record-Projection operation/unary/record/projection)
  [((Operation-Unary-Record-Projection operator operand label))
   (Operation-Unary-Record-Projection operator (substitute/Variable operand) label)])

(: substitute/Operation-Binary (-> Operation-Binary Operation-Binary))
(define/match (substitute/Operation-Binary operation/binary)
  [((Operation-Binary operator operand/left operand/right))
   (Operation-Binary operator
                     (substitute/Variable operand/left)
                     (substitute/Variable operand/right))])

(: substitute/Variable (-> Variable Variable))
(define (substitute/Variable from)
  (match (assoc from (substitute/mapping))
    [`(,_ . ,to) to]
    [_ from]))

;; ---------------------------------------------------------------------------------------------------

(struct exn:fail:user:evaluation-stuck exn:fail:user ())

(: raise-evaluation-error (-> String Nothing))
(define (raise-evaluation-error message)
  (raise
   (exn:fail:user:evaluation-stuck
    (~a "evaluation stuck: " message "\n"
        (current-state->string))
    (current-continuation-marks))))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require (for-syntax typed/racket/base) typed/rackunit syntax/parse/define "core.rkt" "parser.rkt")
  
  (define-syntax-parser check-evaluation
    [(_ program:expr expectations:expr)
     #'(let* ([program/parsed (parse program)]
              [program/evaluated (evaluate program/parsed)]
              [expectations/parsed (parse expectations)])
         (match-define (Program (Block program/evaluated/clauses)) program/evaluated)
         (match-define (Program (Block expectations/clauses)) expectations/parsed)
         (for ([expectation/clause expectations/clauses])
           (check-not-false (member expectation/clause program/evaluated/clauses))))]
    [(_ program:expr #:stuck)
     #'(check-exn exn:fail:user:evaluation-stuck? (λ () (evaluate (parse program))))])

  ;; Block
  (check-evaluation (core #:a 1 #:a 2) #:stuck)

  ;; Value
  (check-evaluation (core #:a 1) (core #:a 1))
  (check-evaluation (core #:a #t) (core #:a #t))
  (check-evaluation (core #:a "glados") (core #:a "glados"))
  (check-evaluation (core #:a (λ b #:c b)) (core #:a (λ b #:c b)))
  (check-evaluation (core #:a []) (core #:a []))
  (check-evaluation (core #:a [] #:b [#:c a]) (core #:b [#:c a]))
  (check-evaluation (core #:a 0 #:b (& a)) (core #:b (& a)))

  (check-evaluation (core #:a (λ b #:c functions-are-lazily-checked))
                    (core #:a (λ b #:c functions-are-lazily-checked)))
  
  (check-evaluation (core #:a [] #:b [#:c a #:c a]) #:stuck)
  (check-evaluation (core #:a [#:b c]) #:stuck)
  
  (check-evaluation (core #:a 0 #:l a #:b (& a)) #:stuck)
  (check-evaluation (core #:a 0 #:b (& a) #:c (& a)) #:stuck)
  (check-evaluation (core #:a 0 #:b (& c)) #:stuck)
  
  (check-evaluation (core #:a 1 #:p (& a) #:r [#:a a]) #:stuck)
  (check-evaluation (core #:a 1 #:r [#:a a] #:p (& a)) #:stuck)

  ;; Conditional
  (check-evaluation
   (core
    #:a 1
    #:c (~ a integer (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a "wheatley"
    #:c (~ a integer (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 3))

  ;; Variable lookup
  (check-evaluation (core #:a 1 #:b a) (core #:b 1))
  (check-evaluation (core #:a 1 #:b a #:c b) (core #:c 1))
  (check-evaluation (core #:a 1 #:b a #:c a) (core #:c 1))

  (check-evaluation (core #:a 1 #:b c) #:stuck)
  (check-evaluation (core #:a 0 #:b (& a) #:l a) #:stuck)

  ;; Boolean not
  (check-evaluation (core #:t #t #:f (not t)) (core #:f #f))
  
  (check-evaluation (core #:t "oops" #:f (not t)) #:stuck)

  ;; Record projection
  (check-evaluation (core #:a 1 #:b [#:a a] #:c (b #:a)) (core #:c 1))
  
  (check-evaluation (core #:a 1 #:b (a #:a)) #:stuck)
  (check-evaluation (core #:a 1 #:b [#:a a] #:c (b #:d)) #:stuck)

  ;; Pointer dereference
  (check-evaluation (core #:a 0 #:b (& a) #:c (* b)) (core #:c 0))
  
  (check-evaluation (core #:a 0 #:c (* a)) #:stuck)

  ;; Function application
  (check-evaluation (core #:f (λ fp #:fr 1) #:a 0 #:b (f a)) (core #:b 1))
  (check-evaluation (core #:f (λ fp #:fr fp) #:a 0 #:b (f a)) (core #:b 0))
  (check-evaluation (core #:non-local 1 #:f (λ fp #:fr non-local) #:a 0 #:b (f a)) (core #:b 1))
  
  (check-evaluation (core #:a 0 #:b 1 #:c (a b)) #:stuck)
  (check-evaluation (core #:a (λ b #:c functions-are-lazily-checked)
                          #:d 0
                          #:e (a d))
                    #:stuck)
  (check-evaluation (core #:reused/fine-because-of-freshening
                          (λ reused/fine-because-of-freshening #:c 1)
                          #:a 0
                          #:b (reused/fine-because-of-freshening a))
                    (core #:b 1))

  ;; Pointer update
  (check-evaluation (core #:a 0 #:p (& a) #:pa (* p) #:b 1 #:c (← p b) #:pb (* p))
                    (core #:pa 0 #:c [] #:pb 1))
  (check-evaluation (core #:one 1
                          #:current-count-box 0
                          #:current-count (& current-count-box)
                          #:next-count (λ ignored-1
                                         #:the-current-count (* current-count)
                                         #:the-next-count (+/integer the-current-count one)
                                         #:ignored-2 (← current-count the-next-count)
                                         #:ignored-3 (* current-count))
                          #:dummy "dummy"
                          #:a (next-count dummy)
                          #:b (next-count dummy)
                          #:c (next-count dummy))
                    (core #:a 1 #:b 2 #:c 3))

  ;; Primitive operations
  (check-evaluation (core #:one 1 #:two 2 #:three (+/integer one two))
                    (core #:three 3))
  (check-evaluation (core #:three 3 #:two 2 #:one (-/integer three two))
                    (core #:one 1))
  (check-evaluation (core #:three 3 #:two 2 #:three-less-than-two? (</integer three two))
                    (core #:three-less-than-two? #f))
  (check-evaluation (core #:three 3 #:two 2 #:three-less-than-or-equal-to-two? (<=/integer three two))
                    (core #:three-less-than-or-equal-to-two? #f))
  (check-evaluation (core #:three 3 #:three-equal-to-three? (<=/integer three three))
                    (core #:three-equal-to-three? #t))
  (check-evaluation (core #:t #t #:f #f #:true-and-false? (and t f))
                    (core #:true-and-false? #f))
  (check-evaluation (core #:t #t #:f #f #:true-or-false? (or t f))
                    (core #:true-or-false? #t))
  (check-evaluation (core #:t #t #:true-equals-to-true? (=/boolean t t))
                    (core #:true-equals-to-true? #t))
  (check-evaluation (core #:a "a" #:b "b" #:ab (+/string a b))
                    (core #:ab "ab"))
  (check-evaluation (core #:zero 0 #:ab "ab" #:a (@/string ab zero))
                    (core #:a "a"))
  (check-evaluation (core #:ab "ab" #:t (=/string ab ab))
                    (core #:t #t))

  ;; Patterns
  (check-evaluation
   (core
    #:a "wheatley"
    #:c (~ a any (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a 2
    #:c (~ a integer (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a #t
    #:c (~ a #t (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a "wheatley"
    #:c (~ a string (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a (λ b #:br b)
    #:c (~ a function (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a (λ b #:br b)
    #:d [#:a a]
    #:c (~ d [#:a function] (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  (check-evaluation
   (core
    #:a 0
    #:d (& a)
    #:c (~ d pointer (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   (core #:c 2))
  
  (check-evaluation
   (core
    #:a 0
    #:c (~ a [#:a function #:a any]
           (λ c-match #:c-match-return 2) (λ c-anti-match #:c-anti-match-return 3)))
   #:stuck))
