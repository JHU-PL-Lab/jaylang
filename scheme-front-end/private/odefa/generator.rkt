#lang typed/racket/base
(require syntax/parse/define racket/match racket/string racket/format racket/function racket/list
         "../core/abstract-syntax-tree.rkt" "../utils/define-match-extensible.rkt")

(provide Core->Odefa define/match/extension/Core->Odefa Odefa)

(define-type Odefa String)

(define/match/extensible (Core->Odefa core) : (-> Core Odefa))

(define/match/extension/Core->Odefa
  [((Program block)) (Core->Odefa block)])

(define/match/extension/Core->Odefa
  [((Block clauses))
   (string-join (for/list : (Listof Odefa) ([clause clauses])
                  (~a (indentation) (Core->Odefa clause)))
                "\n")])

(define/match/extension/Core->Odefa
  [((Clause assigned-variable expression))
   (~a (Core->Odefa assigned-variable) " = " (Core->Odefa expression) ";")])

(define/match/extension/Core->Odefa
  [((Expression-Value value)) (Core->Odefa value)])

(define/match/extension/Core->Odefa
  [((Conditional subject pattern match anti-match))
   (~a (Core->Odefa subject) " ~ " (Core->Odefa pattern) "\n"
       (indentation) "? " (Core->Odefa match) " : "
       (Core->Odefa anti-match))])

(define/match/extension/Core->Odefa
  [((Primitive-Integer primitive)) (~a primitive)])

(define/match/extension/Core->Odefa
  [((Primitive-Boolean primitive)) (if primitive "true" "false")])

(define/match/extension/Core->Odefa
  [((Primitive-String primitive)) (~a "\"" primitive"\"")])

(define/match/extension/Core->Odefa
  [((Function parameter body))
   (~a "fun " (Core->Odefa parameter) " -> (\n"
       (indented (Core->Odefa body)) "\n"
       (indentation) ")")])

(define/match/extension/Core->Odefa
  [((Record '())) "{}"]
  [((Record fields))
   (~a "{\n"
       (string-join
        (indented
         (for/list : (Listof Odefa) ([field fields])
           (~a (indentation)
               (Core->Odefa (car field)) " = " (Core->Odefa (cdr field)))) )
        ",\n")
       "\n" (indentation) "}")])

(define/match/extension/Core->Odefa
  [((Pointer box)) (~a "ref " (Core->Odefa box))])

(define/match/extension/Core->Odefa
  [((Operation-Unary operator operand))
   (~a (Core->Odefa operator) (Core->Odefa operand))])

(define/match/extension/Core->Odefa
  [((Operation-Unary-Record-Projection operator operand label))
   (~a (Core->Odefa operand) (Core->Odefa operator)
       (Core->Odefa label))])

(define/match/extension/Core->Odefa
  [((? Operation-Unary-Operator-Variable-Lookup?)) ""]
  [((? Operation-Unary-Operator-Primitive-Boolean-Not?)) "not "]
  [((? Operation-Unary-Operator-Record-Projection?)) "."]
  [((? Operation-Unary-Operator-Pointer-Dereference?)) "!"])

(define/match/extension/Core->Odefa
  [((Operation-Binary operator
                      operand/left operand/right))
   (~a (Core->Odefa operand/left) (Core->Odefa operator)
       (Core->Odefa operand/right))])

(define/match/extension/Core->Odefa
  [((Operation-Binary-Operator-Function-Application)) " "]
  [((Operation-Binary-Operator-Pointer-Update)) " <- "]
  [((Operation-Binary-Operator-Primitive-Integer-Addition)) " + "]
  [((Operation-Binary-Operator-Primitive-Integer-Subtraction)) " - "]
  [((Operation-Binary-Operator-Primitive-Integer-LessThan)) " < "]
  [((Operation-Binary-Operator-Primitive-Integer-LessThanOrEqualTo)) " <= "]
  [((Operation-Binary-Operator-Primitive-Integer-Equal)) " == "]
  [((Operation-Binary-Operator-Primitive-Boolean-And)) " and "]
  [((Operation-Binary-Operator-Primitive-Boolean-Or)) " or "]
  [((Operation-Binary-Operator-Primitive-Boolean-Equal)) " == "]
  [((Operation-Binary-Operator-Primitive-String-Concatenation)) " + "]
  [((Operation-Binary-Operator-Primitive-String-Indexing)) " @ "]
  [((Operation-Binary-Operator-Primitive-String-Equal)) " == "])

(define/match/extension/Core->Odefa
  [((Pattern-Any)) "_"]
  [((Pattern-Integer)) "int"]
  [((Pattern-Boolean boolean)) (if boolean "true" "false")]
  [((Pattern-String)) "string"]
  [((Pattern-Function)) "fun"]
  [((? Pattern-Record? pattern)) (Core->Odefa pattern)]
  [((Pattern-Pointer)) "ref"])

(define/match/extension/Core->Odefa
  [((Pattern-Record '())) "{}"]
  [((Pattern-Record fields))
   (~a "{\n"
       (string-join
        (indented
         (for/list : (Listof Odefa) ([field fields])
           (~a (indentation)
               (Core->Odefa (car field)) " = " (Core->Odefa (cdr field)))) )
        ",\n")
       "\n" (indentation) "}")])

(define/match/extension/Core->Odefa
  [((Variable name)) (~a name)])

(define/match/extension/Core->Odefa
  [((Record-Label label)) (~a label)])

;; ---------------------------------------------------------------------------------------------------

(define indentation/level : (Parameterof Nonnegative-Integer) (make-parameter 0))

(define indentation/string : String "  ")

(define (indentation) : Odefa
  (string-append* ((inst make-list Odefa) (indentation/level) indentation/string)))

(define-simple-macro (indented expression:expr ...+)
  (parameterize ([indentation/level (add1 (indentation/level))])
    expression ...))

;; ---------------------------------------------------------------------------------------------------

(module+ test
  (require typed/rackunit "../core/core.rkt" "../core/parser.rkt")

  (check-equal?
   (Core->Odefa (parse (core
                        #:one 1
                        #:thirtytwo 32
                        #:fifteen 15
                        #:eight 8
                        #:tak (λ takparameters
                                #:x (takparameters #:x)
                                #:y (takparameters #:y)
                                #:z (takparameters #:z)
                                #:yx (</integer y x)
                                #:notyx (not yx)
                                #:zr (~ notyx #t
                                        (λ notyxmatch #:notyxmatchr z)
                                        (λ notyxantimatch
                                          #:x1 (-/integer x one)
                                          #:takxparams [#:x x1 #:y y #:z z]
                                          #:takx (tak takxparams)
                                          #:y1 (-/integer y one)
                                          #:takyparams [#:x x #:y y1 #:z z]
                                          #:taky (tak takyparams)
                                          #:z1 (-/integer z one)
                                          #:takzparams [#:x x #:y y #:z z]
                                          #:takz (tak takzparams)
                                          #:takrparams [#:x takx #:y taky #:z takz]
                                          #:notyxantimatchreturn (tak takrparams))))
                        #:takparams [#:x thirtytwo #:y fifteen #:z eight]
                        #:r (tak takparams))))
 
   #<<odefa
one = 1;
thirtytwo = 32;
fifteen = 15;
eight = 8;
tak = fun takparameters -> (
  x = takparameters.x;
  y = takparameters.y;
  z = takparameters.z;
  yx = y < x;
  notyx = not yx;
  zr = notyx ~ true
  ? fun notyxmatch -> (
    notyxmatchr = z;
  ) : fun notyxantimatch -> (
    x1 = x - one;
    takxparams = {
      x = x1,
      y = y,
      z = z
    };
    takx = tak takxparams;
    y1 = y - one;
    takyparams = {
      x = x,
      y = y1,
      z = z
    };
    taky = tak takyparams;
    z1 = z - one;
    takzparams = {
      x = x,
      y = y,
      z = z
    };
    takz = tak takzparams;
    takrparams = {
      x = takx,
      y = taky,
      z = takz
    };
    notyxantimatchreturn = tak takrparams;
  );
);
takparams = {
  x = thirtytwo,
  y = fifteen,
  z = eight
};
r = tak takparams;
odefa
   ))
