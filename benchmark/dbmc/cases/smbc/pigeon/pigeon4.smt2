; UNSAT

; the classic "pigeonhole" principle: trying to fit 5 pigeons
; into 4 holes is bound to fail.

(declare-sort hole 0)
(declare-const h1 hole)
(declare-const h2 hole)
(declare-const h3 hole)
(declare-const h4 hole)
(declare-const p1 hole)
(declare-const p2 hole)
(declare-const p3 hole)
(declare-const p4 hole)
(declare-const p5 hole)
(assert
 (and
  (not (= h1 h2))
  (not (= h1 h3))
  (not (= h1 h4))
  (not (= h2 h3))
  (not (= h2 h4))
  (not (= h3 h4))
 ))
(assert
  (and
    (not (= p1 p2))
    (not (= p1 p3))
    (not (= p1 p4))
    (not (= p1 p5))
    (not (= p2 p3))
    (not (= p2 p4))
    (not (= p2 p5))
    (not (= p3 p4))
    (not (= p3 p5))
    (not (= p4 p5))
  ))
; map pigeons to holes
(assert
 (forall ((p hole)) (or (= p h1) (= p h2) (= p h3) (= p h4))))
(check-sat)
