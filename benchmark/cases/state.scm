;; Suresh Jagannathan, Peter Thiemann, Stephen Weeks, and Andrew Wright. 1998. Single and loving it: must-alias analysis for higher-order languages. In Proceedings of the 25th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '98). ACM, New York, NY, USA, 329-341. DOI=http://dx.doi.org/10.1145/268946.268973.
;;
;; Translation of original example:
;;
;; (let ([f (lambda (z) (set-box! z -3))])
;;   (letrec ([g (lambda (x)
;;                 (let ([y (box x)])
;;                   (f y)
;;                   (if (< x 10)
;;                       (g (add1 x))
;;                       (unbox y))))])
;;     (g 0)))
;;
;; The analysis abstracts all numbers to the same abstract integer, but is precise about Boolean values, thus the following adaptation:

(let ([f (lambda (z) (set-box! z #t))])
  (letrec ([g (lambda (x)
                (let ([y (box x)])
                  (f y)
                  (if x
                      (g #t)
                      (unbox y))))])
    (g #f)))
