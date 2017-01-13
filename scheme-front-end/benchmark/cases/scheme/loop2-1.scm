(letrec ((lp1 (lambda (i x)
                (if (= 0 i)
                    x
                    (letrec ((lp2 (lambda (j f y)
                                    (if (= 0 j)
                                        (lp1 (- i 1) y)
                                        (lp2 (- j 1) f (f y))))))
                      (lp2 10 (lambda (n) (+ n i)) x))))))
  (lp1 10 0))
