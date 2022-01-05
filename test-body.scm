(test-group "Simple currying"
  (test-eqv 5 ((lambda* (x y) (+ x y)) 2 3))
  (test-eqv 5 (((lambda* (x y) (+ x y)) 2) 3))
  (test-eqv 5 ((lambda* (w x y z) (+ w x y z)) 1 1 1 2))
  (test-eqv 5 (((((lambda* (w x y z) (+ w x y z)) 1) 1) 1 2)))
  (test-eqv 5 ((((lambda* (w x y z) (+ w x y z)) 1) 1 1 2)))
  (test-eqv 5 ((((lambda* (w x y z) (+ w x y z)) 1 1) 1 2)))
  (test-eqv 5 ((((lambda* (w x y z) (+ w x y z)) 1 1 1) 2)))
  (test-eqv 5 ((((((lambda* (w x y z) (+ w x y z)) 1) 1) 1) 2)))
  )

(test-group "Variadic"
  (test-equal '(3 (3 4))
              ((lambda* (a b . rest) (list (+ a b) rest)) 1 2 3 4))
  (test-equal
   '(3 (3 4))
   ((((lambda* (a b . rest) (list (+ a b) rest)) 1) 2) 3 4))
  )

(test-group "Nullary"
  (test-eqv 3 ((lambda* () (lambda* (x y) (+ x y))) 1 2))
  (test-eqv 3 (((lambda* () (lambda* (x y) (+ x y))) 1) 2))
  )

(test-group "Extra arguments"
  (test-eqv 20 ((lambda* (x y) (lambda* (z) (* z (+ x y)))) 2 3 4))
  )
