(define make-adder (lambda (x) (lambda (y) (+ x y))))
(define add-two (make-adder 2))
(add-two 5)
(define (add-three x) (+ x 3))
(add-three 5)