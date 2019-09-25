(define (abs x) (if (< x 0) (- x) x))

(define (foldl proc init list)
  (if list
    (foldl proc
           (proc init (car list))
           (cdr list))
     init))

(define (foldr proc init list)
  (if list
    (proc (car list)
          (foldr proc init (cdr list)))
    init))

(define (list . items)
  (foldr cons nil items))

; (define (add-two x) (+ x 2))

(define (reverse list)
  ; this is a comment
  (foldl (lambda (a x) (cons x a)) nil list))

; (define (add-one x) (+ x 1))
