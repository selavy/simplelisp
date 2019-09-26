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

(define (unary-map proc list)
  (foldr (lambda (x rest) (cons (proc x) rest))
         nil
         list))

(define (map proc . arg-lists)
  (if (car arg-lists)
    (cons (apply proc (unary-map car arg-lists))
          (apply map  (cons proc
                            (unary-map cdr arg-lists))))
    nil))

(define (append a b) (foldr cons b a))

(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(defmacro (quasiquote x)
  (if (pair? x)
      (if (eq? (car x) 'unquote)
          (cadr x)
          (if (and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))

;; (defmacro (ignore x)
;;   (quasiquote (quote (unquote x))))
(defmacro (ignore X)
  `(quote ,X))

(ignore foo)
