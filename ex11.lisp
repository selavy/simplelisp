(quasiquote (0 1 2))

(quasiquote (0 (unquote (+ 1 2)) 4))

(quasiquote (0 (unquote-splicing (list 1 2)) 4))

(define bar 2)

(quasiquote (foo (unquote bar) baz))
