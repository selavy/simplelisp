(defmacro (ignore x) (cons 'quote (cons x nil)))
(ignore foo)
foo
