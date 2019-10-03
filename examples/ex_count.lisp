(define (count n) (if (= n 0) t (count (- n 1))))
(count 10000000)
