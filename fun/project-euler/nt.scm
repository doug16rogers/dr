; Number Theory module for Scheme.

; With tail call:
(define (fact n) (if (= 1 n) 1 (* n (fact (- n 1)))))

; The definition in Problem 25 has F1 = 1, F2 = 1, so we have to match that
; here.
(define (fibnext n_max n fn1 fn2)
  (if (>= n n_max) fn1
      (fibnext n_max (+ n 1) (+ fn1 fn2) fn1)))

(define (fib n) (if (= n 0) 0 (fibnext n 1 1 0)))

(define (fibnext_by_value MAX n fn1 fn2)
  (if (>= fn1 MAX) n
      (fibnext_by_value MAX (+ n 1) (+ fn1 fn2) fn1)))

(define (fib_value_more_than MAX)
  (fibnext_by_value MAX 1 1 0))



