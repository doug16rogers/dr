; Puzzle created by Doug Rogers in 2009:
(define-macro (λ a . b) `(lambda (,a) ,b))
(define-macro (? n L o) `(if (< ,n (length ,L)) (list-ref ,L ,n) ,o))
(((λ r (λ f f f) (λ f r (λ x (f f) x)))
  (λ s λ n ? n '(263 14 2) (+ (s (- n 1)) (s (- n 2)) (s (- n 3)))))
 22)             ;; Whose birthday does this calculate?
