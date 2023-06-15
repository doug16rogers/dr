;; Full 2-argument Ackermann function:
(define (ack m n)
  (if (= m 0) (+ n 1)
      (if (= n 0) (ack (- m 1) 1)
          (ack (- m 1) (ack m (- n 1))))))

;; Functions for small values of m:
(define (ack0 n) (+ n 1))
(define (ack1 n) (+ n 2))
(define (ack2 n) (+ (* 2 n) 3))
(define (ack3 n) (- (expt 2 (+ n 3)) 3))
(define (tower1 m n)
  (if (= 0 n) 1
      (expt m (tower1 m (- n 1)))))
(define (ack4 n) (- (tower1 2 (+ n 3)) 3))
