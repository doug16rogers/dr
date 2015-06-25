; Central binomial coefficients. http://oeis.org/A000984
; Number of words of 2n bits with exactly n 1s (and 0s). So this is how many
; unique rows (and columns) are available for use in a 2n x 2n Binairo puzzle.
;       n =  0   1   2   3   4   5   6
;      2n =  0   2   4   6   8  10  12
; C(2n,n) =  1   2   6  20  70 252 924

; C2nn(n)   = (2n)!   / (n!)^2     = [2n * (2n-1) * (2n-2) * .. * (n+1)]     / [n * (n-1) * .. * 1]
; C2nn(n-1) = (2n-2)! / ((n-1)!)^2 = [              (2n-1) * .. * (n+1) * n] / [    (n-1) * .. * 1]
; C2nn(n) / C2nn(n-1) = [2n * (2n-1)] / [n * n]
; C2nn(n) = [2n * (2n - 1) / n^2] * C2nn(n-1)
; Ratio   = [2n * (2n - 1) / n^2] = [(4n - 2) / n] = 4 - (2/n)

; So the ratio goes slowly to 4 in the limit.
; For example,  (exact->inexact (C2nn-ratio 1000000)) => 3.999998.

(define (C2nn-ratio n) (/ (- (* 4 n) 2) n))

(define (C2nn n)
  (if (= n 0) 1
      (* (C2nn-ratio n) (C2nn (- n 1)))))

