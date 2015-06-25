; Copyright (c) 2015 Doug Rogers under the terms of the MIT License.
; See http://www.opensource.org/licenses/mit-license.html..

(load "to-base.sch")

; -----------------------------------------------------------------------------
; This file provides a simple calculator for tau up to as many digits as
; desired (given the wait time to produce them) in a given radix. It actually
; provides a function that calculates any generalized continued fraction
; until it is good to that many digits.
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; Calculates a GCF to N digits in the given radix, returning a rational
; number.

; This function is the internal one with more elements in its API. The
; simpler API is found below in (gcf-to-digits an-func bn-func radix N),
; below.

; The GCF is evaluated using the standard recurrence rules until the
; difference between terms is less than half of the last digit's weight -
; that is, (radix^(-n) / 2). The last digit(s) might still be wrong, but it
; will be likely to be close.

;   delta[n] = (A[n]/B[n]) - (A[n-1]/B[n-1])
;            = (A[n]*B[n-1] - A[n-1]*B[n]) / (B[n]*B[n-1])

; We want |delta[n]| < 1 / (2 * radix^N), or
; |2 * radix^N * (A[n]*B[n-1] - A[n-1]*B[n])| < |B[n]*B[n-1]|, or
; |C[n]| < |D[n]|

; Since we already carry and calculate B[n] and B[n-1], there's no need to
; carry D[n] separately unless there's some savings in the calculation. But
; there is not:
;     D[n] = B[n] * B[n-1]
;          = (b[n]B[n-1] + a[n]B[n-2]) * B[n-1]
;          = b[n]*B[n-1]*B[n-1] + a[n]*B[n-1]*B[n-2]
;          = b[n]*B[n-1]^2 + a[n]*D[n-1]
; We're not saving much by carrying D[n] forward as a parameter since we
; will still have to calculate B[n-1]^2, a rather large number. So D[n]
; will be calculated each time we check for convergence.

; For C[n] the calculation breaks down kinda nicely.
;   C[n]  = 2 * radix^N * C'[n]
; where
;   C'[n] = A[n]*B[n-1] - A[n-1]*B[n]
;         = (b[n]*A[n-1] + a[n]*A[n-2])*B[n-1] - A[n-1]*(b[n]*B[n-1] + a[n]*B[n-2])
;         = b[n]*A[n-1]*B[n-1] + a[n]*A[n-2]*B[n-1] - b[n]*A[n-1]*B[n-1] - a[n]*A[n-1]*B[n-2]
;         = b[n]*(A[n-1]*B[n-1] - A[n-1]*B[n-1]) + a[n]*(A[n-2]*B[n-1] - A[n-1]*B[n-2])
;         = b[n]*(0) + a[n]*(A[n-2]*B[n-1] - A[n-1]*B[n-2])
;         = -a[n] * (A[n-1]*B[n-2] - A[n-2]*B[n-1])
;         = -a[n] * C'[n-1]
; We will keep C[n] nonnegative, so by choosing
;    C[0] = 2 * radix^N
; we get the very handy simplification for n > 0:
;    C[n] = |a[n]| * C[n-1]

; Our concession to performance - how often to check for convergence to the
; number of digits we want in our given radix. The calculation (* Bn Bn-1)
; eats up quite a bit of time.
(define kGcfToDigitsCheckInterval 256)

(define (gcf-to-digits-step an-func bn-func n An An-1 Bn Bn-1 Cn)
  (if (and (zero? (modulo n kGcfToDigitsCheckInterval)) (< Cn (abs (* Bn Bn-1))))
      (/ An Bn)
      (let* (( n+1 (+ 1 n))
             (an+1 (an-func n+1))
             (bn+1 (bn-func n+1))
             (An+1 (+ (* bn+1 An) (* an+1 An-1)))
             (Bn+1 (+ (* bn+1 Bn) (* an+1 Bn-1)))
             (Cn+1 (* (abs an+1) Cn)))
        (gcf-to-digits-step an-func bn-func n+1 An+1 An Bn+1 Bn Cn+1))))

; -----------------------------------------------------------------------------
; Given functions that calculate a[n] (starting at n=1) and b[n] (starting at
; n=0), calculate the generalized continued fraction's recurrence formulas
; until the value represented by A[n] / B[n] is within half of the Nth digit
; in the given radix.

; See http://en.wikipedia.org/wiki/Generalized_continued_fraction.

(define (gcf-to-digits an-func bn-func radix N)
  (gcf-to-digits-step an-func bn-func 0 (bn-func 0) 1 1 0 (* 2 (expt (abs radix) N))))

; -----------------------------------------------------------------------------
; Like gcf-to-digits above, but returns a string. Requires that to-base.sch
; be loaded.
(define (gcf-to-digits-string an-func bn-func radix N)
  (to-base radix (gcf-to-digits an-func bn-func radix N) -1 N))

; -----------------------------------------------------------------------------
; Stuff for tau in particular...
; -----------------------------------------------------------------------------
(define (sqr x) (* x x))

; These are from an unnamed GCF for pi listed in the Wikipedia article above.
(define (tau-an n) (if (= n 1) 8 (sqr (- n 1))))    ; a[n] are squares.
(define (tau-bn n) (if (= n 0) 0 (- (* 2 n) 1)))    ; b[n] are odd numbers.

(define (tau-to-digits radix N)
  (gcf-to-digits tau-an tau-bn radix N))

(define (tau-to-digits-string radix N)
  (gcf-to-digits-string tau-an tau-bn radix N))

; -----------------------------------------------------------------------------
; To calculate z^(m/n) from Wikipedia article above.
; Find x, y such that z = x^n + y, then...
(define (nth-root-of-<x^n+y>^m-an-func x n y m)
  (lambda (k)
    (if (odd? k)
        (let ((c (/ (- k 1) 2))) (* y (+ (* c n) m)))
        (let ((c (/       k 2))) (* y (- (* c n) m))))))

(define (nth-root-of-<x^n+y>^m-bn-func x n y m)
  (let* ((nx^n-m (* n (expt x (- n m))))
         (x^m    (expt x m))
         (2x^m   (* 2 x^m)))
    (lambda (k)
      (cond
       ([zero? k] x^m)
       ([odd? k]  (* k nx^n-m))
       (#t        2x^m)))))

(define (nth-root-of-<x^n+y>^m-to-digits x n y m radix N)
  (gcf-to-digits
   (nth-root-of-<x^n+y>^m-an-func x n y m)
   (nth-root-of-<x^n+y>^m-bn-func x n y m)
   radix N))

(define (nth-root-of-<x^n+y>^m-to-digits-string x n y m radix N)
  (to-base radix (nth-root-of-<x^n+y>^m-to-digits x n y m radix N) -1 N))

(define (find-x^n+y=z-minimize-y best x z n)
  (let ((x^n (expt x n)))
    (if (> x^n z)   ; Stop when x^n > z.
        best
        (let ((y (- z x^n)))
          (if (< y (cdr best))
              (find-x^n+y=z-minimize-y (cons x y) (+ 1 x) z n)
              (find-x^n+y=z-minimize-y best       (+ 1 x) z n))))))
; This needs some optimization, like starting at something near log z / n.
(define (find-x^n+y=z z n)
  (find-x^n+y=z-minimize-y (cons 0 z) 1 z n))

(define (expt-m/n-to-digits z m n radix N)
  (let ((xy (find-x^n+y=z z n)))
    (nth-root-of-<x^n+y>^m-to-digits (car xy) n (cdr xy) m radix N)))

(define (expt-m/n-to-digits-string z m n radix N)
  (to-base radix (expt-m/n-to-digits z m n radix N) -1 N))
