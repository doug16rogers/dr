(load "to-base.sch")

(define (gcf-to-digits-fat-cutoff ak-func bk-func k Ak Ak-1 Bk Bk-1 Ck)
  (if (and (zero? (modulo k 256)) (< Ck (abs (* Bk Bk-1))))
      (list ak-func bk-func k Ak Ak-1 Bk Bk-1 Ck)
      (let* (( k+1 (+ 1 k))
             (ak+1 (ak-func k+1))
             (bk+1 (bk-func k+1))
             (Ak+1 (+ (* bk+1 Ak) (* ak+1 Ak-1)))
             (Bk+1 (+ (* bk+1 Bk) (* ak+1 Bk-1)))
             (Ck+1 (* (abs ak+1) Ck)))
        (gcf-to-digits-fat-cutoff ak-func bk-func k+1 Ak+1 Ak Bk+1 Bk Ck+1))))

(define (gcf-to-digits-fat ak-func bk-func radix N)
  (gcf-to-digits-fat-cutoff ak-func bk-func 0 (bk-func 0) 1 1 0 (* 2 (expt (abs radix) N))))

; Takes the list returned from gcf-to-digits-fat.
(define (gcf-to-digits-fat-rational L)
  (/ (cadddr L) (cadddr (cddr L))))

(define (sqr x) (* x x))
(define (tau-ak k) (if (= k 1) 8 (sqr (- k 1))))    ; a[k] are squares.
(define (tau-bk k) (if (= k 0) 0 (- (* 2 k) 1)))    ; b[k] are odd numbers.

(define (gcf-to-digits-fat-string ak-func bk-func radix N)
  (to-base radix (gcf-to-digits-fat-rational (gcf-to-digits-fat ak-func bk-func radix N))))

(define (tau-to-digits-fat radix N) (gcf-to-digits-fat tau-ak tau-bk radix N))
(define (tau-to-digits-fat-string radix N)
  (gcf-to-digits-fat-string tau-ak tau-bk radix N))
