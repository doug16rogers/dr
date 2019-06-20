
;; The rationals get fairly large much more quickly than I initially
;; expected.  Running (rat-krieger-n 2) will bog down the machine after the
;; 24th iteration.  Without the exact->inexact call, it's possible to see
;; just how large these numbers get. It makes sense since for (rat-krieger-n
;; 2) we're starting with a denominator that's 5 digits wide (2501/10000) and
;; that is squared with each iteration.

;; For an integer version, see the int-xxx calls after the rat-xxx ones.

(define (rat-mand-it z c) (+ (* z z) c))
(define (rat-mand-n n z c)
  (begin
    (display n)
    (display " ")
    (display (exact->inexact z))
    (newline)
  (if (> z 2)
      n
      (rat-mand-n (+ n 1) (rat-mand-it z c) c)))
 )
 
(define (rat-start-c minus-power-10)
  (+ (/ 1 4) (expt 10 (- 0 minus-power-10))))
  
(define (rat-krieger-n minus-power-10)
  (rat-mand-n 0 0 (rat-start-c minus-power-10)))

; -----------------------------------------------------------------------------
;; Below is my attempt at an integer-only version. This avoids all divisions.
;; It is still really, really slow even for small n.

;; Return 25000...00001. Assuming outside will track power of 10 as
;; denominator.
(define (int-start-c minus-power-10)
  (+ 1 (* 25 (expt 10 (- minus-power-10 2)))))

(define (int-mand-n n check-2-val z-numerator c-numerator power-10)
  (display "n=") (display n)
;;  (display " check-2-val=") (display check-2-val)
;;  (display " z-value=") (display (exact->inexact (/ z-numerator power-10)))
;;  (display " z-numerator=") (display z-numerator)
;;  (display " c-numerator=") (display c-numerator)
;;  (display " power-10=") (display power-10)
  (newline)
  (if (> z-numerator check-2-val)
      n
      (let ((new-c-numerator (* c-numerator power-10)))   ; Next c must be multiplied by denominator.
        (int-mand-n
         (+ n 1)                                          ; Next value of n.
         (* check-2-val power-10)                         ; Next comparison must be multiplied by denominator.
         (+ (* z-numerator z-numerator) new-c-numerator)  ; z^z + c
         new-c-numerator                                  ; Next c (already multiplied).
         (* power-10 power-10))                           ; Square the denominator (from z^2).
   )))
 
(define (int-krieger-n minus-power-10)
  (if (< minus-power-10 2)
      (begin  (display "sorry; n must be at least 2") (newline) 0)
      (int-mand-n
       0
       (* 2 (expt 10 minus-power-10))
       0
       (int-start-c minus-power-10)
       (expt 10 minus-power-10))))
