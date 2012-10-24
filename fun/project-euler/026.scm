; Project Euler Problem 26.

; Find the value of d < 1000 for which 1/d contains the longest recurring
; fractional part.

; This would be better suited for Lua since it has hash tables built in.
; But this will force me to learn how to do the same thing in Scheme.

(load "nt.scm")

; Have 'state' contain '(numerator denominator current-digit max-digit)'?

(define (digits num den count max-count l)
  (if (> count max-count) (reverse l)
      (digits (modulo (* num 10) den) den (+ count 1) max-count
              (cons (quotient (* num 10) den) l))))

(define (numerators num den count max-count l)
  (if (> count max-count) (reverse l)
      (digits (modulo (* num 10) den) den (+ count 1) max-count
              (cons num l))))

(define len 35)
(newline)
; (digits 1  7 1 len '())
; (digits 1 13 1 len '())
; (digits 1 17 1 len '())
; (digits 1 23 1 len '())
; (digits 1 41 1 len '())
; (digits 1 43 1 len '())
(apply digits `(1 7 1 ,len ()))

; (numerators 1  7 1 len '())
; (numerators 1 13 1 len '())
; (numerators 1 17 1 len '())
; (numerators 1 23 1 len '())
; (numerators 1 41 1 len '())
; (numerators 1 43 1 len '())
