(define (ltau) (load "tau.sch"))  ; Easy reloader.

(load "gcf.sch")
(load "to-hex.sch")

; -----------------------------------------------------------------------------
; At https://en.wikipedia.org/wiki/Generalized_continued_fraction this is
; unattributed but said to converge quickly:
;   tau = 0 + 8 / (1 + 1^2 / (3 + 2^2 / (5 + 3^2 / (7 + ...
; My testing shows that it does indeed converge quickly. I get more than 1
; hexadecimal digit of precision for every 2 iterations.
(define (sqr n) (* n n))        ; Useful squaring function.
(define (tau-ak-func i) (if (= i 1) 8 (sqr (- i 1))))  ; a[k] are squares.
(define (tau-bk-func i) (- (* 2 i) 1))                 ; b[k] are odd numbers.
(define (tau-rational n) (gcf-rational-func n 0 tau-ak-func tau-bk-func))
(define (tau-float n) (exact->inexact (tau-rational n)))

; This shows the first 324 hex digits of tau (guile < tau.sch):
(to-hex (tau-rational 512) 1 324)
