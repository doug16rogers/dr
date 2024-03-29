; (define (ltau) (load "tau.sch"))  ; Easy reloader.

(load "gcf3.sch")       ; For generalized continued fraction code.

; -----------------------------------------------------------------------------
; At https://en.wikipedia.org/wiki/Generalized_continued_fraction this is
; unattributed but said to converge quickly:
;   tau = 0 + 8 / (1 + 1^2 / (3 + 2^2 / (5 + 3^2 / (7 + ...
; My testing shows that it does indeed converge quickly. I get more than 1
; hexadecimal digit of precision for every 2 iterations.
(define (sqr n) (* n n))        ; Useful squaring function.
(define (tau-ak-func i) (if (= i 1) 8 (sqr (- i 1))))  ; a[k] are squares.
(define (tau-bk-func i) (- (* 2 i) 1))                 ; b[k] are odd numbers.
(define (tau-rational-by-gcf n) (gcf-rational-func n 0 tau-ak-func tau-bk-func))
(define (tau-float-by-gcf n) (exact->inexact (tau-rational-by-gcf-iterations n)))

(define (current-time-usec)
  (let ((ct (gettimeofday)))
    (+ (* 1000000 (car ct)) (cdr ct))))
(define (time-usec proc)
  (letrec ((start-usec (current-time-usec))
           (result (proc))
           (end-usec (current-time-usec)))
    (display (string-append (number->string (- end-usec start-usec)) " usec\n"))
    result))

; This generates the first 4096 digits of tau in base 16. It's quick:
(display "; Use this to peform a timed run:\n(load \"tau.sch\")\n(time-usec (lambda () (tau-to-digits-string 16 4096)))\n")
; (time-usec (lambda () (tau-to-digits-string 16 4096)))

; -----------------------------------------------------------------------------
; Calculate tau to N digits (in any base) by calculating tau-rational-by-gcf
; until the first N digits do not change.
;; (define (tau-rational-digits base digits)
;;   )
;;
;; To write the results to a file:
;; (call-with-output-file "tau-00010000.txt"
;;   (lambda (port) (display (tau-to-digits-string 16 65536) port)))
