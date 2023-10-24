;; (define (le) (load "e.sch"))  ; Easy reloader.

(load "gcf3.sch")       ; For generalized continued fraction code.

;; -----------------------------------------------------------------------------
;; https://mathworld.wolfram.com/eContinuedFraction.html
;; Two versions are shown. The simple continued fraction is:
;;    b[n] = [2; 1, 2, 1,  1, 4, 1,  1, 6, 1,  1, 8, 1, ...]
;; The regular continued fraction by Wall [1948] has a cool pattern:
;;    a[n] = [1, 1, 2, 3, 4, 5, 6, ...]
;;    b[n] = [2; 1, 2, 3, 4, 5, 6, ...]
(define (e-an n) (if (= n 1) 1 (- n 1)))
(define (e-bn n) (if (= n 0) 2 n))
(define (e-rational-by-gcf-iterations n) (gcf-rational-func n 0 e-an e-bn))
(define (e-float-by-gcf n) (exact->inexact (e-rational-by-gcf-iterations n)))

(define (e-to-digits radix N)
  (gcf-to-digits e-an e-bn radix N))

(define (e-to-digits-string radix N)
  (gcf-to-digits-string e-an e-bn radix N))

(define (current-time-usec)
  (let ((ct (gettimeofday)))
    (+ (* 1000000 (car ct)) (cdr ct))))

(define (time-usec proc)
  (letrec ((start-usec (current-time-usec))
           (result (proc))
           (end-usec (current-time-usec)))
    (display (string-append (number->string (- end-usec start-usec)) " usec\n"))
    result))

;; This generates the first 4096 digits of e in base 16.
(display "; Use this to peform a timed run:\n(load \"e.sch\")\n(time-usec (lambda () (e-to-digits-string 16 4096)))\n")
;; (time-usec (lambda () (e-to-digits-string 16 4096)))

;;
;; To write the results to a file:
;; (call-with-output-file "e-00010000.txt"
;;   (lambda (port) (display (e-to-digits-string 16 65536) port)))
