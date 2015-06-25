; Run the test with:
;  guile -l assert.sch -l detect-cycle.sch -l detect-cycle-test.sch -c "(detect-cycle-test)"
; You might want to append this to the end of that to get rid of noise:
;     2>&1 | grep -v "compil" | grep -v "note: "

; 0->1, 1->2, (mu->mu+1,) ... mu+lam-1->mu
(define (easy-repeat-func lam mu) (lambda (x) (if (< x mu) (+ 1 x) (+ mu (modulo (+ 1 (- x mu)) lam)))))
(define (easy-repeat-7-3 x) (if (< x 3) (+ 1 x) (+ 3 (modulo (- x 2) 7))))

(define (detect-cycle-test)
  (assert-equal '(7 3) (detect-cycle-floyd easy-repeat-7-3 0))
  (assert-equal '(6 5) (detect-cycle (easy-repeat-func 6 5) 0))
  (assert-equal '(12 33) (detect-cycle (easy-repeat-func 12 33) 0))
  (assert-equal '(12 33) (detect-cycle (easy-repeat-func 12 33) 1))
  (assert-equal '(12 33) (detect-cycle (easy-repeat-func 12 33) 20))
  (display "ok") (newline))

