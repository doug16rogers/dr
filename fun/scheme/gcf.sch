; -----------------------------------------------------------------------------
; General continued fraction evaluation according to
; https://en.wikipedia.org/wiki/Generalized_continued_fraction.

; Evaluate a single recurrence value for either A or B and return its new
; state. AB represents either A or B (since they are calculated using the same
; formula) and should have a car that is AB[n-1] and a cdr that is AB[n-2].
; Return (AB[n] . AB[n-1]) = (b*AB[n-1] + a*AB[n-2] . AB[n-1]).
(define (gcf-recurrence a b AB)
  (cons (+ (* b (car AB)) (* a (cdr AB)))
        (car AB)))

; Calculate the kth step (and all later steps to k=n) in the recurrence
; formula for the general continued fraction described in gcf-func below,
; using a tail-recursive method to avoid blowing up the stack.
(define (gcf-step k n ak-func bk-func A B)
  (if (> k n) (cons A B)
      (let ((ak (ak-func k))
            (bk (bk-func k)))
        (gcf-step (+ k 1) n ak-func bk-func (gcf-recurrence ak bk A) (gcf-recurrence ak bk B)))))

; Evaluate the generalized continued fraction b0 + a1 / (b1 + a2 / (b2 + ...
; to n terms, where each a[k] and b[k] are (other than b0) are evaluated
; using (ak-func k) and (bk-func k).
(define (gcf-func n b0 ak-func bk-func)
  (gcf-step 1 n ak-func bk-func (cons b0 1) '(1 . 0)))

(define (gcf-rational-func n b0 ak-func bk-func)
  (let ((AB (gcf-func n b0 ak-func bk-func)))
    (/ (car (car AB)) (car (cdr AB)))))     ; Result is A[n] / B[n]

