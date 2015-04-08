
; This provides for executing a gcf while maintaining its state.
(define (raw-gcf-recur a b AB)
  (cons (+ (* b (car AB)) (* a (cdr AB))) (car AB)))
(define (make-gcf-state k ak bk Ak Bk ak-func bk-func)
  (list k (cons ak bk) (cons Ak Bk) (cons ak-func bk-func)))

(define (raw-gcf-recur a b AB)
  (cons (+ (* b (car AB)) (* a (cdr AB))) (car AB)))
(define (gcf-state-init ak-func bk-func)
  (let ((b0 (bk-func 0)))
    (make-gcf-state 0 0 b0   ; k ak bk
      (cons b0 1) (cons 1 0) ; A B
      ak-func bk-func)))
(define (gcf-k g) (car g))
(define (gcf-a g) (car (cadr g)))
(define (gcf-b g) (cdr (cadr g)))
(define (gcf-A g) (car (caddr g)))
(define (gcf-B g) (cdr (caddr g)))
(define (gcf-ak-func g) (car (cadddr g)))
(define (gcf-bk-func g) (cdr (cadddr g)))
(define (gcf-step g)
  (let* ((kk (+ 1 (gcf-k g)))
         (ak ((gcf-ak-func g) kk))
         (bk ((gcf-bk-func g) kk))
         (Ak (raw-gcf-recur ak bk (gcf-A g)))
         (Bk (raw-gcf-recur ak bk (gcf-B g))))
    (make-gcf-state kk ak bk Ak Bk (gcf-ak-func g) (gcf-bk-func g))))

; Maintains state of a gcf calculation That attempts to
; converge on a number of digits in a particular radix.
(define (grc-make gcf radix C n)
  (list gcf radix C n))
(define (grc-init ak-func bk-func radix)
  (grc-make (gcf-state-init ak-func bk-func) radix 2 0))
(define (grc-g grc) (car grc))
(define (grc-r grc) (cadr grc))
(define (grc-C grc) (caddr grc))
(define (grc-n grc) (cadddr grc))

(define (grc-step-gcf grc)
  (grc-make (gcf-step (grc-g grc)) (grc-r grc) (grc-C grc) (grc-n grc)))
;; (define (grc-to-digits ak-func bk-func radix N)
;;   (let ((grc (grc-init ak-func bk-func radix)))
;;     (grc-step-to-n grc N)))

(define (sqr x) (* x x))
(define (tau-ak k) (if (= k 1) 8 (sqr (- k 1))))    ; a[k] are squares.
(define (tau-bk k) (if (= k 0) 0 (- (* 2 k) 1)))    ; b[k] are odd numbers.
(define (pi-ak  k) (if (= k 1) 4 (sqr (- k 1))))
(define (pi-bk  k) tau-bk)

(define tau-G (grc-init tau-ak tau-bk 16))
(define pi-G  (grc-init pi-ak  pi-bk 16))
(define (tau-step N)
  (if (>= (gcf-k (grc-g tau-G)) N)
      tau-G
      (begin
        (set! tau-G (grc-step-gcf tau-G))
        (display tau-G) (newline)
        (tau-step N))))
(tau-step 13)
