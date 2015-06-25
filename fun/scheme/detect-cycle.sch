; A finite group with a generator must eventually fall into a cycle of
; repeating values. Think of the cyclical part as a circle. The starting
; value might not be on the circle so the sequence leading to the circle
; looks like a tail. Hence the rho shape that gives some of the detectors
; their names.

; Traditionally the cycle length is called lambda - a possibly confusing term
; in Lisp. The first value in the group that is repeated is called mu.

; For example, a group that consists of values mod 13 with
; self-multiplication (squaring) as the operator would form the following
; cycles:

; The progression below was generated using (print-cycle-sqr-mod 19)
; N = 19; S = [0 .. N-1], F(x) = (x * x) mod N
; Start -> progression                             Lambda    Mu
; -----------------------------------------------  ------  ------
; 0 -> [0] (1)                                        1       1
; 1 -> [1] (1)                                        1       1
; 2 -> 4 -> 16 -> 9 -> 5 -> 6 -> 17 -> [4] (6)        6       4
; 3 -> 9 -> 5 -> 6 -> 17 -> 4 -> 16 -> [9] (6)        6       9
; 4 -> 16 -> 9 -> 5 -> 6 -> 17 -> [4] (6)             6       4
; 5 -> 6 -> 17 -> 4 -> 16 -> 9 -> [5] (6)             6       5
; 6 -> 17 -> 4 -> 16 -> 9 -> 5 -> [6] (6)             6       6
; 7 -> 11 -> [7] (2)                                  2       7
; 8 -> 7 -> 11 -> [7] (2)                             2       7
; 9 -> 5 -> 6 -> 17 -> 4 -> 16 -> [9] (6)             6       9
; 10 -> 5 -> 6 -> 17 -> 4 -> 16 -> 9 -> [5] (6)       6       5
; 11 -> 7 -> [11] (2)                                 2      11
; 12 -> 11 -> 7 -> [11] (2)                           2      11
; 13 -> 17 -> 4 -> 16 -> 9 -> 5 -> 6 -> [17] (6)      6      17
; 14 -> 6 -> 17 -> 4 -> 16 -> 9 -> 5 -> [6] (6)       6       6
; 15 -> 16 -> 9 -> 5 -> 6 -> 17 -> 4 -> [16] (6)      6      16
; 16 -> 9 -> 5 -> 6 -> 17 -> 4 -> [16] (6)            6      16
; 17 -> 4 -> 16 -> 9 -> 5 -> 6 -> [17] (6)            6      17
; 18 -> 1 -> [1] (1)                                  1       1

; Each cycle detector below returns a '(lambda mu) list where lambda is the
; cycle length and mu is the first value in set to be repeated.

; These are from: https://en.wikipedia.org/wiki/Cycle_detection

; Floyd's cycle detection algorithm.
(define (floyd-double-finder f tortoise hare)
  (if (= tortoise hare) tortoise
      (floyd-double-finder f (f tortoise) (f (f hare)))))
(define (floyd-mu-finder f tortoise hare)
  (if (= tortoise hare) tortoise
      (floyd-mu-finder f (f tortoise) (f hare))))
(define (floyd-lam-finder lam f tortoise hare)
  (if (= tortoise hare) lam
      (floyd-lam-finder (+ 1 lam) f tortoise (f hare))))

(define (detect-cycle-floyd f x0)
  (let* ((tortoise (floyd-double-finder f (f x0) (f (f x0))))
         (mu       (floyd-mu-finder f x0 tortoise))
         (lam      (floyd-lam-finder 1 f mu (f mu))))
    (list lam mu)))

(define detect-cycle detect-cycle-floyd)

; -----------------------------------------------------------------------------
; Below is some code to generate the progressions like the example above.
(define (print-cycle-starting-at a USED F)
  (if (member a USED)
      (begin
        (display "[") (display a) (display "]")
        (display " (") (display (car (detect-cycle F a))) (display ")")
        (newline))
      (let ((Fa (F a)))
        (display a) (display " -> ")
        (print-cycle-starting-at Fa (cons a USED) F))))

(define (print-next-cycle L F)
  (if (null? L) '()
      (begin
        (print-cycle-starting-at (car L) '() F)
        (print-next-cycle (cdr L) F))))

(define (print-cycle S F)
  (if (null? S) '()
      (print-next-cycle S F)))

; -----------------------------------------------------------------------------
; Test functions for suppling stuff to (print-next-cycle).
(define (range-list L min max)
  (if (< max min) L
      (range-list (cons max L) min (- max 1))))

(define (range min . max-arg)
  (if (null? max-arg)
      (range-list '() 1 min)
      (range-list '() min (car max-arg))))

(define (print-cycle-sqr-mod N)
  (print-cycle (range 0 (- N 1)) (lambda (n) (modulo (* n n) N))))
