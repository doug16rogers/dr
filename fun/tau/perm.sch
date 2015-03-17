; The definition of (permutations size elements) is from:
; http://stackoverflow.com/questions/3179931/how-do-i-generate-all-permutations-of-certain-size-with-repetitions-in-scheme

; Supporting code is from SICP:
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html

(define nil '())

; This is exactly how I think of it. Cool. But they apply it to lists.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; (flatmap) doesn't exist in guile so I found this one in SICP:
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; TEXT FROM StackOverflow:
; --------
; So basically what you want to do is, given R = (permutations n elements),
; you can get (permutations (+ n 1) elements) by taking each permutation P in
; R, and then for each element E in ELEMENTS, adjoin E to P to create a new
; permutation, and collect a list of them. And we can do this with nested
; MAPs:
(define (permutations size elements)
  (if (zero? size)
      '(())
      (flatmap (lambda (p)            ; For each permutation we already have:
                 (map (lambda (e)     ;   For each element in the set:
                        (cons e p))   ;     Add the element to the perm'n.
                      elements))
               (permutations (- size 1) elements))))
; I'm using FLATMAP for the outer mapping, because the inner MAP creates
; lists of new permutations, and we have to append those lists together to
; create the one big flat list of permutations that we want.
; --------
