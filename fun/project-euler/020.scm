; Project Euler Problem 20

; There's probably a clever way to do this. Certainly factors of powers of 10
; could be removed. But I don't see anything else. One could easily calculate
; the sum mod 9 of the digits.

; I used Scheme to calculate 100! then sum the digits.

(define (fact n) (if (= 1 n) 1 (* n (fact (- n 1)))))
(fact 100)
; 9332621544394415268169923885626670049071596826438162146859296389
; 5217599993229915608941463976156518286253697920827223758251185210
; 916864000000000000000000000000

(define (sumdigits n) (if (= n 0) 0
                          (+ (modulo n 10) (sumdigits (floor (/ n 10))))))
(sumdigits (fact 100))
; 648
