; Project Euler Problem 25.

; This is brute force. I should be able to calculate this using a calculator
; and the golden ratio (and its). But it is stunning how fast Scheme is at
; calculating these numbers.

(load "nt.scm")

(write (fib-with-value-at-least (expt 10 999)))   ; 10^999 has 1000 digits.
(display "\n")
; 4782
