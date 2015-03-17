(define (lold) (load "old-cf-pi-tau.sch"))  ; Easy reloader.

; NOTE!!!!!! ******************************************************************
;    The earlier algorithms are left-recursive and so are very slow. Also,
;    some of the continued fraction expressions are very VERY slow to
;    converge. I've pulled the good stuff out into the non-old files.

; -----------------------------------------------------------------------------
; Simple continued fractions.
; https://en.wikipedia.org/wiki/Continued_fraction

(define (cf-hk-next as hks)
  (if (null? as) hks
      (cf-hk-next (cdr as)
                  (cons (+ (* (car as) (car hks)) (cdr hks))
                        (car hks)))))

(define (cf-calc-hn as) (cf-hk-next as '(1 . 0)))
(define (cf-calc-kn as) (cf-hk-next as '(0 . 1)))

(define (cf-rational as)
  (/ (car (cf-calc-hn as)) (car (cf-calc-kn as))))

(define (cf-float as)
  (exact->inexact (cf-rational as)))

; -----------------------------------------------------------------------------
; The square root of two.
; https://en.wikipedia.org/wiki/Solving_quadratic_equations_with_continued_fractions#A_simple_example

; This actually converges fairly quickly.
(define (cf-sqrt2 n) (cf-float (cons 1 (make-list n 2))))

; -----------------------------------------------------------------------------
; From http://www.harremoes.dk/Peter/Undervis/Turnpage/Turnpage1.html, the
; continued fraction for tau = [6; 3, 1, 1, 7, 2, 146, 3, 6, ...]

; From http://mathworld.wolfram.com/PiContinuedFraction.html, the continued
; fraction for pi = [3; 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1, 1, 2, 2, 2, 2, ...]

; For any continued fraction [a0; a1, a2, ...], the value of the cf may be
; estimated at term n by h[n] / k[n] where:
;   h[n] = (a[n] * h[n-1]) + h[n-2];  h[-1] = 1, h[-2] = 0
;   k[n] = (a[n] * k[n-1]) + k[n-2];  k[-1] = 0, k[-2] = 1
; Beautiful.

(define tau-as-cf '(6 3 1 1 7 2 146 3 6))
(define pi-as-cf  '(3 7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2 2 2))

; -----------------------------------------------------------------------------
; From https://en.wikipedia.org/wiki/Generalized_continued_fraction:

; K is either (A[n-1] . A[n-2]) or (B[n-1] . B[n-2]).
(define (gcf-AB-one a b K)
  (cons (+ (* b (car K)) (* a (cdr K))) (car K)))

; ABs is ((A[n-1] . A[n-2]) . (B[n-1] . B[n-2])).
; as are the numerators.
; bs are the denominators, starting AFTER the integer part.
; That's weird, but that's tradition.
(define (gcf-AB-next as bs ABs)
  (if (null? as) ABs
      (gcf-AB-next (cdr as) (cdr bs)
                   (cons (gcf-AB-one (car as) (car bs) (car ABs))
                         (gcf-AB-one (car as) (car bs) (cdr ABs))))))

; as are the numerators.
; bs are the denominators, starting WITH the integer part.
(define (gcf-rational as bs)
  (let ((AB (gcf-AB-next as (cdr bs) (cons (cons (car bs) 1) '(1 . 0)))))
    (/ (car (car AB)) (car (cdr AB)))))

(define (gcf-float as bs)
  (exact->inexact (gcf-rational as bs)))

; -----------------------------------------------------------------------------
; There's Brouncker non-simple expansion (from same wolfram.com page):
;   4 / pi = 1 + (1^2) / (2 + 3^2 / (2 + 5^2 / (2 + 7^2 / (2 + 9^2 / (...

; ber = Brouncker expansion recursive.
(define ber-4-over-pi-a '(  1 9 25 49 81))   ; a[1] is the first numerator.
(define ber-4-over-pi-b '(1 2 2  2  2  2))   ; b[0] is the integer part.
(define (sqr n) (* n n))
(define (sqr2n+1 n) (sqr (+ (* 2 n) 1)))
(define (sqr2n-1 n) (sqr (- (* 2 n) 1)))
(define (append-list l a)
  (if (null? l) (list a)
      (cons (car l) (append-list (cdr l) a))))
(define (index-list-i i n)
  (if (> i n)
      '()
      (cons i (index-list-i (+ i 1) n))))
(define (index-list n) (index-list-i 1 n))
(define (ber-4-over-pi-as n) (if (zero? n) '() (map sqr2n-1 (index-list n))))
(define (ber-4-over-pi-bs n) (if (zero? n) '(1) (cons 1 (make-list n 2))))
(define (ber-4-over-pi-rational n)
  (gcf-rational (ber-4-over-pi-as n) (ber-4-over-pi-bs n)))
(define (ber-4-over-pi-float n)
  (exact->inexact (ber-4-over-pi-rational n)))
; This converges VERY VERY slowly. (ber-pi-float 4000) is only 4 digits!
; And (ber-pi-float 40000) causes a VM stack overflow!
; Clearly this needs to be reimplemented in a non-or-tail-recursive fashion.
(define (ber-pi-rational n) (/ 4 (ber-4-over-pi-rational n)))
(define (ber-pi-float n) (/ 4 (ber-4-over-pi-float n)))

; -----------------------------------------------------------------------------
; Re-implemented general continued fraction that is tail-recursive.

(define (gcftr-single a b K)
  (cons (+ (* b (car K)) (* a (cdr K)))
        (car K)))
(define (gcftr-step-i i n an-func bn-func A B)
  (if (> i n) (cons A B)
      (let ((an (an-func i))
            (bn (bn-func i)))
        (gcftr-step-i (+ i 1) n an-func bn-func (gcftr-single an bn A) (gcftr-single an bn B)))))

; n is the number of terms to evaluate.
; b0 is the integer term, b[0].
; an-func is a function to return a[n] (n = 1, 2, 3, ...).
; bn-func is a function to return b[n] (n = 1, 2, 3, ...).
; bs are the denominators, starting WITH the integer part.
(define (gcftr-func n b0 an-func bn-func)
  (gcftr-step-i 1 n an-func bn-func (cons b0 1) '(1 . 0)))

(define (gcftr-rational-func n b0 an-func bn-func)
  (let ((AB (gcftr-func n b0 an-func bn-func)))
    (/ (car (car AB)) (car (cdr AB)))))

; -----------------------------------------------------------------------------
; Brouncker's expansion, tail-recursive.
; Wow. It's a bit faster now and it doesn't kill the stack. I could remove the
; function calls for bn I suppose. But this is how VERY VERY VERY slow it is
; to converge:
; (benr-pi-float  40000) => 3.141617652964805 
; (benr-pi-float 100000) => 3.141602653489794

(define (benr-rational n)
  (gcftr-rational-func n 1 sqr2n-1 (lambda (x) 2)))
(define (benr-float n)
  (exact->inexact (benr-rational n)))
(define (benr-pi-rational n) (/ 4 (benr-rational n)))
(define (benr-pi-float n)    (exact->inexact (benr-pi-rational n)))

; Clearly I need a better method!
; I need to come up with my own.

; -----------------------------------------------------------------------------
; From the Generalized_continued_fraction wiki page, lau Nilakantha Somayaji:
;   pi = 3 + 1^2 / (6 + 3^2 / (6 + 5^2 / (6 + 7^2 / (6 + ...
(define (ns-pi-rational n)
  (gcftr-rational-func n 3 sqr2n-1 (lambda (x) 6)))
(define (ns-pi-float n) (exact->inexact (ns-pi-rational n)))

; -----------------------------------------------------------------------------
; This one is unattributed in the wikipedia page but said to converge quickly:
;   pi = 0 + 4 / (1 + 1^2 / (3 + 2^2 / (5 + 3^2 / (7 + ...
(define (ua-pi-an-func  i) (if (= i 1) 4 (sqr (- i 1))))
(define (ua-tau-an-func i) (if (= i 1) 8 (sqr (- i 1))))
(define (ua-bn-func i) (- (* 2 i) 1))
(define (ua-pi-rational n) (gcftr-rational-func n 0 ua-pi-an-func ua-bn-func))
(define (ua-pi-float n) (exact->inexact (ua-pi-rational n)))
(define (ua-tau-rational n) (gcftr-rational-func n 0 ua-tau-an-func ua-bn-func))
(define (ua-tau-float n) (exact->inexact (ua-tau-rational n)))

; -----------------------------------------------------------------------------
; For the best versions:
(define pi-rational  ua-pi-rational)
(define pi-float     ua-pi-float)
(define tau-rational ua-tau-rational)
(define tau-float    ua-tau-float)

; -----------------------------------------------------------------------------
; To print in hexadecimal. There are probably better Scheme ways to do this
; but it was educational to write my own.
(define kHexDigits "0123456789ABCDEF")
(define (hex-digit-char n) (string-ref kHexDigits n))
(define (hex-digit-string n) (string (hex-digit-char n)))
(define (to-hex-positive-integer-widthless-after-first x)
  (if (zero? x) ""
      (let ((digit (modulo x 16)))
        (string-append (to-hex-positive-integer-widthless-after-first (/ (- x digit) 16))
                       (hex-digit-string digit)))))
(define (to-hex-positive-integer-widthless x)
  (if (zero? x) "0" (to-hex-positive-integer-widthless-after-first x)))
(define (to-hex-positive-integer x width)
  (if (< width 0)
      (to-hex-positive-integer-widthless x)
      (if (zero? width) ""
          (let ((digit (modulo x 16)))
            (string-append (to-hex-positive-integer (/ (- x digit) 16) (- width 1))
                           (hex-digit-string digit))))))
(define (to-hex-integer x width)
  (if (< x 0)
      (string-append "-" (to-hex-positive-integer (- 0 x) width))
      (to-hex-positive-integer x width)))
(define (to-hex-fraction f digits)
  (if (zero? digits) ""
      (letrec ((times16 (* 16 f))
               (unit    (floor times16)))
        (string-append
         (hex-digit-string (inexact->exact unit))
         (to-hex-fraction (- times16 unit) (- digits 1))))))
(define (to-hex-formatted unit-digits fractional-digits number)
  (if (< number 0)
      (string-append "-" (to-hex-formatted (- 0 number) unit-digits fractional-digits))
      (let ((whole (inexact->exact (floor number))))
        (if (< fractional-digits 0)
            (to-hex-positive-integer whole unit-digits)
            (string-append
             (to-hex-positive-integer whole unit-digits)
             "."
             (to-hex-fraction (- number whole) fractional-digits))))))

; Return a string that is the hexadecimal expansion of 'n'
; with the given number of digits.
; 'digits' is optional width-for-integer and width-for-fraction.
; Negative numbers are treated just like positive, but will have
; an additional '-' in the front. That is, the '-' is *not*
; included in any of the widths.
; This does *not* perform rounding.
; For example, (to-hex 8/3 2 3) yields "02.AAA".
(define (to-hex n . digits)
  (if (< n 0)
      (string-append "-" (apply to-hex (- 0 n) digits))
      (let ((unit-digits (if (null? digits) -1 (car digits)))
            (frac-digits (if (null? digits) -1 (if (null? (cdr digits)) -1 (car (cdr digits))))))
        (to-hex-formatted unit-digits frac-digits n))))

; This shows the first 324 hex digits of tau:
; (to-hex (ua-tau-rational 512) 1 324)
; (to-hex (ua-tau-rational 513) 1 324)
