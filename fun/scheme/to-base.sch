(define (ltb) (load "to-base.sch"))

; -----------------------------------------------------------------------------
; There are probably better Scheme ways to do this but it was educational to
; write my own. The values for digits 0x3E and 0x3F are according to the
; original base64 RFC (and many follow-ons). Not that it matters; this
; encoding starts with numbers first (as it should) whereas base64 uses upper
; case at 0x00, then lower case then letters then '+' and '/'.
(define kBaseDigits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/")
(define (base-digit-char n) (string-ref kBaseDigits n))
(define (base-digit-string n) (string (base-digit-char n)))
(define (to-base-positive-integer-widthless-after-first base x)
  (if (zero? x) ""
      (let ((digit (modulo x base)))
        (string-append (to-base-positive-integer-widthless-after-first base (/ (- x digit) base))
                       (base-digit-string digit)))))
(define (to-base-positive-integer-widthless base x)
  (if (zero? x) "0" (to-base-positive-integer-widthless-after-first base x)))
(define (to-base-positive-integer base x width)
  (if (< width 0)
      (to-base-positive-integer-widthless base x)
      (if (zero? width) ""
          (let ((digit (modulo x base)))
            (string-append (to-base-positive-integer base (/ (- x digit) base) (- width 1))
                           (base-digit-string digit))))))
(define (to-base-integer base x width)
  (if (< x 0)
      (string-append "-" (to-base-positive-integer base (- 0 x) width))
      (to-base-positive-integer base x width)))

; Calculates the fractional part by appending to a string. This takes
; 82 seconds for 4000 digits of (tau-rational 65536).
(define (to-base-fraction-by-string base f digits)
  (if (zero? digits) ""
      (letrec ((times-base (* base f))
               (unit       (floor times-base)))
        (string-append
         (base-digit-string (inexact->exact unit))
         (to-base-fraction base (- times-base unit) (- digits 1))))))

; Calculates the fractional part by calculating a list of characters and
; turning that into a string. No faster. 82 seconds for 4000 digits of
; (tau-rational 65536).
(define (to-base-fraction-list base f digits)
  (if (zero? digits) '()
      (letrec ((times-base (* base f))
               (unit       (floor times-base)))
        (cons (base-digit-char (inexact->exact unit))
         (to-base-fraction-list base (- times-base unit) (- digits 1))))))
(define (to-base-fraction-from-list base f digits)
  (list->string (to-base-fraction-list base f digits)))

; Calculates the fractional part by allocating a string then modifying it in
; place using a tail-recursive call. Surprisingly this wasn't faster either.
; It takes 82 seconds for 4000 digits of (tau-rational 65536). The numerator
; must be < denominator.
(define (to-base-fraction-by-char base f s k digits)
  (if (>= k digits) s
      (letrec ((times-base (* base f))
               (unit       (floor times-base)))
        (string-set! s k (base-digit-char (inexact->exact unit)))
        (to-base-fraction-by-char base (- times-base unit) s (+ k 1) digits))))
(define (to-base-fraction-fixed-string base f digits)
  (to-base-fraction-by-char base f (make-string digits) 0 digits))

; This method is for rationals only. It breaks the numerator and denominator
; apart so it can avoid the gcd calculation in keeping them relatively prime.
; THIS IS MUCH, MUCH FASTER! I can do 40000 digits in just a second or so.
; Note that let-values requires rnrs.
(import (rnrs))         ; Or (use-modules (srfi srfi-11)).
(define (to-base-fraction-rational-nd base num den s k digits)
  (if (>= k digits) s
      (letrec ((times-base (* base num))
               (qr         (let-values (((q r) (floor/ times-base den))) (list q r))))
        (string-set! s k (base-digit-char (car qr)))
        (to-base-fraction-rational-nd base (cadr qr) den s (+ k 1) digits))))
(define (to-base-fraction-rational base f digits)
  (to-base-fraction-rational-nd base
                                (inexact->exact (numerator f))  ; ->exact allows use of floating point f.
                                (inexact->exact (denominator f))
                                (make-string digits) 0 digits))

; -----------------------------------------------------------------------------
; (define to-base-fraction to-base-fraction-fixed-string)
(define to-base-fraction to-base-fraction-rational)

; Must be non-negative number, but may be rational or floating point.
(define (to-base-formatted base number unit-digits fractional-digits)
  (let* ((whole       (inexact->exact (floor number)))
         (fraction    (- number whole))
         (frac-digits (if (and (< fractional-digits 0) (not (= 0 fraction)))
                          kDefaultBaseFractionalWidth fractional-digits)))
    (if (< frac-digits 0)
        (to-base-positive-integer base whole unit-digits)
        (string-append (to-base-positive-integer base whole unit-digits)
                       "."
                       (to-base-fraction base fraction frac-digits)))))

; -----------------------------------------------------------------------------
; Below is the public API.
; -----------------------------------------------------------------------------

; If a fractional part exists and no fractional width is specified, this will
; be used as the width of the fractional part.
(define kDefaultBaseFractionalWidth 8)

; 'digits' is optional width-for-integer and width-for-fraction.
; Negative numbers are treated just like positive, but will have
; an additional '-' in the front. That is, the '-' is *not*
; included in any of the widths.
; This does *not* perform rounding.
; For example, (to-base 8/3 2 3) yields "02.AAA".
(define (to-base base n . digits)
;  (assert (< base string-length kBaseDigits))
  (if (< n 0)
      (string-append "-" (apply to-base base (- 0 n) digits))
      (let ((unit-digits (if (null? digits) -1 (car digits)))
            (frac-digits (if (null? digits) -1 (if (null? (cdr digits)) -1 (car (cdr digits))))))
        (to-base-formatted base n unit-digits frac-digits))))

; Uses to-base for hexadecimal (base 16).
; @param n - the number to convert.
; @param digits - optional number of digits left of the radix point and
; number of digits right of the radix points.
(define (to-hex n . digits)
  (apply to-base 16 n digits))
