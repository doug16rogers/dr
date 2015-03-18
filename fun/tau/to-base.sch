; (define (ltb) (load "to-base.sch"))

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
(define (to-base-fraction base f digits)
  (if (zero? digits) ""
      (letrec ((times-base (* base f))
               (unit       (floor times-base)))
        (string-append
         (base-digit-string (inexact->exact unit))
         (to-base-fraction base (- times-base unit) (- digits 1))))))

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
(define (to-hex n . digits)
  (apply to-base 16 n digits))
