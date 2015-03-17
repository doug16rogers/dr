
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

; If a fractional part exists and no fractional width is specified, this will
; be used as the width of the fractional part.
(define kDefaultFractionalWidth 8)

; Must be non-negative number, but may be rational or floating point.
(define (to-hex-formatted number unit-digits fractional-digits)
  (let* ((whole       (inexact->exact (floor number)))
         (fraction    (- number whole))
         (frac-digits (if (and (< fractional-digits 0) (not (= 0 fraction)))
                          kDefaultFractionalWidth fractional-digits)))
    (if (< frac-digits 0)
        (to-hex-positive-integer whole unit-digits)
        (string-append (to-hex-positive-integer whole unit-digits)
                       "."
                       (to-hex-fraction fraction frac-digits)))))

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
        (to-hex-formatted n unit-digits frac-digits))))

