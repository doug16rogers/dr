; Run the test with:
;  guile -l assert.sch -l to-base.sch -l to-base-test.sch -c "(to-base-test)"
; You might want to append this to the end of that to get rid of noise:
;     2>&1 | grep -v "compil" | grep -v "note: "

(define (to-base-test)
; Default fractional width (kDefaultBaseFractionalWidth) should be 8.
  (assert-= 8 kDefaultBaseFractionalWidth)
  (assert-equal "2.AAAAAAAA" (to-hex 8/3))

  ; Simple conversions with integers.
  (assert-equal "0" (to-base 8 0))
  (assert-equal "0" (to-base 32 0))
  (assert-equal "1" (to-base 8 1))
  (assert-equal "-1" (to-base 8 -1))
  (assert-equal "145" (to-base 8 101))
  (assert-equal "-145" (to-base 8 -101))
  (assert-equal "1" (to-base 32 1))
  (assert-equal "-1" (to-base 32 -1))
  (assert-equal "35" (to-base 32 101))
  (assert-equal "-35" (to-base 32 -101))
  (assert-equal "1111111" (to-base 2 127))
  (assert-equal "11201" (to-base 3 127))
  (assert-equal "3V" (to-base 32 127))    ; V=0x56 - 0x41 = 0x15 + 0x0A = 0x1F.
  (assert-equal "1/" (to-base 64 127))

  ; With units width specified.
  (assert-equal "000" (to-base 2 0 3))
  (assert-equal "001" (to-base 2 1 3))
  (assert-equal "001" (to-base 24 1 3))
  (assert-equal "111" (to-base 2 15 3))   ; Only lsds are shown - least significant digits.
  (assert-equal    "787" (to-base 24 1234567 3))
  (assert-equal   "H787" (to-base 24 1234567 4))
  (assert-equal  "3H787" (to-base 24 1234567 5))
  (assert-equal "03H787" (to-base 24 1234567 6))
  (assert-equal    "-787" (to-base 24 -1234567 3))
  (assert-equal   "-H787" (to-base 24 -1234567 4))
  (assert-equal  "-3H787" (to-base 24 -1234567 5))
  (assert-equal "-03H787" (to-base 24 -1234567 6))

  ; With digits after the radix point.
  (assert-equal "6.487ED511" (to-base 16 6.283185307179582))
  (assert-equal "6.487ED511" (to-hex 6.283185307179582))
  (assert-equal "6.487ED5" (to-base 16 6.283185307179582 -1 6))
  (assert-equal "06.487ED5" (to-base 16 6.283185307179582 2 6))

  (display "ok") (newline))
