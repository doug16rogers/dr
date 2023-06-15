; Run the test with:
;  guile -l assert.sch -l ack.sch -l ack-test.sch -c "(ack-test)"
; You might want to append this to the end of that to get rid of noise:
;     2>&1 | grep -v "compil" | grep -v "note: "

(define (ack-test)
  ;; Short-cut Ackermann functions for small values of m:
  (assert-= 1 (ack0 0))
  (assert-= 2 (ack0 1))
  (assert-= 3 (ack0 2))
  (assert-= 4 (ack0 3))

  (assert-= 2 (ack1 0))
  (assert-= 3 (ack1 1))
  (assert-= 4 (ack1 2))
  (assert-= 5 (ack1 3))

  (assert-= 3 (ack2 0))
  (assert-= 5 (ack2 1))
  (assert-= 7 (ack2 2))
  (assert-= 9 (ack2 3))

  (assert-=  5 (ack3 0))
  (assert-= 13 (ack3 1))
  (assert-= 29 (ack3 2))
  (assert-= 61 (ack3 3))

  (assert-= 13 (ack4 0))
  (assert-= 65533 (ack4 1))

  ;; General Ackermann functions:
  (assert-= 1 (ack 0 0))
  (assert-= 2 (ack 0 1))
  (assert-= 3 (ack 0 2))
  (assert-= 4 (ack 0 3))

  (assert-= 2 (ack 1 0))
  (assert-= 3 (ack 1 1))
  (assert-= 4 (ack 1 2))
  (assert-= 5 (ack 1 3))

  (assert-= 3 (ack 2 0))
  (assert-= 5 (ack 2 1))
  (assert-= 7 (ack 2 2))
  (assert-= 9 (ack 2 3))

  (assert-=  5 (ack 3 0))
  (assert-= 13 (ack 3 1))
  (assert-= 29 (ack 3 2))
  (assert-= 61 (ack 3 3))

  (assert-= 13 (ack 4 0))
  (display "The calculation of (ack 4 1) takes up to a minute...") (newline)
  (assert-= 65533 (ack 4 1))

  (display "ok") (newline))
