; Provides assertions that will emit a backtrace and exit the program upon
; failiure.

(define (assert condition . failure-message)
  (let ((msg (if (or (null? failure-message) (null? (car failure-message)))
                 "assertion failed"
                 (car failure-message))))
    (if (not (condition))
        (begin
          (backtrace)
          (display "**** ")
          (display msg)
          (display " ****")
          (newline)
          (exit)))))

(define (equality-assert equality-check proper actual)
  (if (equality-check proper actual) #t
      (begin
        (backtrace)
        (display "**** assertion failed ****") (newline)
        (display "proper=") (write proper) (newline)
        (display "actual=") (write actual) (newline)
        (exit))))

(define (assert-=     proper actual) (equality-assert =      proper actual))
(define (assert-eq    proper actual) (equality-assert eq?    proper actual))
(define (assert-equal proper actual) (equality-assert equal? proper actual))
