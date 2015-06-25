(define (lwf) (load "write-file.sch"))

(define (write-file-list f xs)
  (if (null? xs) '()
      (begin
        (write (car xs) f)
        (write-file-list f (cdr xs)))))

(define (write-file filename s . opt)
  (let ((f (open-file filename "w")))
    (if (null? f) #f
        (begin
          (write s f)
          (write-file-list f opt)
          (close-port f)
          #t))))
