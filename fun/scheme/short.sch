(define-syntax ? (syntax-rules () ((_ c ...) (if c ...))))
(define-syntax \ (syntax-rules () ((_ a ...) (lambda a ...))))
(define-syntax @ (syntax-rules () ((_ v ...) (define v ...))))

; Curry.
(@ (\\.. f arg-list) (\ (x) (apply f (append arg-list (list x)))))
(@ (\\ f . args) (\\.. f args))

(@ (append-atom lis a) (append lis (cons a '())))
(@ (rev ls) (? (null? ls) '() (? (null? (cdr ls)) ls (append-atom (rev (cdr ls)) (car ls)))))
;; (rev '()) (rev '(1)) (rev '(1 2 3 4 5))

;; Need to change order to make currying better:
(@ (foldl acc f lis) (? (null? lis) acc (foldl (f acc (car lis)) f (cdr lis))))
(@ (foldr lis f acc) (? (null? lis) acc (f (car lis) (foldr (cdr lis) f acc))))   ;; Is this correct?
;; (foldl 0 - '(4 5 6))
;; (foldr '(4 5 6) - 0)

;; Optional arguments (http://computer-programming-forum.com/40-scheme/00361f09f72cf87c.htm):
(define (take-optional x y . z)
  (cond
   [(null? z)       "received two arguments"]
   [(null? (cdr z)) "received three arguments"]
   [else            "received the wrong number of arguments"]))

