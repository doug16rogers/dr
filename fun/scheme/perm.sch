;; ----------------------------------------------------------------------------
;; Generate a list of permutations of the given @p elements.
;;
;; @param elements - list of elements to be permuted.
;;
;; @return a list of permutations of @p elements, each being a list.
;;
(define (perms elements)
  (if (null? elements) '(())
      (map-each-rotation-over-sub-perms (rotations elements))))

;; ----------------------------------------------------------------------------
;; Given a list of rotations of a set of elements, return a list - for each
;; rotation - of the result of appending after its first element all the
;; permutations of the rest of the elements in that rotation.
;;
;; @param rotations - list of rotations of a set of elements.
;;
;; @return ultimately, a list of all the permutations of all elements in any
;; of the @p rotations.
;;
(define (map-each-rotation-over-sub-perms rotations)
  (if (null? rotations) '()
      (letrec ((rotation (car rotations))
               (first-item (list (car rotation)))
               (append-to-first-item (lambda (sub-perm) (append first-item sub-perm)))
               (sub-perms (perms (cdr rotation))))
        (append (map append-to-first-item sub-perms)
                (map-each-rotation-over-sub-perms (cdr rotations))))))

;; ----------------------------------------------------------------------------
;; Create a list of rotations of the list @p elements.
;;
;; Note: This uses an optional parameter to track the rotated elements.
;;
;; @param elements - list of elements to be rotated.
;;
;; @param opt-rotated - optional list of elements already rotated out of @p
;; elements. This is used in the recursive call but is not needed in an
;; initial call.
;;
;; @return a list of all the rotations of @p elements.
;;
(define (rotations elements . opt-rotated)
  (if (null? elements) '()
      (let ((rotated (if (null? opt-rotated) '() (car opt-rotated))))
        (append (list (append elements rotated))
                (rotations (cdr elements) (append rotated (list (car elements))))))))

;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; Below are some experiments in trying to make the permtation code shorter
;; or clearer - based on more abstract primitives.
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; First are some utilities.
;; ----------------------------------------------------------------------------

(define (last L)
  (if (null? L) '()
      (if (null? (cdr L)) (car L)
          (last (cdr L)))))

;; Accumulate the result of running a binary operation @p op over the values
;; in @p sequence with the given @p initial value.
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; `flatmap` doesn't exist in guile. This was taken from SICP.
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Feed the ongoing function evaluation results through to the next round
;; while recording the results in a list. `binfunc` is of the form `(binfunc
;; previous-value (car L))`.
(define (map-composition binfunc prev L)
  (if (null? L) (list prev)
      (let ((next (binfunc prev (car L))))
        (append (list prev) (map-composition binfunc next (cdr L))))))

;; ----------------------------------------------------------------------------
;; Simplify a bit by using flatmap and map.
(define (perms2 elements)
  (flatmap map-rotation-over-sub-perms2 (rotations2 elements)))
    
(define (map-rotation-over-sub-perms2 rotation)
  ;; This `if` block is needed because `(perms2 '())` returns `()` not `(())`
  ;; and `flatmap` and `map` return an empty set if mapping over an empty set.
  (if (null? (cdr rotation)) (list rotation)
      (map (lambda (p) (append (list (car rotation)) p)) (perms2 (cdr rotation)))))

(define (rotations2 elements . opt-rotated)
  (if (null? elements) '()
      (let ((rotated (if (null? opt-rotated) '() (car opt-rotated))))
        (append (list (append elements rotated))
                (rotations2 (cdr elements) (append rotated (list (car elements))))))))

;; ----------------------------------------------------------------------------
;; Get rid of map-rotation-over-sub-perms2
(define (perms3 elements)
  (if (null? elements) '(())  ;; This is the one permutation of the empty set.
      (flatmap (lambda (rotation)
                 (map (lambda (p) (append (list (car rotation)) p))
                      (perms3 (cdr rotation))))
               (rotations3 elements))))

(define (rotations3 elements . opt-rotated)
  (if (null? elements) '()
      (let ((rotated (if (null? opt-rotated) '() (car opt-rotated))))
        (append (list (append elements rotated))
                (rotations3 (cdr elements) (append rotated (list (car elements))))))))

;; ----------------------------------------------------------------------------
;; Try with a different rotations function, rotations4
(define (perms4 elements)
  (if (null? elements) '(())
      (flatmap (lambda (rotation)
                 (map (lambda (p) (append (list (car rotation)) p))
                      (perms3 (cdr rotation))))
               (rotations4 elements))))

(define (rotate4 L)
  (append (cdr L) (list (car L))))

(define (rotations4 elements)
  (accumulate (lambda (_ acc) (append (list (rotate4 (car acc))) acc))
              (list elements) (cdr elements)))

;; ----------------------------------------------------------------------------
;; Try embedding the rotate function into the rotations function.
(define (perms5 elements)
  (if (null? elements) '(())
      (flatmap (lambda (rotation)
                 (map (lambda (p) (append (list (car rotation)) p))
                      (perms3 (cdr rotation))))
               (rotations5 elements))))

(define (rotations5 elements)
  (accumulate (lambda (_ acc)
                (append (list (append (cdar acc) (list (caar acc)))) acc))
              (list elements) (cdr elements)))

;; ----------------------------------------------------------------------------
;; Try with `rotations6` using `map-composition`.
(define (perms6 elements)
  (if (null? elements) '(())
      (flatmap (lambda (rotation)
                 (map (lambda (p) (append (list (car rotation)) p))
                      (perms3 (cdr rotation))))
               (rotations6 elements))))

(define (rotations6 elements)
  (map-composition (lambda (prev _) (rotate6 prev)) elements (cdr elements)))

(define (rotate6 L)
  (append (cdr L) (list (car L))))

;; ----------------------------------------------------------------------------
(define (debug name x)
  (display name) (display "=") (display x) (newline) x)
