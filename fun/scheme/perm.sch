;; ----------------------------------------------------------------------------
;; Generate a list of permutations of the given @p elements.
;;
;; @param elements - list of elements to be permuted.
;;
;; @return a list of permutations of @p elements, each being a list.
;;
(define (perms elements)
  (if (null? elements) '(())
      (for-each-rotation-map-car-with-sub-perms (rotations elements))))

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
(define (for-each-rotation-map-car-with-sub-perms rotations)
  (if (null? rotations) '()
      (append (map (append-to-atom (caar rotations)) (perms (cdar rotations)))
              (for-each-rotation-map-car-with-sub-perms (cdr rotations)))))

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
;; @return a function that appends its argument to `(list item)`.
;;
(define (append-to-atom item)
  (let ((item-as-list (list item)))
    (lambda (appendee) (append item-as-list appendee))))
