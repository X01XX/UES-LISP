;;;; Implement the masksvertices struct and functions.

;;; The masksvertices struct.
(defstruct masksvertices
  masks    ; A store of correspondung regions.
  vertices ; A vertexstore instance, indicating a value for matching Domain regions.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (masksvertices-<field name> <instance>) -> struct field.
;   (masksvertices-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> masksvertices
;   (typep <instance> 'masksvertices) -> bool
;
; Probably shouldn't use:
;   (make-masksvertices [:<field-name> <field-masksvertices>]*), use masksvertices-new instead.
;   (copy-masksvertices <instance>) copies a masksvertices instance.

;;; Return a new masksvertices, made up of corresponding maskstorex and a vertexstore.
(defun masksvertices-new (masks vertices) ; -> masksvertices.
  (assert (maskstore-p masks))
  (assert (vertexstore-p vertices))
  (assert (= (1+ (maskstore-length masks)) (vertexstore-length vertices)))
  ;; TODO more rigorous check that arguments are in sync?

  (make-masksvertices :masks masks :vertices vertices)
)

;;; Return true if two masksvertices are equal.
(defun masksvertices-eq (store1 store2) ; -> bool
  (assert (masksvertices-p store1))
  (assert (masksvertices-p store2))

  (and (maskstore-eq (masksvertices-masks store1) (masksvertices-masks store1))
       (vertexstore-eq (masksvertices-vertices store1) (masksvertices-vertices store2)))
)

