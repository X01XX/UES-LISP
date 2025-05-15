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

;;; Return a new masksvertices, made up of corresponding maskstore and a vertexstore.
(defun masksvertices-new (masks vertices) ; -> masksvertices.
  ;(format t "~&masks: ~A vertices ~A" (type-of masks) (type-of vertices))
  ;; Check arguments.
  (assert (maskstore-p masks))
  (assert (vertexstore-p vertices))
  (assert (= (1+ (maskstore-length masks)) (vertexstore-length vertices)))
  ;; TODO more rigorous check that arguments are in sync?

  ;; Return result.
  (make-masksvertices :masks masks :vertices vertices)
)

;;; Return true if two masksvertices are equal.
(defun masksvertices-eq (mskvtc1 mskvtc2) ; -> bool
  ;; Check arguments.
  (assert (masksvertices-p mskvtc1))
  (assert (masksvertices-p mskvtc2))

  ;; Return result.
  (and (maskstore-eq (masksvertices-masks mskvtc1) (masksvertices-masks mskvtc1))
       (vertexstore-eq (masksvertices-vertices mskvtc1) (masksvertices-vertices mskvtc2)))
)

;;; Return a string representing a masksvertices.
(defun masksvertices-str (mskvtc1) ; -> string.
  ;; Check arguments.
  (assert (masksvertices-p mskvtc1))

  ;; Return result.
  (format nil "(~A ~A)" (maskstore-str (masksvertices-masks mskvtc1)) (vertexstore-str (masksvertices-vertices mskvtc1)))
)
