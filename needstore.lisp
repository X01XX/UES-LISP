;;;; Implement a needstore structxa, a store of need struct instances.

(defvar true t)
(defvar false nil)

;;; Implement a store of masks.
(defstruct needstore
  need-list  ; A list of zero, or more, non-duplicate, same number bits, needs.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (needstore-<field name> <instance>) -> struct field.
;   (needstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> needstore
;   (typep <instance> 'needstore) -> bool
;
; Probably shouldn't use:
;   (make-needstore [:<field-name> <field-needstore>]*), use needstore-new instead.
;   (copy-needstore <instance>) copies a needstore instance.

;;; Return a new needstore instance.
(defun needstore-new (needs) ; -> needstore.
  ;(format t "~&needstore-new ~A" needs)
  (assert (need-list-p needs))

  (make-needstore :need-list needs)
)

;;; Return a string representing a needstore list.
(defun needstore-str (alist)
; (format t "~&needstore-str for ~A" alist)
    (assert (needstore-p alist))

    (let ((str "("))

        (loop for needx in alist
              for count from 0 do

                 (when (plusp count)
                     (setf str (concatenate 'string str (format nil " ~&            "))))

                 (setf str (concatenate 'string str (need-str needx)))
        )
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Returns true if a need of a given kind and target are in a needstore.
(defun needstore-find-kind-target (needs kind target)
    (assert (needstore-p needs))
    (assert (integerp kind))
    (assert (or (zerop kind) (plusp kind)))

    (loop for needx in needs do
        (if (and (= (need-kind needx) kind) (region-eq (need-target needx) target))
            (return-from needstore-find-kind-target t))
    )
    nil
)

;;; Returns the number of a given kind of need in a needstore.
(defun needstore-number-kind (needs kind)
    (assert (needstore-p needs))
    (assert (integerp kind))
    (assert (or (zerop kind) (plusp kind)))

    (let ((cnt 0))
        (loop for needx in needs do
            (if (= (need-kind needx) kind)
                (incf cnt))
        )
        cnt
    )
)

;;; Do various tests
(defun needstore-tests ()

    (format t "~&needstore-tests OK")
    'OK
)

