;;;; Implement a store of selectregionss.

; Implement a store of selectregionss.
(defstruct selectregionsstore
  selectregions  ; A list of zero, or more, non-duplicate, same number bits, selectregionss.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (selectregionsstore-<field name> <instance>) -> struct field.
;   (selectregionsstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> selectregionsstore
;   (typep <instance> 'selectregionsstore) -> bool
;
; Probably shouldn't use:
;   (make-selectregionsstore [:<field-name> <field-selectregionsstore>]*), use selectregionsstore-new instead.
;   (copy-selectregionsstore <instance>) copies a selectregionsstore instance.

;;; Return a new selectregionsstore instance.
(defun selectregionsstore-new (selectregions) ; -> selectregionsstore.
  ;; Check argument.
  (assert (listp selectregions))
  (eval (append (list 'and) (mapcar #'(lambda (x) (selectregions-p x)) selectregions)))

  ;; Return result.
  (make-selectregionsstore :selectregions selectregions)
)

; Push a new selectregions into a selectregionsstore.
(defun selectregionsstore-push (storex selectregionsx) ; -> nothing. Side-effect selectregionsstore is changed.
  ;; Check arguments.
  (assert (selectregionsstore-p storex))
  (assert (selectregions-p selectregionsx))

  ;; Return result.
  (push selectregionsx (selectregionsstore-selectregions storex))
)

; Return the number of selectregionss in a selectregionsstore.
(defun selectregionsstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  ;; Return result.
  (length (selectregionsstore-selectregions storex))
)

; Return true if a selectregionsstore is empty.
(defun selectregionsstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  ;; Return result.
  (zerop (selectregionsstore-length storex))
)

; Return true if a selectregionsstore is not empty.
(defun selectregionsstore-is-not-empty (storex) ; -> bool
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  ;; Return result.
  (plusp (selectregionsstore-length storex))
)

; Return a string representing a selectregionsstore.
(defun selectregionsstore-str (storex) ; -> string.
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  (let ((ret "(SRS ") (start t))

    (loop for sregsx in (selectregionsstore-selectregions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (selectregions-str sregsx)))
    )
    ;; Return result.
    ret
  )
)

;;; Return the rate af a given regionscorr, based on superset SelectRegions.
(defun selectregionsstore-rate (storex regscr) ; -> rate
  ;; Check arguments.
  (assert (selectregionsstore-p storex))
  (assert (regionscorr-p regscr))

  (let ((ret (rate-new :positive 0 :negative 0)))

    ;; Aggregate rates from selected selectregions.
    (loop for srx in (selectregionsstore-selectregions storex) do
      (when (regionscorr-intersects regscr (selectregions-regionscorr srx))
        (setf ret (rate-union ret (selectregions-rate srx)))
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return a regionscorrstore of all selectregions-regionscorr.
(defun selectregionsstore-regionscorrs (storex) ; -> regionscorrstore
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  (let ((ret (regionscorrstore-new nil)))
    ;; Gather regionscorr from each selectregions.
    (loop for selx in (selectregionsstore-selectregions storex) do
      (regionscorrstore-push ret (selectregions-regionscorr selx))
    )
    ;; Return result.
    ret
  )
)

;;; Return true if at least one negative valued selectregion exists.
(defun selectregionsstore-negative-selectregions-exist (storex) ; -> bool
  ;; Check argument.
  (assert (selectregionsstore-p storex))

  ;; Check each selectregions
  (loop for selx in (selectregionsstore-selectregions storex) do
    (if (not (zerop (rate-negative (selectregions-rate selx))))
      (return-from selectregionsstore-negative-selectregions-exist true)) ; Return positive result.
  )
  ;; Return negative result.
  false
)

