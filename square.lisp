;;;; Implement the Square type.

(defstruct square
    state
    (count 0)
    results
    pn
    pnc
    rules
)
; Functions automatically created by defstruct:
;
; Most used:
;   (square-<field name> <instance>) returns struct field.
;   (square-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> square
;   (typep <instance> 'square) -> t
;
; Don't use:
;   (make-square [:<field-name> <field-value>]*), use square-new instead.
;   (copy-square <instance>) copies a square instance.

;;; Return a new Square instance.
(defun square-new (smpl)  ; -> square instance
    (assert (sample-p smpl))

    (let (ary)
        (setf ary (make-array '(4)))	; Make a four element array, filled with nils.
        (setf (aref ary 0) (sample-result smpl))
    
        (make-square
            :state (sample-initial smpl)
            :count 1
            :results ary
            :pn *pn-one*
            :pnc nil
            :rules (rulestore-new (list (rule-new smpl)))
        )
    )
)

;;; Return the length of the square results list.
(defun square-results-length (square)  ; -> integer, 1-4
    (assert (square-p square))
    (min (square-count square) 4)
)

;;; Return the pn for a (probably just updated) square.
(defun square-calc-pn (square)  ; -> pn value

    (assert (square-p square))

    (if (= 1 (square-results-length square))
      (return-from square-calc-pn *pn-one*))

    (let ((result0 (aref (square-results square) 0)) (pn-one t))

        ;; Check for pn 1
        (loop for inx from 1 to (1- (square-results-length square)) do
            (if (state-neq (aref (square-results square) inx) result0) (setf pn-one nil))
        )

        ;; Calc pn, pnc values, rules.
        (when pn-one
            (return-from square-calc-pn *pn-one*)
        )

        ;; Try to disprove pn-two
        (when (> (square-count square) 2)

	    (if (state-neq result0 (aref (square-results square) 2))
                (return-from square-calc-pn *pn-none*))

            (when (> (square-count square) 3)

	        (if (state-neq (aref (square-results square) 1) (aref (square-results square) 3))
                    (return-from square-calc-pn *pn-none*))
	    )
	)

        *pn-two*
    ) ; end-let
) ; end square-calc-pn

;;; Calculate the pnc of a (probably just updated) square.
(defun square-calc-pnc (square) ; -> bool
    (assert (square-p square))

    (if (eq (square-pn square) *pn-one*)
        (if (> (square-count square) 1)
            (return-from square-calc-pnc t)
            (return-from square-calc-pnc nil)))

    (if (eq (square-pn square) *pn-two*)
        (if (> (square-count square) 3)
            (return-from square-calc-pnc t)
            (return-from square-calc-pnc nil)))

    t
)

;;; Add a result to a square.
;;; An existing square will have at least one result already.
;;; Return true if the square pn value or pnc bool changes.
(defun square-add-result (square smpl) ; -> bool
    (assert (square-p square))
    (assert (sample-p smpl))
    (assert (state-eq (sample-initial smpl) (square-state square)))

    ; Update results
    ; If the number of results are over four, the oldest result will be overlaid.
    (setf (aref (square-results square) (mod (square-count square) 4)) (sample-result smpl))

    (incf (square-count square))

    (let (pnnew pncnew ret)

        (setf pnnew (square-calc-pn square))

        (when (neq pnnew (square-pn square))
            (format t "~&                 square ~A pn  changed from ~A to ~A" (state-str (square-state square)) (pn-str (square-pn square)) (pn-str pnnew))
            (setf (square-pn square) pnnew) ; set new pn, so subsequent pnc calc works correctly.

	    (cond ((eq pnnew *pn-one*)
	           (setf (square-rules square) (rulestore-new (list (rule-new smpl)))))

	          ((eq pnnew *pn-two*)
	           (setf (square-rules square)
			 (rulestore-new (list (rule-new (sample-new :initial (square-state square) :result (aref (square-results square) 0)))
	                                      (rule-new (sample-new :initial (square-state square) :result (aref (square-results square) 1)))))))

	          ((eq pnnew *pn-none*)
	           (setf (square-rules square) nil))

		  (t (error "unrecognized pn value"))
            )

	    (setf ret t)
        )

        (setf pncnew (square-calc-pnc square))
	;(format t "~& sqr ~A pncnew ~A pnc ~A" (state-str (square-state square)) pncnew (square-pnc square))

        (when  (not (eq pncnew (square-pnc square)))
            (format t "~&                 square ~A pn ~A pnc changed from ~A to ~A"
		    (state-str (square-state square)) (pn-str (square-pn square)) (square-pnc square) pncnew)
            (setf (square-pnc square) pncnew)
            (return-from square-add-result t)
        )
        ; (if (null ret)
	;    (format t "~&square ~A nothing changed pn ~A pnc ~A" (state-str(square-state square)) (pn-str (square-pn square)) (square-pnc square)))
        ret
    ) ; end let
) ; end square-add-result

;;; Return a string representing a square.
(defun square-str (asqr)  ; -> string
    (assert (square-p asqr))
 
    (let ((str "#S[SQUARE "))
        (setf str (concatenate 'string str (state-str (square-state asqr))))
        (setf str (concatenate 'string str (format nil " :pn ~D :pnc ~A" (pn-str (square-pn asqr)) (square-pnc asqr))))
        (setf str (concatenate 'string str (format nil " :rules ~A" (rulelist-str (square-rules asqr)))))
        (setf str (concatenate 'string str "]"))
        str
    )
)

;;; Return the number of samples needed to reach pnc
(defun square-number-samples-needed (sqrx)  ; -> integer, 0 - 2
    (assert (square-p sqrx))

    (cond   ((eq *pn-none* (square-pn sqrx)) 0)
            ((eq *pn-one* (square-pn sqrx))
                (if (eq 1 (square-count sqrx)) 1 0))
            ((eq *pn-two* (square-pn sqrx))
                (if (eq 2 (square-count sqrx)) 2
                    (if (eq 3 (square-count sqrx)) 1 0))))
)

;;; Return true if two squares are equal.
(defun square-eq (sqr1 sqr2)  ; -> bool
    (assert (square-p sqr1))
    (assert (square-p sqr2))
    (state-eq (square-state sqr1) (square-state sqr2))
)

;;; Return can combination now indicator
;;; A possible subset relationship will return false, but
;;; with more samples may return true.
(defun square-can-combine-now (sqrx sqry) ; -> bool
    (assert (square-p sqrx))
    (assert (square-p sqry))

    ;; Trying to combine the same square is probably an error in logic.
    (assert (state-neq (square-state sqrx) (square-state sqry)))

    (let ((pnx (square-pn sqrx)) (pny (square-pn sqry))
          (rulsx (square-rules sqrx)) (rulsy (square-rules sqry)))

      ;; Three possibilities. 1/1, 2/2, 3/3.
      (if (neq pnx pny)
  	  (return-from square-can-combine-now nil))

      (if (eq *pn-none* pnx) (return-from square-can-combine-now t))
  
      (rulelist-can-combine-now rulsx rulsy) 
  ) ; end let
)

;;; Return true if two squares may be compatible as-is, or with more samples of
;;; the second square given.
(defun square-compatible (sqrx sqry)
    (assert (square-p sqrx))
    (assert (square-p sqry))

    ; Trying to combine the same square is probably an error in logic.
    (assert (state-neq (square-state sqrx) (square-state sqry)))

    (if (pn-gt (square-pn sqry) (square-pn sqrx))
        (return-from square-compatible nil))

    (if (and (pn-lt (square-pn sqry) (square-pn sqrx)) (square-pnc sqry))
        (return-from square-compatible nil))

    (if (eq (square-pn sqrx) *pn-none*)
        (return-from square-compatible t))

    (if (rulelist-can-combine-now (square-rules sqrx) (square-rules sqry))
        (return-from square-compatible t))

    (format t "~&about to compare ~A and ~A" (rulelist-str (square-rules sqrx)) (rulelist-str (square-rules sqry)))
    (if (= 1 (length (square-rules sqrx)))
        (rule-valid-union-p (rule-union (first (square-rules sqrx)) (first (square-rules sqry))))
        (or (rule-valid-union-p (rule-union (first (square-rules sqrx)) (first (square-rules sqry))))
            (rule-valid-union-p (rule-union (second (square-rules sqrx)) (first (square-rules sqry))))))
)

