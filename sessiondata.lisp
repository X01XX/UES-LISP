;;;; Implement a data store for a session.

(defstruct sessiondata
    domains ; A store of one, or more, domains.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (sessiondata-<field name> <instance>) -> struct field.
;   (sessiondata-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> sessiondata
;   (typep <instance> 'sessiondata) -> bool
;
; Probably shouldn't use:
;   (make-sessiondata [:<field-name> <field-sessiondata>]*), use sessiondata-new instead.
;   (copy-sessiondata <instance>) copies a sessiondata instance.

(defun sessiondata-new () ; -> sessiondata
    (make-sessiondata :domains (domainstore-new))
)

;;; Return a string representation of a sessiondata instance.
(defun sessiondata-str (sessx)  ; -> string.                                                                      
  (assert (sessiondata-p sessx))

    (let ((strs "#S(SESSIONDATA "))
      (setf strs (concatenate 'string strs (domainstore-str (sessiondata-domains sessx))))

      (setf strs (concatenate 'string strs ")"))

      strs
    )
) 

(defun sessiondata-from (symbols) ; -> sessiondata
    (assert (listp symbols))

    ;(assert (eq (car symbols) 'QUOTE))
    ;(setf symbols (second symbols))

    ;(format t "~&sessiondata-from1 ~A ~A" (type-of symbols) symbols)
    (assert (consp symbols))
    (assert (typep (car symbols) 'SYMBOL))

    ;(format t "~&sessiondata-from2 ~A ~A" (type-of symbols) symbols)
    
    ;(setf symbols (cdr symbols))
    ;(format t "~&sessiondata-from3 ~A ~A" (type-of symbols) symbols)
    
    (let (sdx key ds sr sc rest-symbols)
        (cond ((string= (symbol-name (car symbols)) "SD")
                (setf rest-symbols (cdr symbols))
                (format t "~&sessiondata-from4 ~A ~A" (type-of rest-symbols) rest-symbols)

                ; Process tokens, cet domainstore.
                (loop for tokx in rest-symbols do
                    (setf key (symbol-name (car tokx)))
                    ;(format t "~&sessiondata-from: tokx: ~A" tokx)
                    (when (string= key "DS") ; Must be just one domainstore.
                           (format t "~&sessiondata-from: ds: ~A" tokx)
                           (assert (null ds)) ; only one ds allowed.
                           (setf ds (domainstore-from tokx))
                           (if ds
                               ;(session-data :domains ds)
                               nil)
                     )
                )
                (assert ds) ; one ds required.

                ;; Stort sessiondata.
                (setf sdx (make-sessiondata :domains ds))

                ;; Get select regions and statescorr, if any.
                (loop for tokx in rest-symbols do
                    (setf key (symbol-name (car tokx)))
                    (cond ((string= key "SR") ; Can be zero, or more, selectregions.
                           ;(format t "~&sessiondata-from: sr: ~A" tokx)
                       (setf sr (selectregions-from tokx))
                       (if sr
                           (format t "~&sessiondata: sr is ~A" (selectregions-str sr))
                           nil))
                      ((string= key "SC") ; Must be zero, or one, statescorr.
                       ;(format t "~&sessiondata-from: sc: ~A" tokx)
                       (setf sc (statescorr-from tokx))
                       (if sc
                           (format t "~&sessiondata: sc is ~A" (statescorr-str sc))
                           nil))
                      (t nil)
                    )
                )

                ;; Return sessiondata instance.
                sdx
              )
              (t (error "SD symbol missing")))
     )
)
