; Implement a store of domains.

(defvar true t)
(defvar false nil)

; Implement a store of domains.
(defstruct domainstore
  domains  ; A list of zero, or more, non-duplicate, same number bits, domains.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domainstore-<field name> <instance>) -> struct field.
;   (domainstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> domainstore
;   (typep <instance> 'domainstore) -> bool
;
; Probably shouldn't use:
;   (make-domainstore [:<field-name> <field-domainstore>]*), use domainstore-new instead.
;   (copy-domainstore <instance>) copies a domainstore instance.
(defun domainstore-new (domains) ; -> domainstore instance.
  ;(format t "~&domains ~A" domains)
  (assert (domain-list-p domains))

  (let ((ret (make-domainstore :domains nil)))
    (loop for domx in domains do 
      (if (not (domainstore-contains ret domx))
        (domainstore-push ret domx))
    )
    ret
  )
)

; Push a new domain into a domainstore, suppress dups, subsets.
; Return true if the domain has been added.
(defun domainstore-push(storex domx) ; -> bool.
  (assert (domainstore-p storex))
  (assert (domain-p domainx))

  (let ((ret (domainstore-push-na storex domx)))
     (cond ((err-p ret) (error (err-str ret)))
           ((bool-p ret) ret)
            (t (error "Result is not a bool"))))
)

(defun domainstore-push-na(storex domx) ; -> bool, or err.
  ; Check for equal domains.
    (loop for acty in (domainstore-domains storex) do
      (if (= (domain-id acty) (domain-id domx))
        (return-from domainstore-push-na (err-new "duplicate domain id")))
    )

    ; Add the new domain to the end of the domains list.
    (if (null (domainstore-domains storex))
      (push domx (domainstore-domains storex))
      (push domx (cdr (last (domainstore-domains storex))))) 
  )
  true
)

; Return the number of domains in a domainstore.
(defun domainstore-length (storex) ; -> number.
  (assert (domainstore-p storex))

  (length (domainstore-domains storex))
)

; Return true if a domainstore is empty.
(defun domainstore-is-empty (storex) ; -> bool
  (zerop (domainstore-length storex))
)

; Return true if a domainstore is not empty.
(defun domainstore-is-not-empty (storex) ; -> bool
  (plusp (domainstore-length storex))
)

; Return a string representing a domainstore.
(defun domainstore-str (storex) ; -> string.
  (assert (domainstore-p storex))

  (let ((ret "#S(ACTIONSTORE ") (start t))

    (loop for domx in (domainstore-domains storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (domain-str domx)))
    )

    ret
  )
)

; Return true if a domainstore contains a given domain.
(defun domainstore-contains (storex domx) ; -> bool
  (assert (domainstore-p storex))
  (assert (domain-p domx))

  (if (member domx (domainstore-domains storex) :test #'domain-eq) true false)
)

(defun domainstore-first-domain (storex) ; -> domain
  (assert (domainstore-p storex))
  (assert (domainstore-is-not-empty storex))

  (car (domainstore-domains storex))
)


