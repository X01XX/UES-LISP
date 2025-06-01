;;; Return all combinations of one item from each list in a list.
(defun any-1-of-each (alist)
  (assert (listp alist))
  (assert (> (length alist) 0))

  (let (alist2)
    ;; Check each item is a list, remove empty lists.
    (loop for lstx in alist do
      (assert (listp lstx))
      (if (not (null lstx))
        (setf alist2 (append alist2 (list lstx))))
    )
    (assert (> (length alist2) 0))

    (any-1-of-each2 alist2)
  )
)
(defun any-1-of-each2 (alist)
  (let (ret tupx)

    (when (= 1 (length alist))
      (setf tupx (car alist))
      (return-from any-1-of-each2 (mapcar #'list tupx)))

    (setf tupx (car alist))

    (loop for itemx in tupx do
      (setq ret (append ret  (mapcar #'(lambda (y) (push itemx y)) (any-1-of-each2 (cdr alist)))))
    )

    ret
  )
)
