; Extraneous useful functions.

;; Return true if the passed argument is true or false.
(defun bool-p (val)
  (or (eq val true) (eq val false))
)

; The opposite of eq.
(defun neq (arg1 arg2) ; -> bool
  (not (eq arg1 arg2))
)

;;; Remove comments from a string.
;;; semicolon to \n, delete a line that is all comment.
(defun remove-comments (str) ; -> string
    (assert (stringp str))

    (let ((ret "") skip (token ""))
        (loop for char across str do
            (if (char= #\; char) (setf skip t))

            (when (char= #\NewLine char)
                (setf skip nil)
                (setf token (string-right-trim '(#\Space) token))
                (when (string/= token "")
                    (setf ret (concatenate 'string ret token))
                    (setf ret (concatenate 'string ret (coerce (list #\NewLine) 'string)))
                    (setf token "")
                )
            )
            (when (and (not skip) (char/= #\NewLine char))
                (setf token (concatenate 'string token (coerce (list char) 'string)))
            )

        )
        (setf token (string-right-trim '(#\Space) token))
        (when (string/= token "")
            (setf ret (concatenate 'string ret token))
        )
        ret
    )
)

;;; Parse a string, using commas and spaces as separators, between balanced brackets.
(defun parse-str (str) ; -> list of string tokens.
    (let (ret (token "") (left 0) (right 0))
        (loop for char across str do
            (if (char= char #\[)
                (incf left)
            )
            (when (char= char #\])
                (incf right)
                (if (> right left)
                    (error "brackets unbalanced"))
            )
            (cond ((and (= left right) (or (char= char #\Space) (char= char #\,) (char= char #\NewLine)))
                   (when (> (length token) 0)
                           (push token ret)
                           (setf token "")))

                  (t (setf token (concatenate 'string token (coerce (list char) 'string))))
            )
        )
        (if (> (length token) 0)
              (push token ret))
        (reverse ret)
    )
)

;; Return the Boolean xor of two boolen values.
(defun xor (b1 b2) ; -> bool
  (or (and b1 (not b2)) (and (not b1) b2))
)

;;; Add underscores for each 4 characters of a string, from right to left.
(defun string-add-underscores (str) ; -> string.
  ;(format t "~&string-add-underscores: ~A" (type-of str))
  (assert (stringp str))
  ;(format t "~&string-add-underscores: ~A" str)

  (let ((ret (string-add-underscores-na str)))
    (cond ((err-p ret) (error (err-str ret)))
          ((stringp ret) ret)
          (t (error "Result is not a string"))))
)

;;; Add underscores no-abort (na).
(defun string-add-underscores-na (str) ; -> string, or err.

  (let ((str2 "") cnt (str-len (length str)))
     (setf cnt str-len)
     (loop for chr across str do
       (if (char= chr #\_)
           (return-from string-add-underscores-na (err-new "Argument contains underscores")))

       (if (and (/= cnt str-len) (zerop (mod cnt 4)))
           (setf str2 (concatenate 'string str2 "_"))
       )
       (setf str2 (concatenate 'string str2 (princ-to-string chr)))
       (decf cnt)
     )
     str2
  )
)

