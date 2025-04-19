;;;; Run tests.                                                                                                                           
(defun tools-tests ()
  (format t "~&tools-tests beginning")

  ; Test string-add-underscores.
  (let (strx errx)
    (setf errx (string-add-underscores-na "1_010"))
    (assert (and (err-p errx) (string= (err-str errx) "Argument contains underscores")))

    ; Test empty string.
    (setf strx (string-add-underscores ""))
    (assert (and (stringp strx) (string= strx "")))

    (setf strx (string-add-underscores "12345"))
    (assert (and (stringp strx) (string= strx "1_2345")))

    (format t "~&  string-add-underscores OK")
  )

  (format t "~&tools-tests done")
  t 
)  
