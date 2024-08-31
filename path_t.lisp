;;; Run path tests.
(defun path-tests ()

  (format t "~&path-tests beginning")

  ; Test path-new.
  (let (path1 path2)
    ; Test new, empty, path.
    (setf path1 (path-new (regionstore-new nil)))
    (assert (path-p path1))
    (assert (path-is-empty path1))

    ; Test new, non-empty, path.
    (setf path1 (path-new (regionstore-new (list (region-from-str "0100")))))
    (assert (path-p path1))
    (assert (path-is-not-empty path1))

    (format t "~&  path-new OK")
  )

  ; Test path-add-start.
  (let (path1)
    (setf path1 (path-new (regionstore-new (list (region-from-str "0x0x")))))
    (path-add-start path1 (region-from-str "0101"))
    (assert (= (path-length path1) 2))
    (assert (region-eq (path-first-region path1) (region-from-str "0101")))
    (assert (region-eq (path-last-region path1) (region-from-str "0x0x")))

    (format t "~&  path-add-start OK")
  )

  ; Test path-add-end.
  (let (path1)
    (setf path1 (path-new (regionstore-new (list (region-from-str "0101")))))
    (path-add-end path1 (region-from-str "0x0x"))
    (assert (= (path-length path1) 2))
    (assert (region-eq (path-first-region path1) (region-from-str "0101")))
    (assert (region-eq (path-last-region path1) (region-from-str "0x0x")))

    (format t "~&  path-add-end OK")
  )

  (format t "~&path-tests done")
  t
)

