;; Run square tests.
(defun square-tests ()
  (format t "~&square-tests beginning")

  ; Test square-new
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from-str "#x1") :result (state-from-str "#x2")))
    (assert (sample-p smpl))

    (setf sqr1 (square-new smpl))
    (assert (square-p sqr1))

    (format t "~&  square-new OK")
  )

  ;; Test unpredictable square, three different results.
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from-str "#x1") :result (state-from-str "#x2")))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from-str "#x1") :result (state-from-str "#xb")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x1") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Three different results, OK")
  )

  ;; Test unpredictable square, two different results, with wrong order.
  ;; Change to two-result square, pnc = true, upon more samples.
  ;; Change to one-result square, pnc = true, upon more samples.
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-one*))
    (assert (square-pnc sqr1))

    (format t "~&  Changing Pn with more samples, OK")
  )

  ;; Test two-result square.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xb")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (null (square-pnc sqr1)))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (null (square-pnc sqr1)))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xb")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (format t "~&  Two-result square, OK")
  )

  ;; Test unpredictable square, three different results.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xb")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Three-different-result square, OK")
  )

  ;; Test unpredictable square, two different results, with wrong order.
  ;; Change to two-result square, pnc = true, upon more samples.
  ;; Change to one-result square, pnc = true, upon more samples.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#xa")))
    (square-add-result sqr1 smpl)

    (setf smpl (sample-new :initial (state-from-str "#x5") :result (state-from-str "#x9")))
    (square-add-result sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Two-out-of-order-results square, OK")
  )

  (format t "~&square-tests done")
)
