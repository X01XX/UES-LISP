;;; Run tests.
(defun stepstore-tests ()
  (format t "~&stepstore-tests beginning")

  ; Test stepstore-new.
  (let (store1 step1)

    (setf step1 (step-new 0 (rule-from "[XX]")))

    (setf store1 (stepstore-new (list step1)))
    (assert (stepstore-p store1))

    (assert (= (stepstore-length store1) 1))

    (format t "~&  stepstore-new OK")
  )

  ; Test stepstore-intersection.
  (let (storex storey storez step1 step2 step3 step4)
    (setf step1 (step-new 0 (rule-from "[XX]")))
    (setf step2 (step-new 1 (rule-from "[XX]")))
    (setf step3 (step-new 1 (rule-from "[XX]")))
    (setf step4 (step-new 2 (rule-from "[XX]")))

    (setf storex (stepstore-new (list step1 step2)))
    (setf storey (stepstore-new (list step3 step4)))
    (setf storez (stepstore-intersection storex storey))

    ;(format t "~&storez ~A" (stepstore-str storez))
    (assert (= 1 (stepstore-length storez)))
    (assert (stepstore-member storez step2))
    (assert (stepstore-member storez step3))

    (format t "~&  stepstore-intersection OK")
  )

  ; Test stepstore-union.
  (let (storex storey storez step1 step2 step3 step4)
    (setf step1 (step-new 0 (rule-from "[XX]")))
    (setf step2 (step-new 1 (rule-from "[XX]")))
    (setf step3 (step-new 1 (rule-from "[XX]")))
    (setf step4 (step-new 2 (rule-from "[XX]")))

    (setf storex (stepstore-new (list step1 step2)))
    (setf storey (stepstore-new (list step3 step4)))
    (setf storez (stepstore-union storex storey))

    ;(format t "~&storez ~A" (stepstore-str storez))
    (assert (= 3 (stepstore-length storez)))
    (assert (stepstore-member storez step1))
    (assert (stepstore-member storez step2))
    (assert (stepstore-member storez step3))
    (assert (stepstore-member storez step4))

    (format t "~&  stepstore-union OK")
  )

  ; Test stepstore-difference.
  (let (storex storey storez step1 step2 step3 step4)
    (setf step1 (step-new 0 (rule-from "[XX]")))
    (setf step2 (step-new 1 (rule-from "[XX]")))
    (setf step3 (step-new 1 (rule-from "[XX]")))
    (setf step4 (step-new 2 (rule-from "[XX]")))

    (setf storex (stepstore-new (list step1 step2)))
    (setf storey (stepstore-new (list step3 step4)))
    (setf storez (stepstore-difference storex storey))

    ;(format t "~&storez ~A" (stepstore-str storez))
    (assert (= 1 (stepstore-length storez)))
    (assert (stepstore-member storez step1))

    (format t "~&  stepstore-difference OK")
  )

  ; Test stepstore-initial-region-intersects.
  (let (storex storez step1 step2 step3)
    (setf step1 (step-new 0 (rule-from "[XX/10/00/11]")))
    (setf step2 (step-new 1 (rule-from "[XX/00/00/10]")))
    (setf step3 (step-new 2 (rule-from "[XX/00/00/00]")))

    (setf storex (stepstore-new (list step1 step2 step3)))
    (setf storez (stepstore-initial-region-intersects storex (region-from 'r10X1)))
    ;(format t "~&storez ~A" (stepstore-str storez))
    (assert (= 1 (stepstore-length storez)))
    (assert (stepstore-member storez step2))

    (format t "~&  stepstore-initial-region-intersects OK")
  )

  ; Test stepstore-result-region-intersects.
  (let (storex storez step1 step2 step3)
    (setf step1 (step-new 0 (rule-from "[XX/10/00/11]")))
    (setf step2 (step-new 1 (rule-from "[XX/00/00/10]")))
    (setf step3 (step-new 2 (rule-from "[XX/00/00/00]")))

    (setf storex (stepstore-new (list step1 step2 step3)))
    (setf storez (stepstore-result-region-intersects storex (region-from 'r10X1)))
    ;(format t "~&storez ~A" (stepstore-str storez))
    (assert (= 1 (stepstore-length storez)))
    (assert (stepstore-member storez step1))

    (format t "~&  stepstore-result-region-intersects OK")
  )

  ; Test stepstore-aggregate-changes.
  (let (storex step1 step2 step3 cngx)
    (setf step1 (step-new 0 (rule-from "[XX/10/01/11]")))
    (setf step2 (step-new 1 (rule-from "[XX/00/00/10]")))
    (setf step3 (step-new 2 (rule-from "[XX/01/00/00]")))

    (setf storex (stepstore-new (list step1 step2 step3)))
    (setf cngx (stepstore-aggregate-changes storex))

    ;(format t "~&cngx ~A" (change-str cngx))
    (assert (change-eq cngx (change-new :m01 (mask-from 'm0110) :m10 (mask-from 'm0101))))
    
    (format t "~&  stepstore-aggregate-changes OK")
  )

  (format t "~&stepstore-tests done")
  t
)
