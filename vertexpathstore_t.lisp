;;; Run vertexpathstore tests.
(defun vertexpathstore-tests ()
  (format t "~&vertexpathstore-tests beginning")

  ;; Test vertexpathstore-new.
  ;; Also vertexpathstore-length.
  (let (vtxpth1 vtxpth2 store1)

    (setf vtxpth1 (vertexpath-new 
         (statestore-new (list (state-from 's0000) (state-from 's0001) (state-from 's0011)))))

    (setf vtxpth2 (vertexpath-new 
         (statestore-new (list (state-from 's1000) (state-from 's1001) (state-from 's1011)))))

    ;; Test with list of vertexpaths.
    (setf store1 (vertexpathstore-new (list vtxpth1 vtxpth2)))
    (assert (vertexpathstore-p store1))
    (assert (= (vertexpathstore-length store1) 2))

    ;; Test with one vertexpath.
    (setf store1 (vertexpathstore-new vtxpth1))
    (assert (vertexpathstore-p store1))
    (assert (= (vertexpathstore-length store1) 1))

    (format t "~&  vertexpathstore-new OK")
  )

  ;; Test vertexpathstore-push-nosubs.
  ;; Also vertexpathstore-push, vertexpathstore-member.
  (let (vtxpth1 vtxpth2 vtxpth3 store1)

    (setf store1 (vertexpathstore-new nil))

    (setf vtxpth1 (vertexpath-new 
         (statestore-new (list (state-from 's0000) (state-from 's0001) (state-from 's0011)))))

    (vertexpathstore-push-nosubs store1 vtxpth1)
    (vertexpathstore-push-nosubs store1 vtxpth1)
    (assert (= (vertexpathstore-length store1) 1))
    
    (setf vtxpth2 (vertexpath-new 
         (statestore-new (list (state-from 's1011) (state-from 's1111)))))
    (vertexpathstore-push-nosubs store1 vtxpth2)
    (assert (= (vertexpathstore-length store1) 2))

    (setf vtxpth3 (vertexpath-new 
         (statestore-new (list (state-from 's1111) (state-from 's1011) (state-from 's1001)))))
    (vertexpathstore-push-nosubs store1 vtxpth3)
    (assert (= (vertexpathstore-length store1) 2))

    (assert (vertexpathstore-member store1 vtxpth1))
    (assert (vertexpathstore-member store1 vtxpth3))


    (format t "~&  vertexpathstore-push-nosubs OK")
  )

  ;; Test vertexpathstore-unique-masks.
  (let (store1 vtxpth1 vtxpth2 vtxpth3 vtxpth4 masks mskstr1 mskstr2)
    (setf store1 (vertexpathstore-new nil))

    (setf vtxpth1 (vertexpath-new 
         (statestore-new (list (state-from 's0000) (state-from 's0001) (state-from 's0011))))) ; masks: m0010, m0001.

    (vertexpathstore-push-nosubs store1 vtxpth1)

    (setf vtxpth2 (vertexpath-new 
         (statestore-new (list (state-from 's1000) (state-from 's1001) (state-from 's1011))))) ; masks: m0010, m0001.

    (vertexpathstore-push-nosubs store1 vtxpth2)

    (setf vtxpth3 (vertexpath-new 
         (statestore-new (list (state-from 's1000) (state-from 's1100))))) ; masks: m0100.

    (vertexpathstore-push-nosubs store1 vtxpth3)

    (setf vtxpth4 (vertexpath-new 
         (statestore-new (list (state-from 's1001) (state-from 's1101))))) ; masks: m0100.

    (vertexpathstore-push-nosubs store1 vtxpth4)
;   (format t "~&store1: ~A" (vertexpathstore-str store1))
    (assert (= (vertexpathstore-length store1) 4))

    (setf masks (vertexpathstore-unique-masks store1))

;   (format t "~&masks: ")
;   (loop for msksx in masks do
;     (format t " ~A" (maskstore-str msksx))
;   )
;   (format t "~& ")

    (assert (= (length masks) 2))

    (setf mskstr1 (maskstore-new (list (mask-from 'm0010) (mask-from 'm0001))))
    (assert (member mskstr1 masks :test #'maskstore-eq))

    (setf mskstr2 (maskstore-new (list (mask-from 'm0100))))
    (assert (member mskstr2 masks :test #'maskstore-eq))

    (format t "~&  vertexpathstore-unique-masks OK")
  )

  ;; Test vertexpathstore-matching-masks.
  (let (store1 vtxpth1 vtxpth2 vtxpth3 vtxpth4 masks match)
    (setf store1 (vertexpathstore-new nil))

    (setf vtxpth1 (vertexpath-new 
         (statestore-new (list (state-from 's0000) (state-from 's0001) (state-from 's0011))))) ; masks: m0010, m0001.

    (vertexpathstore-push-nosubs store1 vtxpth1)

    (setf vtxpth2 (vertexpath-new 
         (statestore-new (list (state-from 's1000) (state-from 's1001) (state-from 's1011))))) ; masks: m0010, m0001.

    (vertexpathstore-push-nosubs store1 vtxpth2)

    (setf vtxpth3 (vertexpath-new 
         (statestore-new (list (state-from 's1000) (state-from 's1100))))) ; masks: m0100.

    (vertexpathstore-push-nosubs store1 vtxpth3)

    (setf vtxpth4 (vertexpath-new 
         (statestore-new (list (state-from 's1001) (state-from 's1101))))) ; masks: m0100.

    (vertexpathstore-push-nosubs store1 vtxpth4)
;   (format t "~&store1: ~A" (vertexpathstore-str store1))
    (assert (= (vertexpathstore-length store1) 4))

    (setf masks (vertexpathstore-unique-masks store1))
    (loop for msksx in masks do
      ;(format t "~&mask: ")
      ;(format t " ~A" (maskstore-str msksx))
      (setf match (vertexpathstore-matching-masks store1 msksx))
      (assert (= (vertexpathstore-length match) 2))
      ;(format t " ~A" (vertexpathstore-str match))
      (mapcar #'(lambda (x) (assert (maskstore-eq (vertexpath-masks x) msksx))) (vertexpathstore-vertexpaths match))
    )

    (format t "~&  vertexpathstore-matching-masks OK")
  )

  (format t "~&vertexpathstore-tests done")
  t
)

