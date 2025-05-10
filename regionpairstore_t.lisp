;e; Run tests.
(defun regionpairstore-tests ()
  (format t "~&regionpairstore-tests beginning")

  ;; Test regionpairstore-new.
  ;; Also regionpairstore-is-empty, regionpairstore-is-not-empty, regionpairstore-length, regionpairstore-num-bits.
  (let (rps rp1 rp2)
    ;; Test empty regionpairstore.
    (setf rps (regionpairstore-new nil))
    (assert (regionpairstore-p rps))
    (assert (regionpairstore-is-empty rps))

    ;; Test non-empty regionpairstore.
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (setf rp2 (regionpair-new (list (region-from 'rX001) (region-from 'rX011))))
    (setf rps (regionpairstore-new (list rp1 rp2)))
    (assert (regionpairstore-p rps))
    (assert (regionpairstore-is-not-empty rps))
    (assert (= (regionpairstore-length rps) 2))
    (assert (= (regionpairstore-num-bits rps) 4))

    (format t "~&  regionpairstore-new OK")
  )

  ;; Test regionspairstore-push-nosubs.
  ;; Also regionpairstore-push, regionpairstore-member.
  (let (rps rp1 rp2 rp3 rp4)
    ;; Add first regionpair.
    (setf rps (regionpairstore-new nil))
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (regionpairstore-push-nosubs rps rp1)
    (assert (= (regionpairstore-length rps) 1))

    ;; Push subset pair.
    (setf rp2 (regionpair-new (list (region-from 'r1010) (region-from 'r0010))))
    (regionpairstore-push-nosubs rps rp2)
    (assert (= (regionpairstore-length rps) 1))
    (assert (regionpairstore-member rps rp1))

    ;; Push superset pair.
    (setf rp3 (regionpair-new (list (region-from 'r1X1X) (region-from 'r0X1X))))
    (regionpairstore-push-nosubs rps rp3)
    (assert (= (regionpairstore-length rps) 1))
    (assert (regionpairstore-member rps rp3))

    ;; Push non-subset, non-superset pair.
    (setf rp4 (regionpair-new (list (region-from 'r0X0X) (region-from 'r1X0X))))
    (regionpairstore-push-nosubs rps rp4)
    (assert (= (regionpairstore-length rps) 2))
    (assert (regionpairstore-member rps rp3))
    (assert (regionpairstore-member rps rp4))

    (format t "~&  regionspairstore-push-nosubs OK")
  )

  ;; Test regionpairstore-regionpairs-state-in.
  (let (rps rp1 rp2 sta-in)
    (setf rp1 (regionpair-new (list (region-from 'r101X) (region-from 'r001X))))
    (setf rp2 (regionpair-new (list (region-from 'rX101) (region-from 'rX111))))
    (setf rps (regionpairstore-new (list rp1 rp2)))

    ;; Test no match.
    (setf sta-in (regionpairstore-regionpairs-state-in rps (state-from 's0000)))
    (assert (regionpairstore-p sta-in))
    (assert (regionpairstore-is-empty sta-in))

    ;; Test one match, first region.
    (setf sta-in (regionpairstore-regionpairs-state-in rps (state-from 's1010)))
    (assert (regionpairstore-p sta-in))
    (assert (= (regionpairstore-length sta-in) 1))
    (assert (regionpairstore-member sta-in rp1))

    ;; Test one match, second region.
    (setf sta-in (regionpairstore-regionpairs-state-in rps (state-from 's1111)))
    (assert (regionpairstore-p rps))
    (assert (= (regionpairstore-length sta-in) 1))
    (assert (regionpairstore-member sta-in rp2))

    (format t "~&  regionpairstore-regionpairs-state-in OK")
  )

  (format t "~&regionpairstore-tests done")
  t
)
