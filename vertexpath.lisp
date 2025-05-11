;;;; Implement a struct for a vertex path, that is, a series of sequentially adjacent, dissimilar
;;;; states, winding through a series of regions that are not overlapped by any other region.

;;; The vertexpath struct.
(defstruct vertexpath
  states    ; A StateStore of states, GT 1, sequentially adjacent.
  masks     ; A maskstore of edges traversed by the states.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (vertexpath-<field name> <instance>) -> struct field.
;   (vertexpath-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> vertexpath
;   (typep <instance> 'vertexpath) -> bool
;
; Probably shouldn't use:
;   (make-vertexpath [:<field-name> <field-vertexpath>]*), use vertexpath-new instead.
;   (copy-vertexpath <instance>) copies a vertexpath instance.

;;; Return a new vertexpath, made up of one, or more, states.
(defun vertexpath-new (states) ; -> vertexpath.
  ;; Check arguments.
  (assert (statestore-p states))

  ;; Check states and generate masks.
  (let ((last-state (car (statestore-states states))) (masks (maskstore-new nil)) dif-mask)
    (loop for  cur-state in (cdr (statestore-states states)) do
      ;; State and following state must be adjacent.
      (assert (state-is-adjacent last-state cur-state))

      ;; No duplicate difference mask is allowed, that would indicate turning back into 
      ;; already processed regions.
      (setf dif-mask (mask-new (state-xor last-state cur-state)))
      (assert (not (maskstore-member masks dif-mask)))

      (maskstore-push masks dif-mask) ; Save mask.

      (setf last-state cur-state) ; Set up for next pass.
    )

    ;; Construct result.
    (make-vertexpath :states states :masks masks)
  )
)

;;; Return true if two vertexpaths are equal.
(defun vertexpath-eq (vtxpthx vtxpthy) ; -> bool
  ;; Check arguments.
  (assert (vertexpath-p vtxpthx))
  (assert (vertexpath-p vtxpthy))

  ;; Return result.
  (statestore-eq (vertexpath-states vtxpthx) (vertexpath-states vtxpthy))
)

;;; Return true if a vertexpath is a superset of another.
;;; Superset in terms of eq or superset states.
;;; Proper superset of masks.
(defun vertexpath-superset-of (&key sup sub) ; -> bool
  ;; Check arguments.
  (assert (vertexpath-p sup))
  (assert (vertexpath-p sub))

  ;; Check states.
  (if (statestore-superset-of :sup (vertexpath-states sup) :sub (vertexpath-states sub))
    (return-from vertexpath-superset-of true))

  ;; Check masks.
  (if (> (maskstore-length (vertexpath-masks sup))
         (maskstore-length (vertexpath-masks sub)))
    (maskstore-subset-of :sub (vertexpath-masks sub) :sup (vertexpath-masks sup))
    false
  )
)


(defun vertexpath-str (vtpx)  ; -> string.
  ;; Check argument.
  (assert (vertexpath-p vtpx))

  (let ((strs "(") ; vertexpath prefix.
       )   
    ;; Add vertexpath bits.
    (setf strs (concatenate 'string strs (statestore-str (vertexpath-states vtpx))))
    (setf strs (concatenate 'string strs ", "))
    (setf strs (concatenate 'string strs (maskstore-str (vertexpath-masks vtpx))))
    (setf strs (concatenate 'string strs ")"))

    ;; Return result.
    strs
  )
)

