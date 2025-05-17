;;;; Implement a vertex.
;;;; A pinnacle state, with more than one adjacent states representing samples dissimilar to the pinnacle state.
;;;; The adjacent states may have similar, or dissimilar, samples to each other.
;;;; All states having the same number of bits.

; Implement a vertex.
(defstruct vertex
  pinnacle  ; A state.
  edges     ; A statestore, with GT one states.
)
; Automatically created by defstruct:
;
; Most used:
;   (vertex-<field name> <instance>) -> struct field.
;   (vertex-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> vertex
;   (typep <instance> 'vertex) -> bool
;
; Probably shouldn't use:
;   (make-vertex [:<field-name> <field-vertex>]*), use vertex-new instead.
;   (copy-vertex <instance>) copies a vertex instance.

;;; Return a new vertex instance, from a state and a list of states, or a statestore.
(defun vertex-new (pinnacle edges) ; -> vertex.
  (assert (state-p pinnacle))
  (assert (statestore-p edges))
  (assert (> (statestore-length edges) 0))
  (assert (= (state-num-bits pinnacle) (statestore-num-bits edges)))
  
  ;; Check that edge states are adjacent the pinnacl state.
  (loop for stax in (statestore-states edges) do
    (assert (state-is-adjacent pinnacle stax))
  )
      
  ;; Construct results.
  (make-vertex :pinnacle pinnacle :edges edges)
)

;;; Return the number of edges in a vertex.
(defun vertex-num-edges (vx) ; -> number.
  ;; Check argument.
  (assert (vertex-p vx))

  ;; Calc result.
  (statestore-length (vertex-edges vx))
)

;;; Return the number of bits used in a vertex.
(defun vertex-num-bits (vx) ; -> integer, GT 0.
  ;; Check argument.
  (assert (vertex-p vx))

  ;; Return result.
  (state-num-bits (vertex-pinnacle vx)) 
)

;;; Return true if two vertices are equal.
(defun vertex-eq (vx vy) ; -> Boll.
  ;; Check arguments.
  (assert (vertex-p vx))
  (assert (vertex-p vy))

  ;; Return result.
  (and (state-eq (vertex-pinnacle vx) (vertex-pinnacle vy))
       (= (statestore-length (vertex-edges vx)) (statestore-length (vertex-edges vy))))
)

;;; Return a string representing a vertex.
(defun vertex-str (vx) ; -> string.
  ;; Check argument.
  (assert (vertex-p vx))

  (let ((ret (format nil "(~A -" (state-str (vertex-pinnacle vx)))))

    (loop for stax in (statestore-states (vertex-edges vx)) do
      (setf ret (concatenate 'string ret " "))
      (setf ret (concatenate 'string ret (state-str stax)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret 
  )
)

;;; Return possible regions implied by the vertex.
(defun vertex-structure-implied (vx) ; -> regionstore.
  ;; Check argument.
  (assert (vertex-p vx))

  (let (tmp-regs)
    ;; Init summation of edge complements, (A + B) & (A + C) = A + (B & C).
    (setf tmp-regs (state-complement (car (statestore-states (vertex-edges vx)))))

    (loop for stax in (cdr (statestore-states (vertex-edges vx))) do
      (setf tmp-regs (regionstore-intersection tmp-regs (state-complement stax)))
    )
    ;; Add pinnacle complement to get result.
    (regionstore-union tmp-regs (state-complement (vertex-pinnacle vx)))
  )
)

;;; Return true if a vertex cotians a given state.
(defun vertex-member (vx stax) ; -> bool
  ;; Check arguments.
  (assert (vertex-p vx))
  (assert (state-p stax))

  ;; Check pinnacle state.
  (if (state-eq (vertex-pinnacle vx) stax)
    (return-from vertex-member true)) ; Return positive result.

  ;; Check edges.
  (statestore-member (vertex-edges vx) stax)
)

;;; Return a statestore of states in a vertex.
(defun vertex-states (vx) ; -> statestore.
  ;; Check argument.
  (assert (vertex-p vx))

  ;; Construct result.
  (let ((ret (statestore-new (list (vertex-pinnacle vx)))))
    (statestore-append ret (vertex-edges vx))
  )
)

