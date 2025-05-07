;;;; Implement a store of vertexs.
;;;; Each vertect using the same number of bits, no duplicates.

; Implement a store of vertexs.
(defstruct vertexstore
  vertices  ; A list of zero, or more, vertices.
)
; Automatically created by defstruct:
;
; Most used:
;   (vertexstore-<field name> <instance>) -> struct field.
;   (vertexstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> vertexstore
;   (typep <instance> 'vertexstore) -> bool
;
; Probably shouldn't use:
;   (make-vertexstore [:<field-name> <field-vertexstore>]*), use vertexstore-new instead.
;   (copy-vertexstore <instance>) copies a vertexstore instance.

;;; Return a new vertexstore instance, from a vertex, or a list of vertices.
(defun vertexstore-new (vertices) ; -> vertexstore.
  (let (listx)
    ;; Check argument, convert a vertex to a vertex list.
    (cond ((vertex-p vertices) (setf listx (list vertices)))
          ((listp vertices) (setf listx vertices))
          (t (error "unexpected argument")))
    
    ;; Construct results.
    (let ((rslt (make-vertexstore :vertices nil)))
      (loop for vx in listx do
        (vertexstore-push rslt vx)
      )
      rslt
    )
  )
)

;;; Return the number of vertexs in a vertexstore.
(defun vertexstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (vertexstore-p storex))

  ;; Calc result.
  (length (vertexstore-vertices storex))
)

;;; Return true if a vertex is a member.
(defun vertexstore-member (vstrx vx) ; -> bool
  ;; Check arguments.
  (assert (vertexstore-p vstrx))
  (assert (vertex-p vx))

  ;; Return result.
  (member vx (vertexstore-vertices vstrx) :test #'vertex-eq)
)

;;; Add a vertex to a vertex store.
(defun vertexstore-push (storex vx) ; -> side-effect, vertexstore changed.
  ;; Check arguments.
  (assert (vertexstore-p storex))
  (assert (vertex-p vx))
  (when (vertexstore-is-not-empty storex)
    (assert (= (vertex-num-bits (car (vertexstore-vertices storex)))
               (vertex-num-bits vx))))

  ;; If no duplicate exists in the store, add it.
  (if (not (member vx (vertexstore-vertices storex) :test #'vertex-eq))
     (push vx (vertexstore-vertices storex)))
)

;; Return a string representing a vertexstore.
(defun vertexstore-str (storex) ; -> string
  ;; Check argument.
  (assert (vertexstore-p storex))

  ;; Construct result.
  (let ((ret "(") (start t)) 

    (loop for vrx in (vertexstore-vertices storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (vertex-str vrx)))
    )
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret 
  )
)

;;; Return true if a vertexstore is empty.
(defun vertexstore-is-empty (storex) ; -> bool
  ;; Check argument.
  (assert (vertexstore-p storex))

  ;; Calc result.
  (zerop (vertexstore-length storex))
)

;;; Return true if a vertexstore is not empty.
(defun vertexstore-is-not-empty (storex) ; -> bool                                                     
  ;; Check argument.
  (assert (vertexstore-p storex))

  ;; Calc result.
  (plusp (vertexstore-length storex))
)

;;; Return the structure impled by all vertices in a vertexstore.
(defun vertexstore-structure-implied (storex) ; -> regionstore.
  ;; Check argument.
  (assert (vertexstore-p storex))
  (assert (vertexstore-is-not-empty storex))

  (let ((rslt (vertex-structure-implied (car (vertexstore-vertices storex)))))
    (loop for vtx in (cdr (vertexstore-vertices storex))  do
      (setf rslt (regionstore-intersection rslt (vertex-structure-implied vtx)))
    )
    rslt
  )
)

;;; Return edge states in a region.
(defun vertexstore-edges-in-region (storex regx) ; -> statestore.
  ;; Check arguments.
  (assert (vertexstore-p storex))
  (assert (region-p regx))

  (let ((ret (statestore-new nil)))

    (loop for vtx in (vertexstore-vertices storex)  do
      (loop for stax in (statestore-states (vertex-edges vtx)) do
        (if (region-superset-of-state regx stax)
          (statestore-push ret stax))
      )
    )
    ;; Return result.
    ret
  )
)

;;; Return verticies in region.
;;; An applicable vertex should have the same number of edges as the region.
(defun vertexstore-vertices-in-region (storex regx) ; -> vertexstore.
  ;; Check arguments.
  (assert (vertexstore-p storex))
  (assert (region-p regx))

  (let ((ret (vertexstore-new nil)))

    (loop for vtx in (vertexstore-vertices storex)  do
      (if (region-superset-of-state regx (vertex-pinnacle vtx))
          (vertexstore-push ret vtx))
    )
    ;; Return result.
    ret
  )
)

;;; Return vertices that cantain a given state.
(defun vertexstore-vertices-containing-state (storex stax) ; -> vertexstore.
  ;; Check arguments.
  (assert (vertexstore-p storex))
  (assert (state-p stax))

  (let ((ret (vertexstore-new nil)))
    ;; Check each vertex.
    (loop for vx in (vertexstore-vertices storex) do
  
      (if (vertex-contains-state vx stax)
        (vertexstore-push ret vx))
    )
    ;; Return result.
    ret
  )
)

;;; Return vertices that cantain a given state.
(defun vertexstore-find (storex stax) ; -> vertex, or nil.
  ;; Check arguments.
  (assert (vertexstore-p storex))
  (assert (state-p stax))

  ;; Check each vertex.
  (loop for vx in (vertexstore-vertices storex) do
  
    (if (state-eq (vertex-pinnacle vx) stax)
      (return-from vertexstore-find vx)) ; Return positive result.
  )
  ;; Return negative result.
  nil
)

;;; Return the difference of two vertexstores.
(defun vertexstore-difference (storex storey) ; -> vertexstore.
  (assert (vertexstore-p storex))
  (assert (vertexstore-p storey))

  (vertexstore-new (set-difference (vertexstore-vertices storex) (vertexstore-vertices storey) :test #'vertex-eq))
)

;;; Return the states connected to a given state though vertices.
;;; The given state will be the first in the result list.
(defun vertexstore-states-connected (storex stax) ; -> statestore.
  (assert (vertexstore-p storex))
  (assert (state-p stax))

  (let (
        ;; Current vertices not connected to yet.
        (vertices storex)
        ;; List of new states to process.
        (new-states (statestore-new (list stax)))
        ;; List of processed states.
        (processed (statestore-new nil))
        ;; Verticies the current state is in.
        verts-state-in
        ;; Union of states in processed and new-states statestores.
        all-stored-states
        ;; Current state to use to look for vertex connections.
        cur-state
       )

    (loop

      ;; Get next state to process.
      (setf cur-state (statestore-pop new-states))
      (statestore-push processed cur-state)

      ;; Get verticies the state is in.
      (setf verts-state-in (vertexstore-vertices-containing-state vertices cur-state))

      (when (vertexstore-is-not-empty verts-state-in)

        ;; Take found vertices out of the working list.
        (setf vertices (vertexstore-difference vertices verts-state-in))

        ;; Process each vertex.
        (loop for vtx in (vertexstore-vertices verts-state-in) do
          ;; Get all current states.
          (setf all-stored-states (statestore-union processed new-states))
          ;; Get states in vertex not currently stored.
          (setf new-states (statestore-union new-states (statestore-difference (vertex-states vtx) all-stored-states)))
        )
      )

      ;;  Check for no more verticies to process.
      (if (vertexstore-is-empty vertices)
        (return-from vertexstore-states-connected (statestore-reverse (statestore-union new-states processed)))
      )

      ;; Check for no more states to process.
      (if (statestore-is-empty new-states)
        (return-from vertexstore-states-connected (statestore-reverse processed))
      )
    ) ; next new square.
  )
)

