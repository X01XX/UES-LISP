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

;;; Return vertices that contain a given state.
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

;;; Return all states in the vertices in a vertexstore.
(defun vertexstore-states (storex) ; -> statestore.
  ;; Check argument.
  (assert (vertexstore-p storex))

  (let ((ret (statestore-new nil)))
    ;; Gather states, no duplicates.
    (loop for vtx in (vertexstore-vertices storex) do
      ;; Add vertex pinnacle.
      (if (not (statestore-member ret (vertex-pinnacle vtx)))
        (statestore-push ret (vertex-pinnacle vtx)))

      ;; Add vertex adjacent, external, states.
      (loop for stax in (statestore-states (vertex-states vtx)) do
        (if (not (statestore-member ret stax))
          (statestore-push ret  stax))
      )
    )
    ;; Return result.
    ret
  )
)

