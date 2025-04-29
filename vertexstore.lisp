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

;;; Return a new vertexstore instance, from a vertex, or a list of vertexs.
(defun vertexstore-new (vertices) ; -> vertexstore.
  (let (listx)
    ;; Check argument, convert a vertex to a vertex list.
    (cond ((vertex-p vertices) (setf listx (list vertices)))
          ((listp vertices) (setf listx vertices))
          (t (error "unexpected argument")))
    
    ;; Check each list item.
    (loop for vx in listx do
      (assert (vertex-p vx))
    )
    
    ;; Check for same num bits used across vertices.
    (when (> (length listx) 1)
      (let ((num-bits (vertex-num-bits (car listx))))
        (loop for stastorex in (cdr listx) do
          (assert (= (vertex-num-bits stastorex) num-bits))
        )
      )
    )
  
    (let ((retvert (make-vertexstore :vertices nil)))
      ;; Construct results.
      (loop for vx in listx do
        (vertexstore-push retvert vx)
      )
      retvert
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
  (assert (vertexstore-p storex))
  (assert (vertex-p vx))

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

