;;;; Implement a store of vertexpaths.

(defstruct vertexpathstore
  vertexpaths  ; A list of zero, or more, vertexpaths.
)
; Automatically created by defstruct:
;
; Most used:
;   (vertexpathstore-<field name> <instance>) -> struct field.
;   (vertexpathstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> vertexpathstore
;   (typep <instance> 'vertexpathstore) -> bool
;
; Probably shouldn't use:
;   (make-vertexpathstore [:<field-name> <field-vertexpathstore>]*), use vertexpathstore-new instead.
;   (copy-vertexpathstore <instance>) copies a vertexpathstore instance.

;;; Return a new vertexpathstore instance, from a vertexpath, or a list of vertexpaths.
(defun vertexpathstore-new (vertexpaths) ; -> vertexpathstore.
  (let (listx)
    ;; Check argument, convert a vertexpath to a vertexpath list.
    (cond ((vertexpath-p vertexpaths) (setf listx (list vertexpaths)))
          ((listp vertexpaths) (setf listx vertexpaths))
          (t (error "unexpected argument")))
    
    (loop for vtxpthx in listx do
      (assert (vertexpath-p vtxpthx))
    )

    ;; Construct results.
    (make-vertexpathstore :vertexpaths listx)
  )
)

;;; Return the number of vertexpaths in a vertexpathstore.
(defun vertexpathstore-length (storex) ; -> number.
  ;; Check argument.
  (assert (vertexpathstore-p storex))

  ;; Calc result.
  (length (vertexpathstore-vertexpaths storex))
)

;;; Return true if a vertexpathstore contains a given vertexpath.
(defun vertexpathstore-member (storex vtpx) ; -> bool 
  ;; Check arguments.
  (assert (vertexpathstore-p storex))
  (assert (vertexpath-p vtpx))

  ;; Calc result.
  (member vtpx (vertexpathstore-vertexpaths storex) :test #'vertexpath-eq)
)

;;; Push vertexpath into a vertexpathstore.
(defun vertexpathstore-push (storex vrtstrx) ; -> nothing, side-effect vertexpathstore is changed.
  ;; Check arguments.
  (assert (vertexpathstore-p storex))
  (assert (vertexpath-p vrtstrx))

  ;; Add vertexpath.
  (push vrtstrx (vertexpathstore-vertexpaths storex))
)

;;; Add a vertexpath to a vertexpathstore if there are no vertexpaths that are a states-superset,
;;; or masks-proper-subsets.
;;; If so, delete subsets of new vertexpath.
(defun vertexpathstore-push-nosubs (storex vtxpthx) ; -> bool, true if vertexpathstore is changed.
  ;; Check arguments.
  (assert (vertexpathstore-p storex))
  (assert (vertexpath-p vtxpthx))

  ;; Check for vertexpath in store that is a superset (or dup) states, or proper superset masks, of the new vertexpath.
  (loop for vtxpthy in (vertexpathstore-vertexpaths storex) do
    (when (vertexpath-superset-of :sup vtxpthy :sub vtxpthx)
      ;(format t "~&vertexpath ~A subset of ~A, not adding" (vertexpath-str vtxpthx) (vertexpath-str vtxpthy))
      (return-from vertexpathstore-push-nosubs false)) ;; Return negative result.
  )

  ;; Check for vertexpaths that are a subset of the new vertexpath.
  (let (del-vtxpths)
    ;; Find vertexpaths that are a subset of the new vertexpath.
    (loop for vtxpthy in (vertexpathstore-vertexpaths storex) do
      (if (vertexpath-superset-of :sup vtxpthx :sub vtxpthy)
        (push vtxpthy del-vtxpths)
      )
    )
    ;; Remove the subset vertexpaths.
    (loop for vtxpthy in del-vtxpths do
      ;(format t "~&vertexpath ~A subset of ~A, removing" (vertexpath-str vtxpthy) (vertexpath-str vtxpthx))
      (setf (vertexpathstore-vertexpaths storex) (remove vtxpthy (vertexpathstore-vertexpaths storex) :test #'vertexpath-eq))
    )
  )

  ;; Add the vertexpath.
  (vertexpathstore-push storex vtxpthx)
  ;; Return positive result.
  true
)

;;; Return a string reprsenting a vertexpatnstore.
(defun vertexpathstore-str (storex) ; -> string
  ;; Check argument.
  (assert (vertexpathstore-p storex))

  (let ((ret "(") (start t)) 
    (loop for vtpx in (vertexpathstore-vertexpaths storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret " ")))

      (setf ret (concatenate 'string ret (vertexpath-str vtpx)))
    )   
    (setf ret (concatenate 'string ret ")"))

    ;; Return result.
    ret
  )
)

;;; Return a list a unique maskstores.
(defun vertexpathstore-unique-masks (storex) ; -> list of maskstores.
  ;; Check argument.
  (assert (vertexpathstore-p storex))

  (let (ret)
    ;; Check each vertexpath maskstore.
    (loop for vtpx in (vertexpathstore-vertexpaths storex) do
      (if (null (member (vertexpath-masks vtpx) ret :test #'maskstore-eq))
        (push (vertexpath-masks vtpx) ret))
    )
    ;; Return result.
    ret
  )
)

