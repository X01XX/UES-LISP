;;;; Implement the defining struct and functions.                                                                       

;;; The defining struct.
(defstruct defining
  region    ; A region with at least one state in only one group.
  vertex    ; A vertex, defining the region, and affecting the whole logical structure.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (defining-<field name> <instance>) -> struct field.
;   (defining-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> defining
;   (typep <instance> 'defining) -> bool
;
; Probably shouldn't use:
;   (make-defining [:<field-name> <field-defining>]*), use defining-new instead.
;   (copy-defining <instance>) copies a defining instance.

;;; Return a new defining, made up of one, or more, states.
(defun defining-new (regx vertx) ; -> defining.
  ;; Check arguments.
  (assert (region-p regx))
  (assert (vertex-p vertx))
  (assert (region-superset-of-state regx (vertex-pinnacle vertx)))
  (assert (= (mask-num-ones (region-edge-mask regx)) (vertex-num-edges vertx)))

  (loop for stax in (statestore-states (vertex-edges vertx)) do
    (assert (state-is-adjacent stax (vertex-pinnacle vertx)))
    (assert (not (region-superset-of-state regx stax)))
  )

  ;; Construct result.
  (make-defining :region regx :vertex vertx)
)

;; Return a string for a defining struct.
(defun defining-str (defx)  ; -> string.
  ;; Check argument.
  (assert (defining-p defx))

  ;; Return result.
  (format nil "d(~A ~A)" (region-str (defining-region defx)) (vertex-str (defining-vertex defx)))
)

;; Return true if a state is needed.
(defun defining-state-needed (defx stax) ; -> bool
  ;; Check arguments.
  (assert (defining-p defx))
  (assert (state-p stax))

  ;; Return result.
  (vertex-member (defining-vertex defx) stax)
)
