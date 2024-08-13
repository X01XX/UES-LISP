;;;; Implement the sample struct.
;;;; It represents a initial state and a result of some action.
(defstruct (sample (:print-function sample-print))
  initial ; A state instance, before an action.
  action  ; Action number used to get sample.
  result  ; A state instance, after an action.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (sample-<field name> <instance>) -> struct field.
;   (sample-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> sample
;   (typep <instance> 'sample) -> bool
;
; Probably shouldn't use:
;   (make-sample [:<field-name> <field-sample>]*), use sample-new instead.
;   (copy-sample <instance>) copies a sample instance.
(defun sample-new (&key initial action result) ; -> sample instance.
  (assert (state-p initial))
  (assert (integerp action))
  (assert (>= action 0))
  (assert (state-p result))
  (assert (= (state-num-bits initial) (state-num-bits result)))

  (make-sample :initial initial :action action :result result)
)

; Return a string to represent a sample instance.
(defun sample-str (smpl) ; -> string.
  (format nil "#S(SAMPLE ~A-~D->~A)" (sample-initial smpl) (sample-action smpl) (sample-result smpl))
)

; Print a sample.
(defun sample-print (instance stream depth)
  ;(assert (zerop depth))
  (format stream (sample-str instance))
)

