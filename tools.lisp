; Extraneous useful functions.

(defun bool-p (val)
  (or (eq val true) (eq val false))
)

; The opposite of eq.
(defun neq (arg1 arg2) ; -> bool
  (not (eq arg1 arg2))
)
