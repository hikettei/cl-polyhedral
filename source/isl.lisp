
(in-package :cl-polyhedral)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  A Set of CFFI Bindings for ISL and utils for it.
;; (Which complements the lack of functions in the cl-isl binding)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defmacro with-isl-ctx (bind &body body)
  `(let ((,bind (cl-isl:isl-ctx-alloc)))
     (unwind-protect
	  (progn ,@body)
       (cl-isl:isl-ctx-free ,bind))))

