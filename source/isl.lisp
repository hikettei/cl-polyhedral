
(in-package :cl-polyhedral)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  A Set of CFFI Bindings for ISL and utils for it.
;; (Complements functions which cl-autowrap couldn't handle with)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defmacro with-isl-ctx (bind &body body)
  `(let ((,bind (cl-isl:isl-ctx-alloc)))
     (unwind-protect
	  (progn ,@body)
       (cl-isl:isl-ctx-free ,bind))))

