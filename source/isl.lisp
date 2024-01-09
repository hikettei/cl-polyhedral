
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

(defcfun ("isl_union_map_read_from_str"
	  %isl-union-map-read-from-str)
    :pointer
  (ctx :pointer)
  (x :string))

(defun isl-union-map-read-from-str (ctx str)
  (declare (type cl-isl:isl-ctx ctx)
	   (type string str))
  (%isl-union-map-read-from-str
   (cl-isl::isl-ctx-ptr ctx)
   str))
