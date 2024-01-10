
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

(declaim (inline))

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

(defcfun ("isl_schedule_from_domain"
	  %isl-schedule-from-domain)
    :pointer
  (schedule :pointer))

(defun isl-schedule-from-domain (union-set)
  (declare (type cl-isl:isl-union-set union-set))
  (%isl-schedule-from-domain
   (cl-isl::isl-union-set-ptr union-set)))

(defcfun ("isl_schedule_sequence"
	  %isl-schedule-sequence)
    :pointer
  (x :pointer)
  (y :pointer))

(defun isl-schedule-sequence (x y)
  (%isl-schedule-sequence x y))

(defcfun ("isl_multi_union_pw_aff_read_from_str"
	  %isl-multi-union-pw-aff-read-from-str)
    :pointer
  (ctx :pointer)
  (x   :string))

(defun isl-multi-union-pw-aff-read-from-str (ctx x)
  (declare (type cl-isl::isl-ctx ctx))
  (%isl-multi-union-pw-aff-read-from-str
   (cl-isl::isl-ctx-ptr ctx)
   x))

(defcfun ("isl_schedule_insert_partial_schedule"
	  %isl-schedule-insert-partial-schedule)
    :pointer
  (ctx :pointer)
  (schedule :pointer))

(defun isl-schedule-insert-partial-schedule (ctx schedule)
  (declare (type cl-isl::isl-ctx ctx))
  (%isl-schedule-insert-partial-schedule
   (cl-isl::isl-ctx-ptr ctx)
   schedule))
