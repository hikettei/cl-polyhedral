
(in-package :cl-polyhedral)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  A Set of CFFI Bindings for ISL and utils for it.
;; (Which complements the lack of functions in the cl-isl binding)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~ ISL-CTX ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct isl-ctx ptr)
(defcfun ("isl_ctx_alloc" %isl-ctx-alloc) :pointer)

(declaim (ftype (function () isl-ctx) isl-ctx-alloc))
(defun isl-ctx-alloc () (make-isl-ctx :ptr (%isl-ctx-alloc)))

(defcfun ("isl_ctx_free" %isl-ctx-free) :void
  (ctx :pointer))

(defun isl-ctx-free (ctx) (%isl-ctx-free (isl-ctx-ptr ctx)))

(defmacro with-isl-ctx (bind &body body)
  `(let ((,bind (isl-ctx-alloc)))
     (declare (type isl-ctx ,bind))
     (unwind-protect
	  (progn ,@body)
       (isl-ctx-free ,bind))))

;; ~ ISL-SET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct isl-set ptr)
(defcfun ("isl_set_read_from_str" %isl-set-read-from-str) :pointer
  (ctx :pointer)
  (str :string))

(defun isl-set-read-from-str (ctx str)
  (declare (type isl-ctx ctx)
	   (type string str))
  (make-isl-set
   :ptr
   (%isl-set-read-from-str (isl-ctx-ptr ctx) str)))

;; ~ ISL-UNION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct isl-union-set ptr)
(defcfun ("isl_union_set_read_from_str"
	  %isl-union-set-read-from-str)
    :pointer
  (ctx :pointer)
  (str :string))

(defun isl-union-set-read-from-str (ctx str)
  (declare (type isl-ctx ctx)
	   (type string str))
  (make-isl-union-set
   :ptr
   (%isl-union-set-read-from-str (isl-ctx-ptr ctx) str)))

(defcfun ("isl_union_map_dump"
	  %isl-union-map-dump)
    :void
  (union-map :pointer))

(defcfun ("isl_union_set_copy"
	  %isl-union-set-copy)
    :pointer
  (set :pointer))

(defcfun ("isl_schedule_constraints_set_validity"
	  %isl-schedule-constraints-set-validity)
    :pointer
  (schedule :pointer)
  (map :pointer))

(defcfun ("isl_schedule_constraints_set_proximity"
	  %isl-schedule-constraints-set-proximity)
    :pointer
  (schedule :pointer)
  (map :pointer))

(defcfun ("isl_union_map_union"
	  %isl-union-map-union)
    :pointer
  (map1 :pointer)
  (map2 :pointer))
  
(defcfun ("isl_union_map_read_from_str"
	  %isl-union-map-read-from-str)
    :pointer
  (ctx :pointer)
  (x :string))

(defun isl-union-map-read-from-str (ctx str)
  (declare (type isl-ctx ctx)
	   (type string str))
  (make-isl-union-set
   :ptr
   (%isl-union-map-read-from-str
    (isl-ctx-ptr ctx)
    str)))

(defcfun ("isl_union_map_copy"
	  %isl-union-map-copy)
    :pointer
  (map :pointer))

(defun isl-union-map-copy (set)
  (make-isl-union-set :ptr (%isl-union-map-copy (isl-union-set-ptr set))))

(defcfun ("isl_union_access_info_from_sink"
	  %isl-union-access-info-from-sink)
    :pointer
  (map :pointer))

(defun isl-union-access-info-from-sink (set)
  (%isl-union-access-info-from-sink (isl-union-set-ptr set)))

(defcfun ("isl_union_access_info_set_must_source"
	  %isl-union-access-info-set-must-source)
    :pointer
  (access :pointer)
  (map :pointer))


(defcfun ("isl_union_access_info_set_schedule"
	  %isl-union-access-info-set-schedule)
    :pointer
  (access :pointer)
  (map :pointer))

(defcfun ("isl_union_access_info_compute_flow"
	  %isl-union-access-info-compute-flow)
    :pointer
  (access :pointer))

(defcfun ("isl_union_flow_get_must_dependence"
	  %isl-union-flow-get-must-dependence)
    :pointer
  (access :pointer))

;; ~~ Schedules ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defcfun ("isl_schedule_from_domain"
	  %isl-schedule-from-domain)
    :pointer
  (schedule :pointer))

(defun isl-schedule-from-domain (union-set)
  (declare (type isl-union-set union-set))
  (%isl-schedule-from-domain
   (isl-union-set-ptr union-set)))

(defcfun ("isl_schedule_constraints_on_domain"
	  %isl-schedule-constraints-on-domain)
    :pointer
  (schedule :pointer))

(defcfun ("isl_schedule_sequence"
	  %isl-schedule-sequence)
    :pointer
  (x :pointer)
  (y :pointer))

(defcfun "isl_schedule_copy" :pointer
  (schedule :pointer))

(defcfun ("isl_union_access_info_set_may_source"
	  %isl-union-access-info-set-may-source)
    :pointer
  (access   :pointer)
  (may-read :pointer))

(defcfun ("isl_union_flow_get_may_dependence"
	  %isl-union-flow-get-may-dependence)
    :pointer
  (flow :pointer))

(defun isl-schedule-sequence (x y)
  (%isl-schedule-sequence x y))

(defcfun ("isl_multi_union_pw_aff_read_from_str"
	  %isl-multi-union-pw-aff-read-from-str)
    :pointer
  (ctx :pointer)
  (x   :string))

(defun isl-multi-union-pw-aff-read-from-str (ctx x)
  (declare (type isl-ctx ctx))
  (%isl-multi-union-pw-aff-read-from-str
   (isl-ctx-ptr ctx)
   x))

(defcfun ("isl_schedule_insert_partial_schedule"
	  %isl-schedule-insert-partial-schedule)
    :pointer
  (ctx :pointer)
  (schedule :pointer))

(defun isl-schedule-insert-partial-schedule (ctx schedule)
  (declare (type isl-ctx ctx))
  (%isl-schedule-insert-partial-schedule
   (isl-ctx-ptr ctx)
   schedule))

(defcfun ("isl_ast_build_from_context"
	  %isl-ast-build-from-context)
    :pointer
  (set :pointer))

(defun isl-ast-build-from-context (set)
  (declare (type isl-set set))
  (%isl-ast-build-from-context
   (isl-set-ptr set)))

(defcfun "isl_ast_build_node_from_schedule"
    :pointer
  (x :pointer)
  (y :pointer))

(defcfun "isl_ast_node_get_ctx" :pointer
  (ast :pointer))

(defcfun "isl_printer_to_str" :pointer
  (printer :pointer))

(defcfun "isl_printer_set_output_format" :pointer
  (printer :pointer)
  (format :int))

(defcfun "isl_printer_print_ast_node" :pointer
  (p :pointer)
  (ast :pointer))

(defcfun "isl_printer_get_str" :string
  (ast :pointer))

(defcfun ("isl_schedule_node_n_children"
	  %isl-schedule-node-n-children)
    :int
  (ptr :pointer))

(defcenum :isl-schedule-node-type
  (:isl_schedule_node_error 1)
  :isl_schedule_node_band
  :isl_schedule_node_context
  :isl_schedule_node_domain
  :isl_schedule_node_expansion
  :isl_schedule_node_extension
  :isl_schedule_node_filter
  :isl_schedule_node_leaf
  :isl_schedule_node_guard
  :isl_schedule_node_mark
  :isl_schedule_node_sequence
  :isl_schedule_node_set)

(defcenum :isl-dim-type
  :isl_dim_cst
  :isl_dim_param
  :isl_dim_in
  :isl_dim_out
  :isl_dim_set
  :isl_dim_div
  :isl_dim_all)


