
(in-package :cl-polyhedral)

(declaim (ftype (function (t) fixnum) get-tile-dim))
(defun get-tile-dim (ex &aux (max-dim 0))
  (declare (type fixnum max-dim)
	   (optimize (speed 3)))
  (typecase ex
    (Kernel
     (loop for inst across (kernel-instructions ex) do
       (setf max-dim (apply #'max max-dim (map 'list #'get-tile-dim (coerce (inst-sources inst) 'list)))))
     max-dim)
    (list
     (trivia:ematch ex
       ((list 'aref _ ref)
	(length (the list ref)))))   
    (T
     0)))

(defmacro with-inlined-foreign-funcall-mode (&body body)
  "Enables %\"foreign-name\":return-dtype &rest args syntax under the body execution"
  `(macrolet ((% (fname return-type &rest args)
		`(foreign-funcall ,fname ,@args ,return-type)))
     ,@body))

(defun tiling-sizes (n band ctx element-n-byte)
  "Get the best tiling size for the current environment"
  (declare (type fixnum n element-n-byte)
	   (type isl-ctx ctx)
	   (type cffi:foreign-pointer band))
  (with-inlined-foreign-funcall-mode
    ;; i.g. it fails to cache
    ;; needs more explore/benchmarks
    
    ;; [TODO] Always using the largest cache-size?
    ;; what if L1 sizes are different across multiple cores like Apple Silicon.
    ;; Set = #'min and performs the best speed (helps to reduce the number of cache-miss)
    (let* ((linesize   (apply #'min *L1-Cache*)) ;; Byte
	   (linesize   (floor linesize element-n-byte))
	   (tilesize   (floor (min linesize (expt linesize (/ n)))))
	   (band-space (%"isl_schedule_node_band_get_space":pointer :pointer band))
	   (dim        (%"isl_space_dim":int :pointer band-space :int 3))
	   (tiles      (%"isl_val_list_alloc":pointer :pointer (isl-ctx-ptr ctx) :int dim)))
      (loop for i upfrom 1 to dim do
	(%"isl_val_list_add":void :pointer tiles
				  :pointer
				  (%"isl_val_int_from_si":pointer :pointer (isl-ctx-ptr ctx) :int tilesize)))
      (%"isl_multi_val_from_val_list":pointer :pointer band-space :pointer tiles))))


;; Ref: https://github.com/JuliaLabs/Poly.jl/blob/master/src/tiling.jl#L32
(declaim (ftype (function (foreign-pointer isl-ctx) (values foreign-pointer foreign-pointer)) shift-band-zero))
(defun shift-band-zero (band ctx)
  "shift the band to start at 0 so that full tiling occurs"
  (declare (type isl-ctx ctx))
  ;; inlined-foreign-funcall-mode:
  ;;   Allows a syntax of: (%"CFFI_FUNCTION_NAME":RETURN_TYPE  :TYPE ARG1 :TYPE ARG2 ...)
  (with-inlined-foreign-funcall-mode
    (let* ((domain           (%"isl_schedule_node_get_domain":pointer       :pointer band))          ;; band.domains()
	   (partial-schedule (%"isl_schedule_node_band_get_partial_schedule":pointer :pointer band)) ;; band.partial_schedules()
	   (mupa (%"isl_multi_union_pw_aff_intersect_domain":pointer                                 ;; intersects(copy(partial-schedule), copy(domain))
		   :pointer
		   (%"isl_multi_union_pw_aff_copy":pointer    :pointer partial-schedule)
		   :pointer
		   (%"isl_union_set_copy":pointer             :pointer domain)))
	   (n         (%"isl_multi_union_pw_aff_size":int     :pointer mupa))                        ;; mupa.union_pw_aff_size()
	   (multi-val (%"isl_multi_union_pw_aff_min_multi_val":pointer :pointer mupa)))              ;; 
      (loop for i upfrom 0 below n
	    for v = (%"isl_multi_val_get_val":pointer  :pointer multi-val :int i) ;; multi_val.get(i)
	    do (when (%"isl_val_is_neginfty":boolean        :pointer v)
		 (let ((new-v (%"isl_val_int_from_si":pointer        :pointer (isl-ctx-ptr ctx) :int 1)))
		   (setf multi-val (%"isl_multi_val_set_val":pointer :pointer multi-val :int i :pointer new-v))))
	       (%"isl_val_free":void :pointer v))

      ;; get shift mupa
      (let* ((shift (%"isl_multi_union_pw_aff_multi_val_on_domain":pointer  :pointer domain :pointer multi-val))
	     (shift-neg (%"isl_multi_union_pw_aff_neg":pointer :pointer  (%"isl_multi_union_pw_aff_copy":pointer :pointer shift)))
	     (partial-schedule (%"isl_multi_union_pw_aff_add":pointer :pointer partial-schedule :pointer shift-neg)))
	(values partial-schedule shift)))))

(defun tile-partial-schedule (partial-schedule sizes)
  (declare (type foreign-pointer partial-schedule sizes))
  (with-inlined-foreign-funcall-mode
    (let* ((ctx (%"isl_multi_val_get_ctx":pointer :pointer sizes))
	   (scale (%"isl_options_get_tile_scale_tile_loops":boolean :pointer ctx))
	   (n     (%"isl_multi_union_pw_aff_size":int :pointer partial-schedule)))
      (loop for i upfrom 0 below n
	    for upa = (%"isl_multi_union_pw_aff_get_union_pw_aff":pointer :pointer partial-schedule :int i)
	    for v   = (%"isl_multi_val_get_val":pointer :pointer sizes :int i) do
	      (let* ((upa (%"isl_union_pw_aff_scale_down_val":pointer
			    :pointer upa
			    :pointer (%"isl_val_copy":pointer :pointer v)))
		     (upa (%"isl_union_pw_aff_floor":pointer :pointer upa)))
		(when scale ;;  Is it legal???? 
		  (setf upa (%"isl_union_pw_aff_scale_val":pointer
			      :pointer upa
			      :pointer (%"isl_val_copy":pointer :pointer v))))
		(%"isl_val_free":void :pointer v)
		(setf partial-schedule (%"isl_multi_union_pw_aff_set_union_pw_aff":pointer
					 :pointer partial-schedule
					 :int i
					 :pointer upa))))
      partial-schedule)))	 

(declaim (ftype (function (fixnum foreign-pointer isl-ctx fixnum) foreign-pointer) tile-band))
(defun tile-band (n band ctx element-n-byte)
  "Tiles a band node with the tile dimension n"
  (declare (type isl-ctx ctx)
	   (type foreign-pointer band))
  (with-inlined-foreign-funcall-mode
    (multiple-value-bind (partial-schedule shift) (shift-band-zero band ctx)
      (let* ((multi-val        (tiling-sizes n band ctx element-n-byte))
	     (partial-schedule (tile-partial-schedule partial-schedule multi-val))
	     (partial-schedule (%"isl_multi_union_pw_aff_add":pointer    :pointer partial-schedule :pointer shift))
	     (band (%"isl_schedule_node_insert_partial_schedule":pointer :pointer band :pointer partial-schedule)))
	(%"isl_schedule_node_get_schedule":pointer :pointer band)))))
	     

;; Reference: https://github.com/JuliaLabs/Poly.jl/blob/master/src/tiling.jl#L140
(defun tile-schedule (kernel schedule ctx element-n-byte)
  "Returns a new tiled schedule."
  (declare (type Kernel kernel)
	   (type isl-ctx ctx)
	   (type foreign-pointer schedule))
  (with-inlined-foreign-funcall-mode
    (let* ((tile-schedule schedule)
	   (n (get-tile-dim kernel))
	   (root (%"isl_schedule_get_root":pointer :pointer schedule))
	   (node root)
	   (next-nodes nil))
      (loop named find-tiling
	    while (> (%"isl_schedule_node_n_children":int   :pointer node) 0) do
	      (loop for i fixnum upfrom 0 below (%"isl_schedule_node_n_children":int  :pointer node)
		    for band = (%"isl_schedule_node_get_child":pointer  :pointer node :int i)
		    if (eql (%"isl_schedule_node_get_type":int          :pointer band) 0) do		      
		      (setf tile-schedule (tile-band n band ctx element-n-byte))
		      (let ((child (%"isl_schedule_node_get_child":pointer :pointer band :int 0)))
			(setf next-nodes (append next-nodes (list child)))
			(return-from find-tiling))
		    else do
		      (setf next-nodes (append next-nodes (list band))))
	      (when (= 0 (length next-nodes))
		(return-from find-tiling))
	      (setf node (car (last next-nodes)))
	      (setf next-nodes (butlast next-nodes)))
      (values tile-schedule n))))

