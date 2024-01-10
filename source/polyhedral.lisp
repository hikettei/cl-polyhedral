
(cl:in-package :cl-polyhedral)

(defparameter *verbose* 0 "
Indicates the level of logging. 0 or 1 or 2

0 -> Ignores all warnings
1 -> Puts warnings
2 -> Display before/after schedules
3 -> Displays all progresses")

;; TODO: Adding options;

(defun run-polyhedral
    (kernel
     &key
       (tile 0)
       (verbose *verbose*))
  "Gains the optimized kernel obtained from transforming the given kernel using Polyhedral Model.
Does the following:
- 1. Creates a context and space for ISL
- 
"
  (declare (type Kernel kernel)
	   (type (integer 0 3) verbose))

  (macrolet ((with-verbose-level ((number) &body body) `(when (>= verbose ,number) ,@body)))
    (with-isl-ctx ctx
      
      (foreign-funcall
       "isl_options_set_on_error"
       :pointer (isl-ctx-ptr ctx)
       :int (if (= verbose 0) 0 1)
       :void)
      
      (let* ((initial-problem (Kernel->ISL kernel))
	     (instructions    (isl-union-set-read-from-str ctx initial-problem)))
	(declare (type isl-union-set instructions))

	(multiple-value-bind (may-read must-write)
	    (access-isl-rep kernel)
	  
	  (with-verbose-level (3)
	    (format t "~%Original Read/Write Dependencies(verbose=3):~%may_read:~%~a~%must_write:~%~a~%"
		    may-read
		    must-write))
	  
	  (multiple-value-bind
		(may-read must-write)
	      (values
	       (isl-union-map-read-from-str ctx may-read)
	       (isl-union-map-read-from-str ctx must-write))
	    
	    (let ((schedule
		    (schedule-tree-isl-rep
		     ctx
		     (aref (kernel-domains kernel) 0)
		     kernel)))
	      
	      (with-verbose-level (3)
		;; Displaying the initial schedules
		(format t "~%Original Scheduling(verbose=3):~%")
		(foreign-funcall
		 "isl_schedule_dump"
		 :pointer schedule
		 :void))
		
	      
	      (with-verbose-level (2)
		;; Displaying The Original C Code
		(let* ((space (isl-set-read-from-str ctx "{:}"))
		       (build (isl-ast-build-from-context space))
		       (ast   (isl-ast-build-node-from-schedule build (isl-schedule-copy schedule)))
		       (c     (isl-ast-node-get-ctx ast))
		       (p     (isl-printer-to-str c))
		       (p     (isl-printer-set-output-format p 4)) ;; 4 indicates C
		       (q     (isl-printer-print-ast-node p ast))
		       (str   (isl-printer-get-str q)))
		  (format t "~%Original C Code(verbose=2):~%~a~%" str)))

	      ;; Dependency Analysis	      
	      (let* (;; 1. RAW (Read After Write), a=1 then b=a
		     (access
		       (%isl-union-access-info-from-sink
			(isl-union-set-ptr (isl-union-map-copy may-read))))		     
		     (access
		       (%isl-union-access-info-set-must-source
			access
			(isl-union-set-ptr (isl-union-map-copy must-write))))
		     (access
		       (%isl-union-access-info-set-schedule
			access
			(isl-schedule-copy schedule)))
		     (flow
		       (%isl-union-access-info-compute-flow
			access))
		     (raw-deps
		       (%isl-union-flow-get-must-dependence
			flow))
		     
		     ;; 2. WAR (Write After Read) deps
		     (access
		       (%isl-union-access-info-from-sink
			(isl-union-set-ptr (isl-union-map-copy must-write))))
		     (access
		       (%isl-union-access-info-set-must-source
			access
			(isl-union-set-ptr must-write)))
		     (access
		       (%isl-union-access-info-set-may-source
			access
			(isl-union-set-ptr may-read)))
		     (access
		       (%isl-union-access-info-set-schedule
			access
			schedule))
		     (flow
		       (%isl-union-access-info-compute-flow
			access))
		     (waw-deps
		       (%isl-union-flow-get-must-dependence
			flow))
		     (war-deps
		       (%isl-union-flow-get-may-dependence
			flow)))
		
		(with-verbose-level (3)
		  (format t "~%RAW Dependencies:~%")
		  (%isl-union-map-dump raw-deps)
		  (format t "~%WAW Dependencies:~%")
		  (%isl-union-map-dump waw-deps)
		  (format t "~%WAR Dependencies:~%")
		  (%isl-union-map-dump war-deps))

		(macrolet ((set-option (name level)
			     `(foreign-funcall ,name
					       :pointer (isl-ctx-ptr ctx)
					       :int ,level
					       :void)))
		  (set-option "isl_options_set_schedule_maximize_band_depth" 1)
		  (set-option "isl_options_set_schedule_whole_component" 1)
		  (set-option "isl_options_set_schedule_treat_coalescing" 1)
		  (set-option "isl_options_set_tile_scale_tile_loops" 1)
		  ;; More ...
		  )

		(let* ((all-deps
			 (%isl-union-map-union waw-deps war-deps))
		       (all-deps
			 (%isl-union-map-union all-deps raw-deps))
		       (schedule-constraints
			 (%isl-schedule-constraints-on-domain
			  (%isl-union-set-copy
			   (isl-union-set-ptr instructions))))
		       (schedule-constraints
			 (%isl-schedule-constraints-set-validity
			  schedule-constraints
			  (%isl-union-map-copy all-deps)))
		       (schedule-constraints
			 (%isl-schedule-constraints-set-proximity
			  schedule-constraints
			  (%isl-union-map-copy all-deps))))

		  (with-verbose-level (3)
		    (format t "~%Schedule Constraints:~%")
		    (foreign-funcall
		     "isl_schedule_constraints_dump"
		     :pointer schedule-constraints
		     :void))

		  ;; Tiling
		  (let* ((schedule
			   (foreign-funcall
			    "isl_schedule_constraints_compute_schedule"
			    :pointer schedule-constraints
			    :pointer)))
		    (with-verbose-level (3)
		      (format t "~% Schedule Per Tiling:~%")
		      (foreign-funcall
		       "isl_schedule_dump"
		       :pointer schedule
		       :void))
		    
		    (let* ((loop-orders
			     (get-best-nesting-orders kernel (= verbose 3))))
		      (with-verbose-level (3)
			(format t "~% New Loop Orders:~%~a~%" loop-orders))
		      (apply-reorder-schedule-loops! schedule ctx loop-orders)		      
		      (with-verbose-level (3)
			(format t "~% New Reorderd Schedules:~%")
			(foreign-funcall
			 "isl_schedule_dump"
			 :pointer schedule
			 :void))

		      (when (not (= tile 0))
			(setf schedule (tile-schedule kernel schedule ctx tile)))

		      (with-verbose-level (3)
			(format t "~% New Schedule After Tiling:~%")
			(foreign-funcall
			 "isl_schedule_dump"
			 :pointer schedule
			 :void))

		      ;; Constructs AST From Scheduled Nodes

		      (let* ((space (isl-set-read-from-str ctx "{:}"))
			     (build (isl-ast-build-from-context space))
			     (ast   (isl-ast-build-node-from-schedule build schedule)))
			(with-verbose-level (2)
			  (let* ((c (isl-ast-node-get-ctx ast))
				 (p (isl-printer-to-str c))
				 (p (isl-printer-set-output-format p 4))
				 (q (isl-printer-print-ast-node p ast))
				 (s (isl-printer-get-str q)))
			    
			    (format t "~% Final C Code: ~%~a~%" s)))


			))))))))))))

;; Running example
;; TODO: OpFusion Scheduling...
#+(and)
(run-polyhedral
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT))
  `(for (i 0 10)
	(for (j 0 10)
	     (setf (aref :X i j) (sin (aref :Y i j)))
	     (setf (aref :Z i j) (cos (aref :Y i j)))))
  `(for (i 0 10)
	(for (j 0 10)
	     (setf (aref :Z i j) (logn (aref :Y i j))))))
 :verbose 3)

;; Gemm
#+(and)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(10 10) :FLOAT)
    (make-buffer :Y `(10 10) :FLOAT)
    (make-buffer :Z `(10 10) :FLOAT))
   `(for (i 10)
	 (for (j 0 10)
	      (for (k 0 10)
		   (setf (aref :Z i k) (mulf (aref :X i j) (aref :Y j k) (aref :Z i k)))))))
  :verbose 3))

;; Gemm (Z=0)
#+(and)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(10 10) :FLOAT)
    (make-buffer :Y `(10 10) :FLOAT)
    (make-buffer :Z `(10 10) :FLOAT))
   `(for (i 10)
	 (for (j 0 10)
	      (for (k 0 10)
		   (setf (aref :Z i k) 0.0))
	      (for (k 0 10)
		   (setf (aref :Z i k) (mulf (aref :X i j) (aref :Y j k) (aref :Z i k)))))))
  :verbose 3))

;; Conv
