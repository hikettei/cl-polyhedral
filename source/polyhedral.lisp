
(cl:in-package :cl-polyhedral)

(defparameter *verbose* 0 "
Indicates the level of logging. 0 or 1 or 2

0 -> Ignores all warnings
1 -> Puts warnings
2 -> Display before/after schedules
3 -> Displays all progresses")

;; TODO:
;;  - SIMDify
;;  - Loop Collapse (If strides are nothing)
;; TODO:
;;  - Allowing incf/decf/mulcf/divcf
;;  - Reduce, 0 accessing <- it should be shuffled together!
;; TODO:
;;  - Allowing incf/decf/mulcf/divcf
;;  - Testing/Benchmarking
;;  - fake-simd-pack-unpack (:fake-lisp backend)
;;  - Loop Collapse/Fusion
;;  - Reduction
;;  - Add: Config
;;  - GraphWriter
;;  - Implement: Cache by the dynamic shape
;;  - Add a new syntax: dotimes
;;  - Add: GCC backend
;;  - Opt: Lisp Backend
;;  - VS:  OpenBLAS

(defun run-polyhedral
    (kernel
     &key
       (config (make-config))
       (backend :lisp)
       (parallel t)
       ;;(simd 0)
       (tile nil)
       (verbose *verbose*)
       (tile-element-n-byte (apply #'max (map 'list #'buffer-n-byte (kernel-args kernel)))))
  "Gains the optimized kernel obtained from transforming the given kernel using Polyhedral Model.
Does the following:
- 1. Creates a context and space for ISL
- 
"
  (declare (type Kernel kernel)
	   (type Config config)
	   (type keyword backend)
	   (type boolean tile parallel)
	   (type fixnum tile-element-n-byte)
	   (type (integer 0 3) verbose))

  ;; Updates the config with checking whether the contents satisfy the current backends' requirements.
  (codegen-check-configs backend kernel)
  (setf (kernel-config kernel) config)
  
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
		  (format t "~%RAW Dependencies(verbose=3):~%")
		  (%isl-union-map-dump raw-deps)
		  (format t "~%WAW Dependencies(verbose=3):~%")
		  (%isl-union-map-dump waw-deps)
		  (format t "~%WAR Dependencies(verbose=3):~%")
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
		  ;;(set-option "isl_options_set_schedule_split_scaled" 1)
		  (set-option "isl_options_set_schedule_serialize_sccs" 1)
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
		    (format t "~%Schedule Constraints(verbose=3):~%")
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
		      (format t "~% Schedule Per Tiling(verbose=3):~%")
		      (foreign-funcall
		       "isl_schedule_dump"
		       :pointer schedule
		       :void))
		    
		    (let* ((loop-orders
			     (get-best-nesting-orders kernel (= verbose 3))))
		      (with-verbose-level (3)
			(format t "~% New Loop Orders(verbose=3):~%~a~%" loop-orders))
		      (apply-reorder-schedule-loops! schedule ctx loop-orders)		      
		      (with-verbose-level (3)
			(format t "~% New Reorderd Schedules(verbose=3):~%")
			(foreign-funcall
			 "isl_schedule_dump"
			 :pointer schedule
			 :void))

		      (when tile
			(setf schedule (tile-schedule kernel schedule ctx tile-element-n-byte)))

		      (with-verbose-level (3)
			(format t "~% New Schedule After Tiling(verbose=3):~%")
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
			    
			    (format t "~%Final C Code(verbose=2): ~%~a~%" s)))
			(let ((body
				(codegen-function
				 backend
				 (parse-isl-ast backend ast kernel parallel)
				 kernel)))
			  (with-verbose-level (3)
			    (format t "~%Final ~a Code(verbose=2):~%~a~%"
				    backend body))
			  (load-optimized-function
			   backend
			   body
			   kernel))))))))))))))

;; Running example
;; TODO: OpFusion Scheduling...
#+(or)
(run-polyhedral
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT)
   (make-buffer :a `(10 10) :float))
  `(for (i 0 10)
	(for (j 0 10)
	     (setf (aref :X i j) (sin (aref :Y i j)))
	     (setf (aref :Z i j) (cos (aref :Y i j)))))
  `(for (i 0 10)
	(for (j 0 10)
	     (setf (aref :Z i j) (logn (aref :Y i j))))))
 :verbose 3)

;; Reduce Sum
#+(or)
(run-polyhedral
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 1)  :FLOAT))
  `(for (i 0 10)
	(for (j 0 1)
	     (setf (aref :Y i j) 0.0)))
  `(for (i 0 10)
	(for (j 0 10)
	     (setf (aref :Y i j) (+ (aref :Y i j) (aref :X i j))))))
 :verbose 3)

;; Gemm
#+(or)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(100 256) :FLOAT)
    (make-buffer :Y `(256 512) :FLOAT)
    (make-buffer :Z `(100 512) :FLOAT))
   `(for (i 100)
	 (for (j 0 256)
	      (for (k 0 512)
		   ;; TODO: setf -> incf
		   (setf (aref :Z i k) (* (aref :X i j) (aref :Y j k) (aref :Z i k)))))))
  :verbose 3
  :tile t))

;; Gemm (Z=0)
#+(or)
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
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))
  :verbose 3))

;; Permute
#+(or)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(10 20 30 40 50 60) :FLOAT)
    (make-buffer :Y `(10 20 30 40 50 60) :FLOAT))
   `(for (a 10)
	 (for (b 0 20)
	      (for (c 0 30)
		   (for (d 0 40)
			(for (e 0 50)
			     (for (f 0 60)
				  (setf (aref :X e b d c f a) (aref :Y f e a d b c)))))))))
  :verbose 3
  :tile nil))

;; Conv2D

;;for (n in 0..batch)
;;for (fout in 0..out_features)
;;for (y in 1..H-1)
;;for (x in 1..W-1)
;;for (fin in 0..in_features)
;;for (k0 in 0..3)
;;for (k1 in 0..3)
;;conv[n, fout, y, x] += weigths[fout, fin, y, x] * input[n, fin, y+k0, x+k1];

#+(or)
(multiple-value-bind (N img-x img-y in-features out-features k-x k-y)
    (values 10 128 128 32 32 25 25)
  (time
   (run-polyhedral
    (make-kernel-from-dsl
     (list
      (make-buffer :X `(,N ,in-features ,img-x ,img-y) :FLOAT)
      (make-buffer :W `(,out-features ,in-features ,k-x ,k-y) :FLOAT)
      (make-buffer :OUT `(,N ,out-features ,img-x ,img-y) :float))
     `(for (N ,N)
	   (for (fout ,out-features)
		(for (y 0 ,(1- img-y))
		     (for (x 0 ,(1- img-x))
			  (for (fin ,in-features)
			       (for (k0 ,k-x)
				    (for (k1 ,k-y)
					 ;; C = C + A*B 
					 (incf (aref :OUT n fout y x) (* (aref :W fout fin y x) (aref :X n fin (+ y k0) (+ x k1))))))))))))
    :verbose 2
    :tile t)))

