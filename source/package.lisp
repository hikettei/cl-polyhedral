
(cl:in-package :cl-user)

(mgl-pax:define-package :cl-polyhedral
  (:use
   :cl
   :cffi
   :mgl-pax)
  ;; Writing an extension
  (:export
   #:binary-op-t
   #:codegen-write-id
   #:codegen-write-num
   #:codegen-write-binary-op
   #:codegen-write-minus
   #:codegen-write-for
   #:codegen-write-block
   #:codegen-write-index-ref
   #:codegen-write-expr
   #:codegen-write-array-move
   #:codegen-write-instruction
   #:codegen-write-setf
   #:codegen-function
   #:load-optimized-function
   #:codegen-check-configs)

  ;; Configs
  (:export
   #:*indent-level*
   #:*default-config*
   #:Config
   #:config-of
   #:config-ls
   #:declare-config)
  
  (:export
   #:make-kernel-from-dsl
   #:run-polyhedral
   #:define-poly-func
   #:poly-lambda

   )

  (:export
   #:with-arrays-to-pointers)
  ;; TODO: Graph Structures, Kernel Representations
  (:export
   #:Buffer
   #:Buffer-name
   #:Buffer-shape
   #:Buffer-strides
   #:Buffer-dtype
   #:buffer-n-byte
   #:buffer-io)
  (:export
   #:Instruction
   #:Domain
   #:Kernel))

(in-package :cl-polyhedral)

(eval-when (:compile-toplevel :load-toplevel :execute)
;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Memo: In order to find a path where cl-polyhedral was installed,
;; this library must be loaded via asdf:load-system or ql:quickload otherwise CFFI can't find ISL!
(defvar *src-dir* (asdf:component-pathname (asdf:find-system "cl-polyhedral")))
;; Dependency1. ISL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(labels ((load-helper ()
	   (restart-case
	       (handler-case
		   (cffi:load-foreign-library
		    '(:default "libisl")
		    :search-path (merge-pathnames
				  #P"usr/"
				  (user-homedir-pathname)))
		 (cffi:load-foreign-library-error (c)
		   (warn "cl-polyhedral could not find libisl the shared library.~% (Recommended) Ensure that the ISL was installed and CFFI is able to find out libisl.dylib:~% - sudo apt install libisl-dev~% - brew install libisl")
		   (error c)))
	     (retry-load-foreign-library ()
	       :report "Try doing cffi:load-foreign-library again."
	       (load-helper))
	     (build-from-source-and-try ()
	       :report "Building ISL from the source, cl-isl loads the shared library again."
	       (let* ((cmd
			(format
			 nil
			 "~a ~a ~a ~a ~a"
			 "cd"
			 (namestring
			  (merge-pathnames
			   #P"isl/"
			   *src-dir*))
			 " && sh ./autogen.sh"
			 " && CFLAGS=\"$(pkg-config --libs --cflags gmp)\" ./configure --prefix $HOME/usr"
			 " && make -j 4 && make install"))
		      (info
			(progn
			  (warn "cl-polyhedral: Building ISL from the source with:~%    ~a~%Ensure that all dependencies for ISL are installed." cmd)
			  (uiop:launch-program
			   cmd
			   :error-output :stream)))
		      (error-output
			(uiop:process-info-error-output info)))
		 (unless (zerop (uiop:wait-process info))
		   (error
		    "cl-polyhedral: Building from source was failed due to:~%~a~%Ensure that all dependencies for ISL are installed on your device.~% Or consider installing libisl manually."
		    (alexandria:read-stream-content-into-string error-output)))
		 (load-helper))))))
  (load-helper))


;; 2. Dependency2. HwLOC (Optional) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(labels ((load-helper ()
	   (handler-case
	       (progn
		 (cffi:load-foreign-library
		  '(:default "libhwloc")))
	     (cffi:load-foreign-library-error (c)
	       (declare (ignore c))
	       (warn "cl-polyhedral: HwLoc seens not to be installed on your device, Proceeding with assuming L1=32KB.")))))
  (load-helper))

) ;; eval-when
