
(cl:in-package :cl-user)

(mgl-pax:define-package :cl-polyhedral
  (:use
   :cl
   :cffi
   :mgl-pax)
  (:export
   ;; Graph Representation
   #:Instruction

   ))

(in-package :cl-polyhedral)

;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Memo: In order to find a path where cl-polyhedral was installed,
;; this library must be loaded via asdf:load-system or ql:quickload otherwise CFFI can't find ISL!
(defvar *src-dir* (asdf:component-pathname (asdf:find-system "cl-polyhedral")))
(labels ((load-helper ()
	   (restart-case
	       (handler-case
		   (cffi:load-foreign-library
		    '(:default "libisl")
		    :search-path (merge-pathnames
				  #P"usr/"
				  (user-homedir-pathname)))
		 (cffi:load-foreign-library-error (c)
		   (warn "cl-polyhedral could not find libisl the shared library. Ensure that the ISL was installed.")
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
			  (warn "cl-polyhedral: Building ISL from the source with:~%    ~a" cmd)
			  (uiop:launch-program
			   cmd
			   :error-output :stream)))
		      (error-output
			(uiop:process-info-error-output info)))
		 (unless (zerop (uiop:wait-process info))
		   (error
		    "cl-polyhedral: Building from source was failed due to:~%~a"
		    (alexandria:read-stream-content-into-string error-output)))
		 (load-helper))))))
  (load-helper))

