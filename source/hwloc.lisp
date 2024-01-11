
(in-package :cl-polyhedral)

;; Determining the L1 Cache
(defun hwloc-determine-l1-size ()
  (let* ((cmd "hwloc-ls | grep L1d")
	 (info
	   (uiop:launch-program
	    cmd
	    :output :stream
	    :error-output :stream))
	 (output
	   (uiop:process-info-output info))
	 (error-output
	   (uiop:process-info-error-output info)))
    (symbol-macrolet ((->failed (return-from hwloc-determine-l1-size (map 'list #'(lambda (x) (declare (ignore x)) (* 1000 32)) (range 0 (cl-cpus:get-number-of-processors))))))
      (when (not (zerop (uiop:wait-process info)))
	(warn "cl-polyhedral failed to determine the size of L1 Cache due to:~% ~a~%Attempted:~%    `$ ~a`~%Proceeding with assuming L1=32KB * n_cpu when tiling" (alexandria:read-stream-content-into-string error-output) cmd)
	->failed)
      
      (let ((l1-lines
	      (cl-ppcre:split
	       #\newline
	       (cl-ppcre:regex-replace-all
		" "
		(alexandria:read-stream-content-into-string output) ""))))

	(when (not (= (length l1-lines) (cl-cpus:get-number-of-processors)))
	  (warn "cl-polyhedral failed to determine the size of L1 Cache. Proceeding with assuming L1=32KB * n_cpu when applying tiling.")->failed)

	(map
	 'list
	 #'(lambda (x)
	     (multiple-value-bind (from to) (cl-ppcre:scan "([0-9]+KB)" x)
	       (when (or (null from) (null to))
		 (warn "cl-polyhedral failed to determine the size of L1 Cache. Proceeding with assuming L1=32KB * n_cpu when applying tiling.")->failed)
	       (* 1000 (read-from-string (subseq x from (- to 2))))))
	 l1-lines)))))

(defparameter *L1-Cache* (hwloc-determine-l1-size))
