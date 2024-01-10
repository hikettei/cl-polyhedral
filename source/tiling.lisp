
(in-package :cl-polyhedral)

(declaim (ftype (function (t) fixnum) get-tile-dim))
(defun get-tile-dim (ex &aux (max-dim 0))
  (declare (type fixnum max-dim)
	   (optimize (speed 3)))
  (typecase ex
    (Kernel
     (loop for inst across (kernel-instructions ex) do
       (setf max-dim (max max-dim (get-tile-dim (coerce (inst-sources inst) 'list)))))
     max-dim)
    (list
     (trivia:ematch ex
       ((list 'aref _ ref)
	(length (the list ref)))))   
    (T
     0)))


(defun tile-schedule (kernel schedule ctx tile)
  (warn "schedule ... TODO")
  schedule)
