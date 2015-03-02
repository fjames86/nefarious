;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(defmacro with-nfs-mount ((var path &rest call-args) &body body)
  (alexandria:with-gensyms (gpath gargs)
    `(let ((,gpath ,path)
	   (,gargs (list ,@call-args)))
       (let ((,var (apply #'nefarious.mount:call-mount ,gpath ,gargs)))
	 (unwind-protect (progn ,@body)
	   (apply #'nefarious.mount:call-unmount ,gpath ,gargs))))))

