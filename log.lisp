;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(defparameter *nfs-log-levels* '(:info :warning :error))

(let ((tag (babel:string-to-octets "NFS ")))
  (defun nfs-log (lvl format-control &rest args)
    (unless frpc:*frpc-log*
      (frpc-log :info "initialzing NFS log"))
    (when (member lvl *nfs-log-levels*)
      (pounds.log:write-message frpc:*frpc-log*
				lvl
				(apply #'format nil format-control args)
				:tag tag))))
    
