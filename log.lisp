;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(defvar *nfs-log* nil)

(defun nfs-log (lvl format-control &rest args)
  (unless *nfs-log*
    (frpc-log :info "Initializing NFS log")
    (setf *nfs-log* (pounds.log:copy-log frpc:*frpc-log* :tag "NFS ")))
  (apply #'pounds.log:write-message *nfs-log* lvl format-control args))


    
