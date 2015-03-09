;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)


;; -------------------
;; the rpc server 
(defvar *server* nil)

(defun start (&key (port-mapper t))
  "Start the NFS server."
  (setf *server* (make-rpc-server))
  (let ((ports (append (when port-mapper (list port-mapper:*pmap-port*))
		       (list *nfs-port* nfs.mount:*mount-port*))))
    (port-mapper:add-all-mappings ports ports)
    (start-rpc-server *server* 
		      :tcp-ports ports
		      :udp-ports ports)))

(defun stop ()
  "Stop the NFS server."
  (stop-rpc-server *server*))
