;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

;; the server 
(defvar *server* nil)

(defun start ()
  "Start the NFS server."
  (setf *server* (make-rpc-server))
  (let ((tcp-ports (list pmap:*pmap-port* *nfs-port* nfs.mount:*mount-port*))
	(udp-ports nil))
    ;; tell the port mapper about the NFS ports
    (pmap:add-all-mappings tcp-ports udp-ports)
    (start-rpc-server *server* 
		      :tcp-ports tcp-ports
		      :udp-ports udp-ports)))

(defun stop ()
  "Stop the NFS server."
  (stop-rpc-server *server*))
