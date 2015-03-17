;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)


;; -------------------
;; the rpc server 
(defvar *server* nil)

(defun start (&key (port-mapper t))
  "Start the NFS server. If PORT-MAPPER is nil, then it is assumed the port-mapper program is running externally to Lisp and all port mappings are added using RPC calls to it. Otherwise the server will run its own port mapper by listening on port 111, in addition to ports 635 and 2049 which are always used."
  (setf *server* (make-rpc-server))
  (let ((ports (append (when port-mapper (list port-mapper:*pmap-port*))
		       (list *nfs-port* nfs.mount:*mount-port*))))
    (port-mapper:add-all-mappings ports ports 
                                  :rpc (not port-mapper))
    (start-rpc-server *server* 
		      :tcp-ports ports
		      :udp-ports ports)))

(defun stop ()
  "Stop the NFS server."
  (stop-rpc-server *server*))
