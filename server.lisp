
(in-package #:nefarious)

;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* 
		    :tcp-ports (list *nfs-port* nfs.mount:*mount-port*) 
		    :udp-ports (list *nfs-port* nfs.mount:*mount-port*)))

(defun stop ()
  (stop-rpc-server *server*))
