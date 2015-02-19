
(in-package #:nefarious)

;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (make-rpc-server))
  (start-rpc-server *server* :port *nfs-port*))

(defun stop ()
  (stop-rpc-server *server*))
