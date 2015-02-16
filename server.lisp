
(in-package #:nefarious)

;; the server 
(defvar *server* nil)

(defun start ()
  (setf *server* (start-rpc-server 8000)))

(defun stop ()
  (stop-rpc-server *server*))
