;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

;; the rpc server 
(defvar *server* nil)

;; FIXME: if the user provides wildcard port numbers, i.e. 0, then we won't know the port 
;; that got assigned to us until the rpc server has started
;; It might be better to hold off adding the port mappings until after the server has started

(defun start (&key (nsm t) ports)
  "Start the NFS server.

When NSM is non-nil, the NSM program, which facilitates server state change notifications, will be supported.

If PORTS is supplied, it should be a list of integers specifying the port numbers to listen on (both TCP and UDP will be used). Otherwise port *RPC-PORT* is used, which defaults to 2049.

"  
  ;; if no ports specified, use the default port
  (unless ports 
    (setf ports (list *nfs-port*)))

  (nfs-log :info "Starting server on ports ~A" ports)

  ;; make a new server instance
  (setf *server* (make-rpc-server :tcp-ports ports :udp-ports ports))

  ;; when using locking NLM/NSM protocols we need to initialize the state variable 
  (when nsm (nsm:init-nsm))
  
  ;; start the RPC server 
  (start-rpc-server *server*))

(defun stop ()
  "Stop the NFS server."
  (stop-rpc-server *server*)
  nil)



