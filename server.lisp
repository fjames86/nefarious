;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)


;; -------------------
;; the rpc server 
(defvar *server* nil)

(defun start (&key port-mapper (nsm t) ports)
  "Start the NFS server. If PORT-MAPPER is nil, then it is assumed the port-mapper program is running externally to Lisp and all port mappings are added using RPC calls to it. Otherwise the server will run its own port mapper by listening on port 111.

When NSM is non-nil, the NSM program, which facilitates server state change notifications, will be supported.

If PORTS is supplied, it shouild be a list of integers specifying the port numbers to listen on (TCP and UDP will be used). Otherwise port *RPC-PORT* is used, which defaults to 2049.

"
  ;; make a new server instance
  (setf *server* (make-rpc-server))
  
  ;; if no ports specified, use the default port
  (unless ports 
    (setf ports (list *nfs-port*)))

  ;; when running port mapper program, ensure the port mapper ports are used.
  ;; this is vital for any external systems to find our programs 
  (when port-mapper 
    (pushnew 111 ports :test #'=))

  ;; setup the port mappings
  (port-mapper:add-all-mappings ports ports 
				:rpc (not port-mapper))
  
  ;; when using locking NLM/NSM protocols we need to initialize the state variable 
  (when nsm (nsm:init-nsm))
  
  ;; start the RPC server 
  (start-rpc-server *server* 
		    :tcp-ports ports
		    :udp-ports ports))

(defun stop ()
  "Stop the NFS server."
  (stop-rpc-server *server*))

