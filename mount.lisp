;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The mount protocol

(defpackage #:nefarious.mount
  (:use #:cl #:frpc)
  (:nicknames #:nfs.mount)
  (:export #:call-null
	   #:call-mount
	   #:call-dump
	   #:call-unmount
	   #:call-unmount-all
	   #:call-export
	   #:*mount-port*))

(in-package #:nefarious.mount)

(defconstant +mount-program+ 100005)
(defconstant +mount-version+ 3)

(defparameter *mount-port* 635)

(use-rpc-program +mount-program+ +mount-version+)
(use-rpc-host '*rpc-host* 635)

(defconstant +mount-path-len+ 1024)
(defconstant +mount-name-len+ 255)
(defconstant +mount-fh-size3+ 64)

(defxtype* fhandle3 () (:varray* :octet +mount-fh-size3+))
(defxtype* dir-path () :string)
(defxtype* name () :string)

(defxenum mount-stat3 
  (:ok 0)
  (:perm 1)
  (:noent 2)
  (:io 5)
  (:access 13)
  (:notdir 20)
  (:inval 22)
  (:name-too-long 63)
  (:not-supp 10004)
  (:server-fault 10006))

(define-condition mount-error (error)
  ((stat :initarg :stat :initform nil :reader mount-error-stat))
  (:report (lambda (condition stream)
	     (format stream "MOUNT-ERROR: ~A" (mount-error-stat condition)))))

;; -----------------------------------------------------
;; for testing connections 
(defrpc call-null 0 :void :void)

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; -----------------------------------------------------
;; mount 

(defrpc call-mount 1
  dir-path
  (:union mount-stat3
    (:ok (:list fhandle3 (:varray frpc::auth-flavour)))
    (otherwise :void))
  (:arg-transformer (dpath) dpath)
  (:transformer (res)
    (case (xunion-tag res)
      (:ok (destructuring-bind (dhandle auth-flavours) (xunion-val res)
	     (values dhandle auth-flavours)))
      (otherwise (error 'mount-error :stat (xunion-tag res))))))

(defhandler %handle-mount (dpath 1)
  "Find the exported directory with the export name DPATH and return its handle and authentication flavours."
  (let ((provider (nefarious:find-provider dpath)))
    (cond 
      (provider
       ;; get the mount handle 
       (handler-case 
           (let ((dhandle (nefarious:nfs-provider-mount provider *rpc-remote-host*)))
             ;; push the client onto the provider's client list 
             (pushnew *rpc-remote-host* (nefarious:provider-clients provider) :test #'equalp)         
             (make-xunion :ok
                          (list (nefarious:provider-handle-fh provider dhandle)
                                '(:auth-null))))
         (error (e)
           (log:debug "error ~A" e)
           (make-xunion :access nil))))
      (t 
       ;; no provider registered on that path
       (make-xunion :inval nil)))))

;; -----------------------------------------------------
;; dump -- return mount entries

(defxtype* mount-list () (:optional mount-body))

(defxtype* mounting () 
  (:plist :hostname name
	  :directory dir-path))

(defxstruct mount-body ()
  (mount mounting)
  (next mount-list))

(defrpc call-dump 2 :void mount-list
  (:transformer (res)
    (do ((mount-list res (mount-body-next mount-list))
	 (mlist nil))
	((null mount-list) mlist)
      (push (mount-body-mount mount-list) mlist))))

;; return a list of all the mounts of all the clients 
(defhandler %handle-dump (void 2)
  (declare (ignore void))
  (do ((mlist nil)
       (providers nefarious::*providers* (cdr providers)))
      ((null providers) mlist)
    (dolist (client (nefarious:provider-clients (car providers)))
      (let ((mbody (make-mount-body :mount 
				    (list :hostname (usocket:vector-quad-to-dotted-quad client)
					  :directory (nefarious:provider-path (car providers))))))
	(if mlist
	    (setf (mount-body-next mlist) mbody
		  mlist mbody)
	    (setf mlist mbody))))))

;; -----------------------------------------------------
;; unmount -- remove mount entry 
(defrpc call-unmount 3 dir-path :void
  (:arg-transformer (dpath) dpath))

(defhandler %handle-unmount (dpath 3)
  (let ((provider (nefarious:find-provider dpath)))
    (cond
      (provider
       (handler-case 
           (progn 
             ;; tell the provider the client would  like to unmount        
             (nefarious:nfs-provider-unmount provider *rpc-remote-host*)
             ;; remove the client from the provider's client list 
             (setf (nefarious:provider-clients provider)
                   (remove *rpc-remote-host* (nefarious:provider-clients provider) 
                           :test #'equalp)))
         (error (e)
           nil))
       nil)
      (t 
       nil))))

;; -----------------------------------------------------
;; unmount all -- remove all mount entries
(defrpc call-unmount-all 4 :void :void)

;;(defhandler %handle-unmount-all (void 4)
;;  (declare (ignore void))
;;  nil)

;; -----------------------------------------------------
;; export -- return export list 
(defxtype* groups () (:optional group-node))

(defxtype* group-node () 
  (:plist :name name :next groups))

(defxtype* exports () (:optional export-node))

(defxstruct export-node ()
  (dir dir-path)
  (groups groups)
  (next exports))

(defrpc call-export 5 :void exports
  (:transformer (res)
    (do ((enodes res (export-node-next enodes))
	 (elist nil))
	((null enodes) elist)
      (push (list :dir (export-node-dir enodes)
                  :groups 
                  (do ((groups (export-node-groups enodes) (getf groups :next))
                       (glist nil))
                      ((null groups) glist)
                    (push (getf groups :name) glist)))
	    elist)))
  (:documentation "Returns a list of the exported filesystems from the NFS server."))

(defhandler %handle-export (void 5)
  (declare (ignore void))
  (let ((export-paths (mapcar #'nefarious:provider-path nefarious::*providers*)))
    (when export-paths
      (do ((ex nil)
	   (epaths export-paths (cdr epaths)))
	  ((null epaths) ex)
	(let ((e (make-export-node :dir (car epaths))))
	  (if ex
	      (setf (export-node-next e) ex
		    ex e)
	      (setf ex e)))))))
