;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The mount protocol

(defpackage #:nefarious.mount
  (:use #:cl #:frpc #:nefarious.handles)
  (:nicknames #:nfs.mount)
  (:export #:call-null
	   #:call-mount
	   #:call-dump
	   #:call-unmount
	   #:call-unmount-all
	   #:call-export
	   #:*mount-port*
	   #:mountedp))

(in-package #:nefarious.mount)

(defconstant +mount-program+ 100005)
(defconstant +mount-version+ 3)

(defparameter *mount-port* 635)

(use-rpc-program +mount-program+ +mount-version+)
(use-rpc-port 635)

(defconstant +mount-path-len+ 1024)
(defconstant +mount-name-len+ 255)
(defconstant +mount-fh-size3+ 64)

(defxtype* fhandle3 () (:varray* :octet +mount-fh-size3+))
(defxtype* dir-path () :string)
(defxtype* name () :string)

(defxenum mount-stat3 
  ((:ok 0)
   (:perm 1)
   (:noent 2)
   (:io 5)
   (:access 13)
   (:notdir 20)
   (:inval 22)
   (:name-too-long 63)
   (:not-supp 10004)
   (:server-fault 10006)))

(define-condition mount-error (error)
  ((stat :initarg :stat :initform nil :reader mount-error-stat))
  (:report (lambda (condition stream)
	     (format stream "MOUNT-ERROR: ~A" (mount-error-stat condition)))))

;; -------------------------------

;; list of hosts who have mounted
(defvar *mount-clients* nil)

(defun add-mount-client (address)
  (pushnew address *mount-clients*
	   :test #'equalp))

(defun rem-mount-client (address)
  (setf *mount-clients*
	(remove address *mount-clients*
		:test #'equalp)))

(defun mountedp (address)
  (find address *mount-clients* :test #'equalp))

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

;; FIXME: we need to keep a list of the clients who have mounted. any NFS calls from 
;; non-mounted clients should be rejected
(defhandler %handle-mount (dpath 1)
  "Find the exported directory with the export name DPATH and return its handle and authentication flavours."
  (let ((dhandle (find-export dpath)))
    (cond
      (dhandle
       (log:debug "Host ~A successfully mounted ~A" 
		  frpc:*rpc-remote-host* (handle-pathname dhandle))
       (add-mount-client frpc:*rpc-remote-host*)
       (make-xunion :ok 
		    (list (handle-fh dhandle) '(:auth-null))))
      (t 
       (make-xunion :inval nil)))))

;; -----------------------------------------------------
;; dump -- return mount entries

(defxtype* mount-list () (:optional mount-body))

(defxtype* mounting () 
  (:plist :hostname name
	  :directory dir-path))

(defxstruct mount-body ()
  ((mount mounting)
   (next mount-list)))

(defrpc call-dump 2 :void mount-list
  (:transformer (res)
    (do ((mount-list res (mount-body-next mount-list))
	 (mlist nil))
	((null mount-list) mlist)
      (push (mount-body-mount mount-list) mlist))))

;;(defhandler %handle-dump (void 2)
;;  (declare (ignore void))
;;  nil)

;; -----------------------------------------------------
;; unmount -- remove mount entry 
(defrpc call-unmount 3 dir-path :void
  (:arg-transformer (dpath) dpath))

;; FIXME: when we support keeping a list of mounted clients we should 
;; remove the client from this list 
(defhandler %handle-unmount (dpath 3)
  (log:debug "Host ~A unmounted ~A" frpc:*rpc-remote-host* dpath)
  (rem-mount-client frpc:*rpc-remote-host*)
  nil)

;; -----------------------------------------------------
;; unmount all -- remove all mount entries
(defrpc call-unmount-all 4 :void :void)

(defhandler %handle-unmount-all (void 4)
  (declare (ignore void))
  (log:debug "Host ~A unmounted all" frpc:*rpc-remote-host*)
  (rem-mount-client frpc:*rpc-remote-host*)
  nil)

;; -----------------------------------------------------
;; export -- return export list 
(defxtype* groups () (:optional group-node))

(defxstruct group-node ()
  ((name name)
   (next groups)))

(defxtype* exports () (:optional export-node))

(defxstruct export-node ()
  ((dir dir-path)
   (groups groups)
   (next exports)))

(defrpc call-export 5 :void exports
  (:transformer (res)
    (do ((enodes res (export-node-next enodes))
	 (elist nil))
	((null enodes) elist)
      (push (list (cons :dir (export-node-dir enodes))
		  (cons :groups 
			(do ((groups (export-node-groups enodes) (group-node-next groups))
			     (glist nil))
			    ((null groups) glist)
			  (push (group-node-name groups) glist))))
	    elist))))

;;(defhandler %handle-export (void 5)
;;  (declare (ignore void))
;;  nil)
