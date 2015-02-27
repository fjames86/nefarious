;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The mount protocol

(in-package #:nefarious.mount)

(defconstant +mount-program+ 100005)
(defconstant +mount-version+ 3)

(use-rpc-program +mount-program+ +mount-version+)

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

(defparameter *mount-host* "localhost")
(defparameter *mount-port* 635)

(define-condition mount-error (error)
  ((stat :initarg :stat :initform nil :reader mount-error-stat))
  (:report (lambda (condition stream)
	     (format stream "MOUNT-ERROR: ~A" (mount-error-stat condition)))))

;; -----------------------------------------------------
;; for testing connections 
(defrpc %call-null 0 :void :void)
(defun call-null (&key (host *mount-host*) (port *mount-port*) protocol)
  (%call-null nil :host host :port port :protocol protocol))

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; -----------------------------------------------------
;; mount 

(defrpc %call-mount 1
  dir-path
  (:union mount-stat3
    (:ok (:list fhandle3 (:varray frpc::auth-flavour)))
    (otherwise :void)))

(defun call-mount (dpath &key (host *mount-host*) (port *mount-port*) protocol)
  (let ((res (%call-mount dpath :host host :port port :protocol protocol)))
    (case (xunion-tag res)
      (:ok (destructuring-bind (dhandle auth-flavours) (xunion-val res)
	     (values dhandle auth-flavours)))
      (otherwise (error 'mount-error :stat (xunion-tag res))))))

;; FIXME: we need to keep a list of the clients who have mounted. any NFS calls from 
;; non-mounted clients should be rejected
(defhandler %handle-mount (dpath 1)
  "Find the exported directory with the export name DPATH and return its handle and authentication flavours."
  (let ((dhandle (nefarious::find-export dpath)))
    (if dhandle
	(make-xunion :ok 
		     (list (nefarious::handle-fh dhandle) '(:auth-null)))
	(make-xunion :inval nil))))

;; -----------------------------------------------------
;; dump -- return mount entries

(defxtype* mount-list () (:optional mount-body))

(defxtype* mounting () 
  (:plist :hostname name
	  :directory dir-path))

(defxstruct mount-body ()
  ((mount mounting)
   (next mount-list)))

(defrpc %call-dump 2 :void mount-list)
(defun call-dump (&key (host *mount-host*) (port *mount-port*) protocol)
  (do ((mount-list (%call-dump nil :host host :port port :protocol protocol)
		   (mount-body-next mount-list))
       (mlist nil))
      ((null mount-list) mlist)
    (push (mount-body-mount mount-list) mlist)))

;;(defhandler %handle-dump (void 2)
;;  (declare (ignore void))
;;  nil)

;; -----------------------------------------------------
;; unmount -- remove mount entry 
(defrpc %call-unmount 3 dir-path :void)
(defun call-unmount (dpath &key (host *mount-host*) (port *mount-port*) protocol)
  (%call-unmount dpath :host host :port port :protocol protocol))

;; FIXME: when we support keeping a list of mounted clients we should 
;; remove the client from this list 
(defhandler %handle-unmount (dpath 3)
  (declare (ignore dpath))
  nil)

;; -----------------------------------------------------
;; unmount all -- remove all mount entries
(defrpc %call-unmount-all 4 :void :void)
(defun call-unmount-all (&key (host *mount-host*) (port *mount-port*) protocol)
  (%call-unmount-all nil :host host :port port :protocol protocol))

(defhandler %handle-unmount-all (void 4)
  (declare (ignore void))
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

(defrpc %call-export 5 :void exports)
(defun call-export (&key (host *mount-host*) (port *mount-port*) protocol)
  (do ((enodes (%call-export nil :host host :port port :protocol protocol) (export-node-next enodes))
	(elist nil))
      ((null enodes) elist)
    (push (list (cons :dir (export-node-dir enodes))
		(cons :groups 
		      (do ((groups (export-node-groups enodes) (group-node-next groups))
			   (glist nil))
			  ((null groups) glist)
			(push (group-node-name groups) glist))))
	  elist)))

;;(defhandler %handle-export (void 5)
;;  (declare (ignore void))
;;  nil)
