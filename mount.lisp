
;;; The mount protocol

(defpackage #:nefarious.mount
  (:use #:cl #:frpc)
  (:export #:call-null
	   #:call-mount
	   #:call-dump
	   #:call-unmount
	   #:call-unmount-all
	   #:call-export))

(in-package #:nefarious.mount)

(defconstant +mount-program+ 100005)
(defconstant +mount-version+ 3)

(use-rpc-program +mount-program+ +mount-version+)

(defconstant +mount-path-len+ 1024)
(defconstant +mount-name-len+ 255)
(defconstant +mount-fh-size3+ 64)

(defxtype* fhandle3 () (:varray :octet +mount-fh-size3+))
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
(defparameter *mount-port* 8000)

;; -----------------------------------------------------
;; for testing connections 
(defrpc %call-null 0 :void :void)
(defun call-null (&key (host *mount-host*) (port *mount-port*))
  (%call-null host nil :port port))

;; -----------------------------------------------------
;; mount 

(defrpc %call-mount 1
  dir-path
  (:union mount-stat3
    (:ok (:list fhandle3 (:varray :int32)))
    (otherwise :void)))

(defun call-mount (dpath &key (host *mount-host*) (port *mount-port*))
  (%call-mount host dpath :port port))


;; -----------------------------------------------------
;; dump -- return mount entries

(defxtype* mount-list () (:optional mount-body))

(defxstruct mount-body ()
  ((hostname name)
   (directory dir-path)
   (next mount-list)))

(defrpc %call-dump 2 :void mount-list)
(defun call-dump (&key (host *mount-host*) (port *mount-port*))
  (%call-dump host nil :port port))

;; -----------------------------------------------------
;; unmount -- remove mount entry 
(defrpc %call-unmount 3 dir-path :void)
(defun call-unmount (dpath &key (host *mount-host*) (port *mount-port*))
  (%call-unmount host dpath :port port))

;; -----------------------------------------------------
;; unmount all -- remove all mount entries
(defrpc %call-unmount-all 4 :void :void)
(defun call-unmount-all (&key (host *mount-host*) (port *mount-port*))
  (%call-unmount-all host nil :port port))

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
(defun call-export (&key (host *mount-host*) (port *mount-port*))
  (do ((enodes (%call-export host nil :port port) (export-node-next enodes))
	(elist nil))
      ((null enodes) elist)
    (push (list (cons :dir (export-node-dir enodes))
		(cons :groups 
		      (do ((groups (export-node-groups enodes) (group-node-next groups))
			   (glist nil))
			  ((null groups) glist)
			(push (group-node-name groups) glist))))
	  elist)))



