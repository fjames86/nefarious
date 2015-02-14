
;;; The mount protocol

(defpackage #:nefarious.mount
  (:use #:cl #:frpc)
  (:export #:call-mount-null
	   #:call-mount-mount
	   #:call-mount-dump
	   #:call-mount-unmount
	   #:call-mount-unmount-all
	   #:call-mount-export))

(in-package #:nefarious.mount)

(defconstant +mount-program+ 100005)
(defconstant +mount-version+ 3)

(use-rpc-program +mount-program+)
(use-rpc-version +mount-version+)

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

;; -----------------------------------------------------
;; for testing connections 
(defrpc call-mount-null (:void :void) 0)

;; -----------------------------------------------------
;; mount 
(defxstruct mount-res-ok ()
  ((fhandle fhandle3)
   (auth-flavours (:varray :int32))))

(defxunion mount-res (mount-stat3)
  ((:ok mount-res-ok)
   (otherwise :void)))

(defrpc call-mount-mount (dir-path mount-res3) 1)

;; -----------------------------------------------------
;; dump -- return mount entries

(defxtype* mount-list () (:optional mount-body))

(defxstruct mount-body ()
  ((hostname name)
   (directory dir-path)
   (next mount-list)))

(defrpc call-mount-dump (:void mount-list) 2)


;; -----------------------------------------------------
;; unmount -- remove mount entry 
(defrpc call-mount-unmount (dir-path :void) 3)

;; -----------------------------------------------------
;; unmount all -- remove all mount entries
(defrpc call-mount-unmount-all (:void :void) 4)

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

(defrpc call-mount-export (:void exports) 5)


