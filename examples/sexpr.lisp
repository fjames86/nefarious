;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:nefarious.sexpr
  (:use #:cl #:frpc #:nefarious)
  (:nicknames #:nfs.sexpr))

(in-package #:nefarious.sexpr)

(defun pack-fh (id)
  (let ((v (nibbles:make-octet-vector 4)))
    (setf (nibbles:ub32ref/be v 0) id)
    v))

(defun unpack-fh (fh)
  (nibbles:ub32ref/be fh 0))

(defclass sexpr-provider (nfs-provider)
  ((sexpr :initform nil :initarg :sexpr :accessor sexpr)
   (mount-fh :initform #(0 0 0 0) :reader mount-fh)))

(defun find-id (sexpr id)
  (cond
    ((null sexpr) nil)
    ((= (car sexpr) id)
     sexpr)
    ((atom (cdr sexpr))
     nil)
    (t 
     (dolist (pair (cdr sexpr))
       (let ((found (find-id pair id)))
	 (when found
	   (return-from find-id found))))
     nil)))

(defun find-item (provider fh)
  (let ((id (unpack-fh fh)))
    (find-id (sexpr provider) id)))

(defun (setf find-item) (value provider fh)
  (let ((item (find-item provider fh)))
    (when item
      (setf (cdr item) value))))

(defun plistp (list)
  (do ((p t)
       (%list list (cddr %list)))
      ((or (not p) (null %list)) p)
    (unless (and (car %list) (symbolp (car %list)))
      (setf p nil))))
(defun alistp (list)
  (do ((p t)
       (%list list (cdr %list)))
      ((or (not p) (null %list)) p)
    (unless (and (consp (car %list)) (symbolp (caar %list)))
      (setf p nil))))
		 
