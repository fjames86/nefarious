;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:nefarious)

;; This file defines the interface that NFS providers should implement

(defvar *provider-id* 0)
(defparameter *providers* nil)

(defclass nfs-provider ()
  ((id :initform (prog1 *provider-id* (incf *provider-id*)) :accessor provider-id)
   (path :initform "/" :initarg :path :accessor provider-path)
   (clients :initform nil :accessor provider-clients)))


(defun register-provider (provider export-path)
  (setf (provider-path provider) export-path)
  (let ((old (find-provider (provider-path provider))))
    (cond
      (old
       (warn "Replacing provider registered on path ~A." (provider-path provider))
       (setf (provider-id provider) (provider-id old)
	     *providers* (substitute provider old *providers*)))
      (t 
       (push provider *providers*))))
  provider)

(defun find-provider (id-or-path)
  (etypecase id-or-path
    (integer (find-if (lambda (provider)
			(= (provider-id provider) id-or-path))
		      *providers*))
    (string (find-if (lambda (provider)
		       (string= (provider-path provider) id-or-path))
		     *providers*))))

(defun unregister-provider (id-or-path)
  (let ((provider (find-provider id-or-path)))
    (when provider
      (setf *providers* (remove provider *providers*)))))

;; for the external handles we prepend the provider ID so we know who to dispatch to
(defxtype* provider-fh ((:reader read-provider-fh) (:writer write-provider-fh))
  (:list :uint32 (:varray* :octet)))

(defun fh-provider-handle (fh)
  "Convert an external NFS handle to an internal provider handle."
  (handler-case 
      (destructuring-bind (id handle) (unpack #'read-provider-fh fh)
	(list (find-provider id) handle))
    (error () '(nil nil))))

(defun provider-handle-fh (provider handle)
  "Convert an internal provider handle into an external NFS handle."
  (pack #'write-provider-fh (list (provider-id provider) handle)))

;; ----------------------------------------

;; for the mount protocol
(defgeneric nfs-provider-mount (provider client)
  (:documentation "Mount a client. Returns the handle for the mount point."))

(defgeneric nfs-provider-unmount (provider client)
  (:documentation "Unmount a client."))

;; for nfs
(defgeneric nfs-provider-attrs (provider handle)
  (:documentation "Optionally returns the FATTR3 structure for the handle object."))

(defgeneric (setf nfs-provider-attrs) (value provider handle)
  (:documentation "Sets the attributes for the object."))

(defgeneric nfs-provider-lookup (provider dhandle name)
  (:documentation "Find the handle for an object."))

(defgeneric nfs-provider-access (provider handle access)
  (:documentation "Checks the access privileges for the object. ACCESS shouild be a list of NFS-ACCESS enum symbols.
Returns a list of the valid access privileges."))

;; readlink ? don't support this yet
;;(defgeneric nfs-privider-read-link (provider handle))

(defgeneric nfs-provider-read (provider handle offset count)
  (:documentation "Read count bytes from offset from the object."))

(defgeneric nfs-provider-write (provider handle offset bytes)
  (:documentation "Write bytes at offset to the object. Returns the number of bytes written."))

(defgeneric nfs-provider-create (provider dhandle name)
  (:documentation "Create a new file named NAME in directory DHANDLE. Returns the handle for the new file."))

(defgeneric nfs-provider-remove (provider dhandle name)
  (:documentation "Remove the file named NAME in directory DHANDLE."))

(defgeneric nfs-provider-rename (provider from-dhandle from-name to-dhandle to-name)
  (:documentation "Rename the file named by the FROM-DHANDLE/FROM-NAME."))

(defgeneric nfs-provider-read-dir (provider dhandle)
  (:documentation "Returns a list of all object (file and directory) names in the directory."))

(defgeneric nfs-provider-create-dir (provider dhandle name)
  (:documentation "Create a new directory."))

(defgeneric nfs-provider-remove-dir (provider dhandle name)
  (:documentation "Remove a directory named NAME in directory DHANDLE."))

(defgeneric nfs-provider-create-device (provider type dhandle name &key attrs specdata)
  (:documentation "Create a special device. Type should be an ftype3 enum."))

;; filesystem information
(defgeneric nfs-provider-fs-info (provider)
  (:documentation "Returns dynamic filesystem information, in an FS-INFO structure."))

(defgeneric nfs-provider-fs-stat (provider)
  (:documentation "Returns static filesystem information, in an FS-STAT structure."))

(defgeneric nfs-provider-path-conf (provider)
  (:documentation "Returns a PATH-CONF structure containing information about the filesystem."))

