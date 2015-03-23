;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:nefarious)

;; This file defines the interface that NFS providers should implement

(defvar *provider-id* 0 
  "The ID to use for the next provider. Constantly increasing integer.")
(defparameter *providers* nil
  "List of registered providers.")

(defclass nfs-provider ()
  ((id :initform (prog1 *provider-id* (incf *provider-id*)) :accessor provider-id)
   (path :initform "/" :initarg :path :accessor provider-path)
   (clients :initform nil :accessor provider-clients)))

(defmethod print-object ((p nfs-provider) stream)
  (print-unreadable-object (p stream :type t)
    (format stream ":PATH ~S " (provider-path p))))

(defun register-provider (provider export-path)
  "Register a provider to export on path EXPORT-PATH."
  (declare (type nfs-provider provider)
	   (type string export-path))
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
(defun fh-provider-handle (fh)
  "Convert an external NFS handle to an internal provider handle."
  (handler-case 
      (let ((id (nibbles:ub32ref/be (coerce (subseq fh 0 4) '(vector (unsigned-byte 8))) 0))
            (h (subseq fh 4)))
        (list (find-provider id) h))
    (error () '(nil nil))))

(defun provider-handle-fh (provider handle)
  "Convert an internal provider handle into an external NFS handle."
  (concatenate 'vector 
               (let ((v (nibbles:make-octet-vector 4)))
                 (setf (nibbles:ub32ref/be v 0) (provider-id provider))
                 v)
               handle))

;; ----------------------------------------

;; for the mount protocol
(defgeneric nfs-provider-mount (provider client)
  (:documentation "Mount a client. Returns the handle for the mount point."))
(defmethod nfs-provider-mount ((provider nfs-provider) client)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-unmount (provider client)
  (:documentation "Unmount a client. No meaningful return value."))
(defmethod nfs-provider-unmount ((provider nfs-provider) client)
  (error 'nfs-error :stat :access))

;; for nfs
(defgeneric nfs-provider-attrs (provider handle)
  (:documentation "Optionally returns the FATTR3 structure for the handle object. 
Providers should always return the attributes for the handle if possible."))
(defmethod nfs-provider-attrs ((provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

(defgeneric (setf nfs-provider-attrs) (value provider handle)
  (:documentation "Sets the attributes for the object."))
(defmethod (setf nfs-provider-attrs) (value (provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-lookup (provider dhandle name)
  (:documentation "Find the handle for an object. 
Returns the NFS handle for the object specified by NAME that lives in the directory specified by DHANDLE."))
(defmethod nfs-provider-lookup ((provider nfs-provider) dhandle name)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-access (provider handle access)
  (:documentation "Checks the access privileges for the object. ACCESS shouild be a list of NFS-ACCESS enum symbols.
Returns a list of the valid access privileges, which should be a subset of the flags passed in."))
(defmethod nfs-provider-access ((provider nfs-provider) handle access)
  (error 'nfs-error :stat :access))

;; generics to support the link/symlink functions
(defgeneric nfs-provider-read-link (provider handle)
  (:documentation "Returns the path of the link named by the handle."))
(defmethod nfs-provider-read-link ((provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-create-symlink (provider dhandle name path attrs)
  (:documentation "Create a new symbolic link in directory DHANDLE called NAME. The link should 
point to the PATH and optionally have initial attributes ATTRS. Returns the handle to the newly 
created symlink."))
(defmethod nfs-provider-create-symlink ((provider nfs-provider) dhandle name path attrs)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-link (provider handle dhandle name)
  (:documentation "Create a hard link in directory DHANDLE called NAME, to the file named by HANDLE."))
(defmethod nfs-provider-link ((provider nfs-provider) handle dhandle name)
  (error 'nfs-error :stat :access))

;; general file operators
(defgeneric nfs-provider-read (provider handle offset count)
  (:documentation "Read count bytes from offset from the object. Returns (values bytes eof). EOF should be non-nil if the read finished at the end of the file. If OFFSET + COUNT is larger than the file, then a short read should be returned and EOF set to T. Providers are also free to do short reads at any time, in this case EOF should be nil. It is the NFS client's responsbility to issue further read instructions."))
(defmethod nfs-provider-read ((provider nfs-provider) handle offset count)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-write (provider handle offset bytes)
  (:documentation "Write bytes at offset to the object. Returns the number of bytes written."))
(defmethod nfs-provider-write ((provider nfs-provider) handle offset bytes)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-create (provider dhandle name)
  (:documentation "Create a new file named NAME in directory DHANDLE. Returns the handle for the new file."))
(defmethod nfs-provider-create ((provider nfs-provider) dhandle name)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-remove (provider dhandle name)
  (:documentation "Remove the file named NAME in directory DHANDLE."))
(defmethod nfs-provider-remove ((provider nfs-provider) dhandle name)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-rename (provider from-dhandle from-name to-dhandle to-name)
  (:documentation "Rename the file named by the FROM-DHANDLE/FROM-NAME."))
(defmethod nfs-provider-rename ((provider nfs-provider) from-dhandle from-name to-dhandle to-name)
  (error 'nfs-error :stat :access))

;; directory operators
(defgeneric nfs-provider-read-dir (provider dhandle)
  (:documentation "Returns a list of all object (file and directory) names in the directory."))
(defmethod nfs-provider-read-dir ((provider nfs-provider) dhandle)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-create-dir (provider dhandle name)
  (:documentation "Create a new directory. Returns the handle for the newly created directory."))
(defmethod nfs-provider-create-dir ((provider nfs-provider) dhandle offset)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-remove-dir (provider dhandle name)
  (:documentation "Remove a directory named NAME in directory DHANDLE."))
(defmethod nfs-provider-remove-dir ((provider nfs-provider) dhandle name)
  (error 'nfs-error :stat :access))

;; special device files
(defgeneric nfs-provider-create-device (provider type dhandle name &key attrs specdata)
  (:documentation "Create a special device. Type should be an ftype3 enum. Returns the handle for the newly created device."))
(defmethod nfs-provider-create-device ((provider nfs-provider) type dhandle name &key attrs specdata)
  (declare (ignore attrs specdata))
  (error 'nfs-error :stat :access))

;; filesystem information
(defgeneric nfs-provider-fs-info (provider handle)
  (:documentation "Returns dynamic filesystem information, in an FS-INFO structure."))
(defmethod nfs-provider-fs-info ((provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-fs-stat (provider handle)
  (:documentation "Returns static filesystem information, in an FS-STAT structure."))
(defmethod nfs-provider-fs-stat ((provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

(defgeneric nfs-provider-path-conf (provider handle)
  (:documentation "Returns a PATH-CONF structure containing information about the filesystem."))
(defmethod nfs-provider-path-conf ((provider nfs-provider) handle)
  (error 'nfs-error :stat :access))

;; commit to storage
(defgeneric nfs-provider-commit (provider handle offset count)
  (:documentation "Commit any pending writes to stable storage."))
(defmethod nfs-provider-commit ((provider nfs-provider) handle offset count)
  (error 'nfs-error :stat :access))


