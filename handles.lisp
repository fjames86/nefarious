;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:nefarious.handles 
  (:use #:cl)
  (:nicknames #:nfs.handles)
  (:export #:handle-hash
	   #:handle-fh	   
	   #:handle-pathname
	   #:handle-directory-p
	   #:handle-parent
	   #:handle-children

	   #:make-handle
	   #:allocate-handle
	   #:allocate-dhandle
	   #:find-handle
	   #:export-directory
	   #:find-export 
	   
	   #:read-file
	   #:write-file
	   #:create-file
	   #:remove-file
	   #:create-directory
	   #:remove-directory))

(in-package #:nefarious.handles)

(defparameter *handles* nil)

(defstruct (handle (:constructor %make-handle))
  hash
  fh ;; nfs file handle i.e. an octet array
  pathname
  directory-p
  parent ;; nfs directory handle of the parent (or nil if toplevel export directory)
  children) ;; nfs handles of children

(defun make-handle (dhandle name)
  (declare (type handle dhandle)
	   (type string name))
  (let* ((pathname (cl-fad:merge-pathnames-as-file (handle-pathname dhandle)
						   name))
	 (hash (sxhash pathname))
	 (handle 
	  (%make-handle :hash hash
			:fh (let ((v (nibbles:make-octet-vector 8)))
			      (setf (nibbles:ub64ref/be v 0) hash)
			      v)
			:pathname pathname
			:parent (handle-fh dhandle)
			:directory-p (and (cl-fad:directory-pathname-p pathname)
					  (cl-fad:directory-exists-p pathname)
					  t))))
    handle))

(defun make-dhandle (dhandle name)
  (declare (type handle dhandle)
	   (type string name))
  (let* ((pathname (cl-fad:merge-pathnames-as-directory (handle-pathname dhandle)
							name))
	 (hash (sxhash pathname))
	 (handle 
	  (%make-handle :hash hash
			:fh (let ((v (nibbles:make-octet-vector 8)))
			      (setf (nibbles:ub64ref/be v 0) hash)
			      v)
			:pathname pathname
			:parent (handle-fh dhandle)
			:directory-p (and (cl-fad:directory-pathname-p pathname)
					  (cl-fad:directory-exists-p pathname)
					  t))))
    handle))

(defstruct (mount-handle (:constructor %make-mount-handle)
			 (:include handle))
  export-path)

(defun make-mount-handle (path export-path)
  (let* ((pathname (cl-fad:pathname-as-directory path))
	 (hash (sxhash pathname))
	 (handle 
	  (%make-mount-handle :hash hash
			      :fh (let ((v (nibbles:make-octet-vector 8)))
				    (setf (nibbles:ub64ref/be v 0) hash)
				    v)
			      :pathname pathname
			      :directory-p (and (cl-fad:directory-pathname-p pathname)
						(cl-fad:directory-exists-p pathname)
						t)
			      :export-path export-path)))
    handle))

(defun find-handle (fh)
  (find-if (lambda (handle)
	     (equalp (handle-fh handle) fh))
	   *handles*))

(defun allocate-handle (dhandle name)
  "Create an NFS handle for the file named NAME in the directory specified by DHANDLE and store it in 
the handles list. Returns the newly allocated handle."
  (let ((handle (make-handle dhandle name)))
    ;; if the file does not exist then error
    (unless (cl-fad:file-exists-p (handle-pathname handle))
      (return-from allocate-handle nil))

    ;; the file exists, push it onto the list and the parent's children list
    (unless (find-handle (handle-fh handle))
      (push handle *handles*)
      (push (handle-fh handle) (handle-children dhandle)))

    handle))

(defun allocate-dhandle (dhandle name)
  (let ((handle (make-dhandle dhandle name)))
    (unless (cl-fad:directory-exists-p (handle-pathname handle))
      (return-from allocate-dhandle nil))
    (push handle *handles*)
    (push handle (handle-children dhandle))
    handle))

(defparameter *exports* nil)

(defun export-directory (path export-path)
  (let ((handle (make-mount-handle path export-path)))
    (push handle *exports*)
    (push handle *handles*)
    handle))

(defun find-export (dpath)
  (declare (type string dpath))
  (find dpath *exports* 
	:key #'mount-handle-export-path
	:test #'string-equal))

;; ----------- file operations -------------------


(defun read-file (handle offset count)
  (with-open-file (f (handle-pathname handle) 
		     :direction :input
		     :element-type '(unsigned-byte 8))
    (file-position f offset)
    (let ((buffer (nibbles:make-octet-vector count)))
      (let ((n (read-sequence buffer f)))
	(subseq buffer 0 n)))))

(defun write-file (handle offset buffer)
  (with-open-file (f (handle-pathname handle)
		     :direction :output
		     :if-does-not-exist :error
		     :element-type '(unsigned-byte 8))
    (file-position f offset)
    (length (write-sequence buffer f))))

(defun create-file (dhandle name)
  (let ((handle (make-handle dhandle name)))
    (with-open-file (f (handle-pathname handle)
		       :direction :output 
		       :if-exists :error
		       :if-does-not-exist :create)))
  (allocate-handle dhandle name))

(defun remove-file (dhandle name)
  (let ((handle (make-handle dhandle name)))
    (delete-file (handle-pathname handle))))

(defun create-directory (dhandle name)
  (let ((handle (allocate-dhandle dhandle name)))
    (when handle
      (ensure-directories-exist (handle-pathname handle))
      handle)))

(defun remove-directory (dhandle name)
  (let ((handle (make-dhandle dhandle name)))    
    (cl-fad:delete-directory-and-files (handle-pathname handle))))
