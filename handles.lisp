

(in-package #:nefarious)

(defconstant +nefarious-handle-size+ 32)

;; we have at least 32 bytes to play with in our file handle (possibily up to 64)
;; what should we store in it?
;; traditionally you would store device, inode and generation numbers
;; but we don't really have access to these.
;; you need a system like this so that the handle contains all the information
;; to find the file, meaning you don't need to hold a cache mapping handles to files
;; this is what allows the protocol to be stateless, since all the state is held by 
;; the client in the form of handles
;; 

(defparameter *nfs-handles* nil)

(defstruct (handle (:constructor %make-handle))
  hash
  nfs-fh
  pathname)

(defun make-handle (pathname)
  (let ((hash (sxhash pathname)))
    (%make-handle :hash hash
		  :nfs-fh (let ((v (nibbles:make-octet-vector 8)))
			    (setf (nibbles:ub64ref/be v 0) hash)
			    v)
		  :pathname pathname)))

(defun allocate-handle (pathname)
  (unless (cl-fad:file-exists-p pathname)
    (error "File doesn't exist"))
  (let ((h (make-handle pathname)))
    (push h *nfs-handles*)
    h))

(defun find-handle (&key nfs-fh pathname)
  (cond
    (nfs-fh
     (find-if (lambda (handle)
		(every #'= (handle-nfs-fh handle) nfs-fh))
	      *nfs-handles*))
    (pathname
     (find-if (lambda (handle)
		(equal (handle-pathname handle) pathname))
	      *nfs-handles*))
    (t (error "Must supply an nfs-fh or pathname"))))

;; the list of top-level directories that we export i.e. that are shared
;; using nfs
(defparameter *exports* nil)

(defun export-handles (directory)
  (let ((handles nil))
    (cl-fad:walk-directory directory 
			   (lambda (pathname)
			     (push (make-handle pathname) handles)))
    handles))
