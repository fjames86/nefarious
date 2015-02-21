

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


(defstruct (handle (:constructor %make-handle))
  fh ;; nfs file handle 
  hash ;; pathname hash
  pathname
  parent
  children)

(defun make-handle (pathname)
  (let ((hash (sxhash pathname)))
    (%make-handle 
     :pathname pathname
     :hash hash
     :fh (let ((h (nibbles:make-octet-vector +nefarious-handle-size+)))
	   (setf (nibbles:ub64ref/be h 0) 
		 hash)
	   h))))

;; the list of top-level directories that we export i.e. that are shared
;; using nfs
(defparameter *exports* nil)

(defun export-handles (directory)
  (let ((handles nil))
    (cl-fad:walk-directory directory 
			   (lambda (pathname)
			     (push (make-handle pathname) handles)))
    handles))
