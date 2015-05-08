;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;;; This shows how to mirror local changes up to a remote NFS server, i.e. we 
;;; are modifying files locally, and wish to push the changes out to a remote NFS so that 
;;; the local changes are reflected there.
;;; This is useful because it means you can "share" files without special treatment from 
;;; your host OS.
;;; A potential usecase might be to aid interactive development with a remote Lisp system.

(defpackage #:nefarious.upload 
  (:use #:cl #:nefarious #:frpc)
  (:export #:make-uploader
	   #:upload-changes))

(in-package #:nefarious.upload)

;; want to keep track of the state of files and only push the parts that have changed
;; do this by subdividing the files up into blocks of 4k each and computing a hash on each block
;; if the hash changes then the block is pushed out  

(defstruct fblock
  offset 
  size
  hash)
  
(defstruct file
  pathspec
  blocks
  timestamp
  handle
  client)

(defun hash (array &key (start 0) end)
  (do ((h 0 (mod h (expt 2 32)))
       (i start (1+ i)))
      ((= i (or end (length array)))
       (progn (incf h (ash h 3))
              (setf h (logxor (ash h -11)))
              (incf h (ash h 15))
              (mod h (expt 2 32))))
    (incf h (aref array i))
    (setf h (ash h 10))
    (setf h (logxor h (ash h -6)))))

(defun upload-changes (file)
  "Push changes to the file out to the remote file. 
FILE ::= a file uploader structure, as returned from MAKE-UPLOADER."
  (declare (type file file))
  (with-open-file (f (file-pathspec file) :direction :input :element-type '(unsigned-byte 8))
    (if (= (file-write-date f) (file-timestamp file))
	file 
	(let ((size (file-length f))
	      (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
	  (do ((offset 0 (+ offset 4096))
	       (blocks nil)
	       (oblocks (file-blocks file) (cdr oblocks)))
	      ((>= offset size) 
	       (setf (file-blocks file) (nreverse blocks)
		     (file-timestamp file) (file-write-date (file-pathspec file)))
	       file)
	    (let* ((n (read-sequence buffer f))
		   (hash (hash buffer :start 0 :end n)))
	      (push (make-fblock :offset offset
				 :size n
				 :hash hash)
		    blocks)
	      (let ((ob (car oblocks)))
		(when (and ob (not (= (fblock-hash ob) hash)))
		  ;; hashes differ, push changes
		  (nfs:call-write (file-handle file)
				  offset
				  buffer 
				  :end (unless (= n 4096) n)
				  :client (file-client file))))))))))


(defun make-uploader (pathspec handle client)
  "Make a file uploader which can push changes out to the remote file.
PATHSPEC ::= path of the local file we are tracking
HANDLE ::= NFS handle of the remote file to push changes to
CLIENT ::= rpc-client instance representing the NFS server 

Returns the uploader structure to use in further calls to UPLOAD-CHANGES.
"
  (declare (type rpc-client client))
  (let ((file (make-file :pathspec pathspec
			 :client client
			 :timestamp 0
			 :handle handle)))
    (upload-changes file)))


