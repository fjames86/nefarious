;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:nefarious)

;; define a stream type to access remote files
;; general structure is copied from flexi-streams

(defclass nfs-file-stream (trivial-gray-stream-mixin 
			   fundamental-binary-input-stream 
			   fundamental-binary-output-stream)
  (#+cmu
   (open-p :initform t
           :accessor nfs-file-stream-open-p
           :documentation "For CMUCL we have to keep track of this manually.")
   (fh :initform nil :initarg :fh :reader nfs-file-stream-fh)
   (position :initform 0 :accessor nfs-file-stream-position)
   (end-p :initform nil :accessor nfs-file-stream-end-p)
   (call-args :initform nil :initarg :call-args :reader nfs-file-stream-call-args)))

#+:cmu
(defmethod open-stream-p ((stream nfs-file-stream))
  "Returns a true value if STREAM is open.  See ANSI standard."
  (declare #.*standard-optimize-settings*)
  (nfs-file-stream-open-p stream))

#+:cmu
(defmethod close ((stream nfs-file-stream) &key abort)
  "Closes the stream STREAM.  See ANSI standard."
  (declare #.*standard-optimize-settings*)
  (declare (ignore abort))
  (prog1
      (nfs-file-stream-open-p stream)
    (setf (nfs-file-stream-open-p stream) nil)))

(defmethod check-if-open ((stream nfs-file-stream))
  "Checks if STREAM is open and signals an error otherwise."
;;  (declare #.*standard-optimize-settings*)
  (unless (open-stream-p stream)
    (error "stream closed")))

(defmethod stream-element-type ((stream nfs-file-stream))
  "The element type is always OCTET by definition."
;;  (declare #.*standard-optimize-settings*)
  '(unsigned-byte 8))

(defmethod stream-read-sequence ((stream nfs-file-stream) sequence start end &key)
;;  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (let ((count (- end start)))
    (multiple-value-bind (bytes eof attr) 
	   (apply #'call-read 
		  (nfs-file-stream-fh stream) 
		  (nfs-file-stream-position stream)
		  count
		  (nfs-file-stream-call-args stream))
      (declare (ignore attr))
      (when eof (setf (nfs-file-stream-end-p stream) t))
      (let ((length (length bytes)))
	(do ((i 0 (1+ i)))
	    ((= i (min length count)) 
	     (nfs-file-stream-position stream))
	  (setf (elt sequence (+ i start)) (aref bytes i))
	  (incf (nfs-file-stream-position stream)))))))
  
(defmethod stream-write-sequence ((stream nfs-file-stream) sequence start end &key)
  (apply #'call-write
	 (nfs-file-stream-fh stream)
	 (nfs-file-stream-position stream)
	 (subseq sequence start end)
	 (nfs-file-stream-call-args stream))
  sequence)

(defmethod stream-file-position ((stream nfs-file-stream))
  "Simply returns the index into the underlying vector."
;;  (declare #.*standard-optimize-settings*)
  (nfs-file-stream-position stream))

(defmethod (setf stream-file-position) (position-spec (stream nfs-file-stream))
  "Sets the index into the underlying vector if POSITION-SPEC is acceptable."
;;  (declare #.*standard-optimize-settings*)
  (setf (nfs-file-stream-position stream)
	(case position-spec
	  (:start 0)
	  (:end (error "Can't set to end of file -- don't know file size"))
	  (otherwise 
	   (unless (integerp position-spec) (error "Must be integer"))
	   position-spec)))
  (nfs-file-stream-position stream))

(defun make-nfs-file-stream (dhandle filename &rest call-args)
  (let ((fh (apply #'call-lookup dhandle filename call-args)))
    (make-instance 'nfs-file-stream
		   :fh fh
		   :call-args call-args)))

(defmacro with-nfs-file ((var dhandle filename &rest call-args) &body body)
  `(let ((,var (make-nfs-file-stream ,dhandle ,filename ,@call-args)))
     ,@body))
