;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:nefarious)

;; define a stream type to access remote files
;; general structure is copied from flexi-streams

(defconstant +default-stream-buffer-size+ 1024)

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
   (buffer-size :initform +default-stream-buffer-size+ :initarg :buffer-size :reader nfs-file-stream-buffer-size)
   (buffer :initform (make-array +default-stream-buffer-size+ :element-type '(unsigned-byte))
	   :initarg :buffer :reader nfs-file-stream-buffer)   
   (buffer-pos :initform 0 :accessor nfs-file-stream-buffer-pos)   
   (attrs :initarg :attrs :reader nfs-file-stream-attrs)
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

;; use this to check if there are more bytes to read
(defmethod stream-listen ((stream nfs-file-stream))
  "checks whether there are bytes left to read"
  (check-if-open stream)
  (nfs-file-stream-end-p stream))

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
	  (:end (let ((attrs (nfs-file-stream-attrs stream)))
		  (fattr3-size attrs)))
	  (otherwise 
	   (unless (integerp position-spec) (error "Must be integer"))
	   position-spec)))
  (nfs-file-stream-position stream))





(defmethod stream-read-sequence ((stream nfs-file-stream) sequence start end &key)
;;  (declare #.*standard-optimize-settings*)
  (declare (fixnum start end))
  (let ((count (- end start)))
    (multiple-value-bind (bytes eof attrs) 
	   (apply #'call-read 
		  (nfs-file-stream-fh stream) 
		  (nfs-file-stream-position stream)
		  count
		  (nfs-file-stream-call-args stream))
      ;; update the attrs for the file
      (setf (nfs-file-stream-attrs stream) attrs)
      ;; check whether it's at the end
      (when eof (setf (nfs-file-stream-end-p stream) t))
      ;; copy the bytes into the sequence
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
  ;; increase the file position
  (incf (nfs-file-stream-position stream) (- end start))
  sequence)


;; to support read-byte and write-byte we need to have some form of buffering
;; otherwise each operation would involve an rpc call -- very expensive!
;; we can just use the read/write sequence methods
(defmethod stream-read-byte ((stream nfs-file-stream))
  "Returns the byte or :EOF"
  :eof)

(defmethod stream-write-byte ((stream nfs-file-stream) byte)
  "write the byte"
  nil)


;; need to implement these if using buffered byte methods
(defmethod stream-finish-output ((stream nfs-file-stream))
  nil)

(defmethod stream-force-output ((stream nfs-file-stream))
  nil)


;; -----------------------------------------


;; FIXME: if the file doesn't exist then create it?
;; FIXME: also store the file attrs that we get from a lookup call?
;; this would give us some information such as file size that could be useful later
;; for instance, it would allow us to set file-position to :end 
(defun make-nfs-file-stream (dhandle filename &key call-args (buffer-size +default-stream-buffer-size+))
  (multiple-value-bind (fh file-attrs dir-attrs) (apply #'call-lookup dhandle filename call-args)
    (declare (ignore dir-attrs))
    (make-instance 'nfs-file-stream
		   :fh fh
		   :call-args call-args
		   :attrs file-attrs
		   :buffer-size buffer-size
		   :buffer (make-array buffer-size :element-type '(unsigned-byte 8)))))

(defmacro with-nfs-file ((var dhandle filename &rest call-args) &body body)
  `(let ((,var (make-nfs-file-stream ,dhandle ,filename ,@call-args)))
     ,@body))
