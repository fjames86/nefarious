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
   (position :initform 0 
	     :accessor nfs-file-stream-position
	     :documentation "Stores the current position in the remote file.")
   (end-p :initform nil :accessor nfs-file-stream-end-p)
   (max-buffer-size :initform +default-stream-buffer-size+ :initarg :max-buffer-size :reader nfs-file-stream-max-buffer-size)
   (buffer-size :initform 0 :initarg :buffer-size :accessor nfs-file-stream-buffer-size
		:documentation "Stores the count of the valid bytes in the buffer, could be less than the maximium size.")
   (buffer-pos :initform 0 :accessor nfs-file-stream-buffer-pos
	       :documentation "Stores the current position in the buffer that should next be accessed.")
   (buffer :initform nil :initarg :buffer :reader nfs-file-stream-buffer)
   (attrs :initarg :attrs :accessor nfs-file-stream-attrs)
   (call-args :initform nil :initarg :call-args :reader nfs-file-stream-call-args)))

(defun fill-buffer (stream)
  "Read from the file into the local buffer."
  (declare (type nfs-file-stream stream))
  (multiple-value-bind (data eof attrs)
      (apply #'call-read 
	     (nfs-file-stream-fh stream)
	     (nfs-file-stream-position stream)
	     (nfs-file-stream-max-buffer-size stream)
	     (nfs-file-stream-call-args stream))
    (let ((count (length data)))
      (dotimes (i count)
	(setf (aref (nfs-file-stream-buffer stream) i)
	      (aref data i)))
      (setf (nfs-file-stream-buffer-pos stream) 0
	    (nfs-file-stream-buffer-size stream) count
	    (nfs-file-stream-end-p stream) eof
	    (nfs-file-stream-attrs stream) attrs)
      (incf (nfs-file-stream-position stream) count)))
  nil)

(defun flush-buffer (stream)
  "Write the contents of the local buffer out to the file."
  (apply #'call-write
	 (nfs-file-stream-fh stream)
	 (nfs-file-stream-position stream)
	 (if (= (nfs-file-stream-buffer-size stream) (nfs-file-stream-max-buffer-size stream))
	     (nfs-file-stream-buffer stream)
	     (subseq (nfs-file-stream-buffer stream) 0 (nfs-file-stream-buffer-size stream)))
	 (nfs-file-stream-call-args stream))
  (incf (nfs-file-stream-position stream) (nfs-file-stream-buffer-pos stream))
  (setf (nfs-file-stream-buffer-pos stream) 0
	(nfs-file-stream-buffer-size stream) 0)
  nil)

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

(defun check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
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
  "Returns the index of last byte read."
  (declare (fixnum start end))
  (let ((count (- end start)))
    (do ((offset start))
	((zerop count) offset)
      ;; copy from buffer-pos to buffer-size bytes into sequence
      (do ((i (nfs-file-stream-buffer-pos stream) (1+ i)))
	  ((= i (nfs-file-stream-buffer-size stream)))
	(setf (elt sequence (+ offset i))
	      (aref (nfs-file-stream-buffer stream) i)))
      (incf offset (- (nfs-file-stream-buffer-size stream)
		      (nfs-file-stream-buffer-pos stream)))
      (decf count (- (nfs-file-stream-buffer-size stream)
		      (nfs-file-stream-buffer-pos stream)))
      
      ;; when we're at the end of the file just set the count to zero to exit 
      ;; otherwise refill the buffer 
      (if (nfs-file-stream-end-p stream)
	  (setf count 0)
	  (fill-buffer stream)))))
        
(defmethod stream-write-sequence ((stream nfs-file-stream) sequence start end &key)
  "Returns the index of last byte written."
  (do ((count (- end start))
       (offset start))
      ((= count 0) offset)
    ;; write bytes into the local buffer then flush it 
    (do ((i (nfs-file-stream-buffer-pos stream) (1+ i)))
	((= i (nfs-file-stream-max-buffer-size stream)))
      (setf (aref (nfs-file-stream-buffer stream) i)
	    (elt sequence (+ offset i))))
    (incf offset (- (nfs-file-stream-max-buffer-size stream)
		    (nfs-file-stream-buffer-pos stream)))
    (decf count (- (nfs-file-stream-max-buffer-size stream)
		    (nfs-file-stream-buffer-pos stream)))
    (flush-buffer stream)))

;; to support read-byte and write-byte we need to have some form of buffering
;; otherwise each operation would involve an rpc call -- very expensive!
;; we can just use the read/write sequence methods
(defmethod stream-read-byte ((stream nfs-file-stream))
  "Returns the byte or :EOF"
  ;; if the buffer has been completely read then refill it 
  (when (= (nfs-file-stream-buffer-pos stream)
	   (nfs-file-stream-buffer-size stream))
    (fill-buffer stream))

  (cond 
    ((and (= (nfs-file-stream-buffer-pos stream)
	     (nfs-file-stream-buffer-size stream))
	  (nfs-file-stream-end-p stream))
     :eof)
    (t 
     (let ((byte (aref (nfs-file-stream-buffer stream)
		       (nfs-file-stream-buffer-pos stream))))
       (incf (nfs-file-stream-buffer-pos stream))
       byte))))
  
(defmethod stream-write-byte ((stream nfs-file-stream) byte)
  "write the byte to the local buffer, flush it if at the end of the buffer"
  (setf (aref (nfs-file-stream-buffer stream)
	      (nfs-file-stream-buffer-pos stream))
	byte)
  (incf (nfs-file-stream-buffer-pos stream))
  (incf (nfs-file-stream-buffer-size stream))
  (when (= (nfs-file-stream-buffer-size stream)
	   (nfs-file-stream-max-buffer-size stream))
    (flush-buffer stream)))
  
;; need to implement these if using buffered byte methods
(defmethod stream-finish-output ((stream nfs-file-stream))
  (flush-buffer stream))

(defmethod stream-force-output ((stream nfs-file-stream))
  (flush-buffer stream))


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

(defmacro with-nfs-mount ((var path &rest call-args) &body body)
  (alexandria:with-gensyms (gpath gargs)
    `(let ((,gpath ,path)
	   (,gargs (list ,@call-args)))
       (let ((,var (apply #'nefarious.mount:call-mount ,gpath ,gargs)))
	 (unwind-protect (progn ,@body)
	   (apply #'nefarious.mount:call-unmount ,gpath ,gargs))))))

(defmacro with-nfs-file ((var dhandle filename &rest call-args) &body body)
  `(let ((,var (make-nfs-file-stream ,dhandle ,filename ,@call-args)))
     ,@body))
