;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:nefarious.finfo 
  (:use #:cl #:cffi)
  (:export #:get-file-information
	   #:file-information-ctime
	   #:file-information-atime
	   #:file-information-mtime
	   #:file-information-size))

(in-package #:nefarious.finfo)


;; we need to query the local filesystem to get various information
;; such as access rights and a/c/m times

(defstruct file-information
  ctime atime mtime size)

#+(or windows win32)
(progn

;; for errors
(defcfun (%format-message "FormatMessageA" :convention :stdcall)
    :uint32
  (flags :uint32)
  (source :pointer)
  (msg-id :uint32)
  (lang-id :uint32)
  (buffer :pointer)
  (size :uint32)
  (args :pointer))

(defun format-message (code)
  "Use FormatMessage to convert the error code into a system-defined string."
  (with-foreign-object (buffer :char 1024)
    (let ((n (%format-message #x00001000
			      (null-pointer)
			      code
			      0
			      buffer
			      1024
			      (null-pointer))))
      (if (= n 0)
	  (error "Failed to format message")
	  (foreign-string-to-lisp buffer :count (- n 2))))))

(define-condition win-error (error)
  ((code :initform 0 :initarg :code :reader win-error-code))
  (:report (lambda (condition stream)
	     (format stream "ERROR ~A: ~A" 
		     (win-error-code condition)
		     (format-message (win-error-code condition))))))
	   
(defcfun (%get-last-error "GetLastError" :convention :stdcall) :long)

(defun get-last-error ()
  (let ((code (%get-last-error)))
    (unless (zerop code)
      (error 'win-error :code code))))

(defcfun (%create-file "CreateFileA" :convention :stdcall)
    :pointer 
  (filename :string)
  (access :uint32)
  (mode :uint32)
  (attrs :pointer)
  (disposition :uint32)
  (flags :uint32)
  (template :pointer))

(defcfun (%close-handle "CloseHandle" :convention :stdcall)
    :void
  (handle :pointer))

(defcstruct filetime 
  (low :uint32)
  (high :uint32))

(defcstruct systemtime 
  (year :uint16)
  (month :uint16)
  (wday :uint16)
  (day :uint16)
  (hour :uint16)
  (min :uint16)
  (sec :uint16)
  (milli :uint16))

(defcfun (%filetime-to-systemtime "FileTimeToSystemTime" :convention :stdcall)
    :boolean
  (ft :pointer)
  (st :pointer))

(defun convert-time (ft)
  (with-foreign-object (st '(:struct systemtime))
    (let ((res (%filetime-to-systemtime ft st)))
      (cond
	(res
	 (with-foreign-slots ((sec min hour day month year) st (:struct systemtime))
	   (- (encode-universal-time sec min hour day month year)
	      #.(encode-universal-time 0 0 0 1 1 1970))))
	(t (get-last-error))))))

(cffi:defcstruct file-info
  (attrs :uint32)
  (ctime (:struct filetime)) ;; file creation time
  (atime (:struct filetime))
  (mtime (:struct filetime))
  (vserial :uint32)
  (size-high :uint32)
  (size-low :uint32)
  (nlinks :uint32)
  (index-high :uint32)
  (index-low :uint32))

(cffi:defcfun (%get-file-information "GetFileInformationByHandle" :convention :stdcall)
    :boolean
  (handle :pointer)
  (info :pointer))

(defun get-file-information (pathspec)
  (let ((path (format nil "~A" (truename pathspec)))
	(info (make-file-information)))
    (flet ((getinfo ()
	     (let ((handle (with-foreign-string (p path)
			     (%create-file path 
					   #x10000000 ;; generic all
					   0 ;; exclusive
					   (cffi:null-pointer)
					   4 ;; open always
					   0 
					   (cffi:null-pointer)))))
	       (when (pointer-eq handle 
				 (make-pointer #+(or x86-64 x64 amd64)#xffffffffffffffff
					       #-(or x86-64 x64 amd64)#xffffffff))
		 (get-last-error))
	       (unwind-protect 
		    (cffi:with-foreign-object (i '(:struct file-info))
		      (let ((res (%get-file-information handle i)))
			(cond
			  (res
			   (setf (file-information-size info)
				 (+ (foreign-slot-value i '(:struct file-info) 'size-low)
				    (ash (foreign-slot-value i '(:struct file-info) 'size-high)
					 32))
				 (file-information-ctime info)
				 (convert-time (foreign-slot-pointer i '(:struct file-info) 'ctime))
				 (file-information-atime info)
				 (convert-time (foreign-slot-pointer i '(:struct file-info) 'atime))
				 (file-information-mtime info)
				 (convert-time (foreign-slot-pointer i '(:struct file-info) 'mtime))))
			  (t (get-last-error)))))
		 (%close-handle handle)))))
      (handler-case (getinfo)
	(error (e) 
	  (log:debug "~A" e)))
      info)))


)

#-(or windows win32)
(progn

(defctype time-t 
    #+(or x86-64 x64 amd64):uint64 
    #-(or x86-64 x64 amd64):uint32)

(defcstruct stat-info
  (inode :uint32)
  (mode :uint32)
  (nlink :uint32)
  (uid :uint32)
  (gid :uint32)
  (dev :uint32)
  (size time-t)
  (blksize :uint32)
  (blkcnt :uint32)
  (atime time-t)
  (mtime time-t)
  (ctime time-t)) ;; file change time 

(defcfun (%lstat "lstat")
    :int32
  (path :string)
  (stat :pointer))

(defun get-file-information (pathspec)
  (let ((info (make-file-information)))
    (flet ((getinfo ()
	     (with-foreign-string (s (format nil "~A" (truename pathspec)))
	       (with-foreign-object (i '(:struct stat-info))
		 (let ((res (%lstat s i)))
		   (cond
		     ((zerop res)
		      (setf (file-information-ctime info)
			    (foreign-slot-value i '(:struct stat-info) 'ctime)
			    (file-information-atime info)
			    (foreign-slot-value i '(:struct stat-info) 'atime)
			    (file-information-mtime info)
			    (foreign-slot-value i '(:struct stat-info) 'mtime)
			    (file-information-size info)
			    (foreign-slot-value i '(:struct stat-info) 'size)))
		     (t nil)))))))
      (handler-case (getinfo)
	(error (e)
	  (log:debug "~A" e)))
      info)))

)

