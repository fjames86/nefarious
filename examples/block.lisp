;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:nefarious.block
  (:use #:cl #:frpc #:nefarious #:cffi)
  (:nicknames #:nfs.block)
  (:export #:make-block-provider))

(in-package #:nefarious.block)

;; This example shows how to export block storage devices using NFS
;; For simplicity, the underlying storage is provided by a mmaped file (see the library pounds).


(defclass block-provider (nfs-provider)
  ((mappings :initarg :mappings :reader mappings)
   (mount-fh :initform #(0 0 0 0) :reader mount-fh)))
   
(defun make-block-provider (mappings)
  "Mapping should be a list of POUNDS mapping structures, as returned from a call to OPEN-MAPPING."
  (make-instance 'block-provider 
		 :mappings 
		 (do ((%mappings mappings (cdr mappings))
		      (i 1 (1+ i))
		      (maps nil))
		     ((null %mappings) maps)
		   (push (list :fh (pack #'frpc::write-uint32 i)
			       :mapping (car %mappings)
			       :stream (pounds:make-mapping-stream (car %mappings))
			       :id i
			       :name (format nil "dev~A" i))
			 maps))))

(defun find-mapping (provider &key fh name)
  (find-if (lambda (mapping)
	     (cond
	       (fh (equalp (getf mapping :fh) fh))
	       (name (string= (getf mapping :name) name))))
	   (mappings provider)))

;; for the mount protocol
(defmethod nfs-provider-mount ((provider block-provider) client)
  (mount-fh provider))

(defmethod nfs-provider-unmount ((provider block-provider) client)
  nil)

;; for nfs
(defmethod nfs-provider-attrs ((provider block-provider) fh)
  (cond
    ((equalp fh (mount-fh provider))
     (make-fattr3 :type :dir
		  :mode (pack-mode :owner '(:read :write :execute)                                          
				   :group '(:read :write)
				   :others '(:read))
		  :uid 0
		  :gid 0
		  :size 0
		  :used 0
		  :fileid 0
		  :atime (make-nfs-time3)
		  :mtime (make-nfs-time3)
		  :ctime (make-nfs-time3)))
    (t 
     (let ((mapping (find-mapping provider :fh fh)))
       (if mapping 
	   (make-fattr3 :type :blk
			:mode (pack-mode :owner '(:read :write)
					 :group '(:read :write)
					 :others '(:read))
			:uid 0
			:gid 0
			:size (pounds:mapping-size (getf mapping :mapping)) 
			:used (pounds:mapping-size (getf mapping :mapping))
			:fileid (getf mapping :id)
			:atime (make-nfs-time3)
			:mtime (make-nfs-time3)
			:ctime (make-nfs-time3))
	   (error 'nfs-error :stat :bad-handle))))))

;; don't support this
;;(defmethod (setf nfs-provider-attrs) (value (provider simple-provider) handle)
;;  nil)

(defmethod nfs-provider-lookup ((provider block-provider) dh name)  
  (cond
    ((equalp dh (mount-fh provider))
     (let ((mapping (find-mapping provider :name name)))
       (if mapping 
	   (getf mapping :fh)
	   (error 'nfs-error :stat :noent))))
    (t 
     (error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-read ((provider block-provider) fh offset count)
  "Read count bytes from offset from the object."
  (let ((mapping (find-mapping provider :fh fh)))
    (if mapping 
	(let ((stream (getf mapping :stream)))
	  (file-position stream offset)
	  (let ((buffer (nibbles:make-octet-vector count)))
	    (read-sequence buffer stream)
	    buffer))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-write ((provider block-provider) fh offset bytes)
  "Write bytes at offset to the object. Returns the number of bytes written."
  (nfs-log :info "Block FH ~S" fh)
  (let ((mapping (find-mapping provider :fh fh)))
    (if mapping 
	(let ((stream (getf mapping :stream)))
	  (file-position stream offset)
	  (write-sequence bytes stream))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-read-dir ((provider block-provider) dh &key)
  "Returns a list of all object (file and directory) names in the directory."
  (unless (equalp dh (mount-fh provider))
    (error 'nfs-error :stat :bad-handle))
  (mapcar (lambda (mapping)
	    (getf mapping :name))
	  (mappings provider)))

;; filesystem information
(defmethod nfs-provider-fs-info ((provider block-provider) handle)
  "Returns dynamic filesystem information, in an FS-INFO structure."
  (make-fs-info :attrs (nfs-provider-attrs provider handle) ;; attributes of the file 
		:rtmax 4096 ;; maximum read request count
		:rtpref 4096 ;; preferred read count -- should be same as rtmax
		:rtmult 512 ;; suggested multiple for read requests
		:wtmax 4096 ;; maximum write request count 
		:wtpref 4096 ;; preferred write count
		:wtmult 512 ;; suggested multiple for writes
		:dtpref #xffffffff ;; preferred size for read-dir
		:max-fsize #xffffffff ;; maximum file size
		:time-delta (make-nfs-time3 :seconds 1)
		:properties (enum 'nefarious::nfs-info :homogenous)))

(defmethod nfs-provider-fs-stat ((provider block-provider) handle)
  "Returns static filesystem information, in an FS-STAT structure."
  (make-fs-stat :attrs (nfs-provider-attrs provider handle) ;; fileattribvutes
		:tbytes #xffffffff ;; total size of the filesystem
		:fbytes #xffffffff ;; free bytes
		:abytes #xffffffff ;; available bytes
		:tfiles #xffffffff ;; total file slots
		:ffiles #xffffffff ;; total number of free file slots
		:afiles #xffffffff ;; available file slots
		:invarsec 1))

(defmethod nfs-provider-path-conf ((provider block-provider) handle)
  "Returns a PATH-CONF structure containing information about the filesystem."
  (make-path-conf :attr (nfs-provider-attrs provider handle) ;; file attributes
		  :link-max 0 ;; max link size
		  :link-max 0 ;; maximum number of hard links to an object
		  :name-max 255 ;; maximum file name
		  :no-trunc t ;; if T the server will reject any request with a name longer than max name
		  :chown-restricted t ;; will reject any attempt to chown
		  :case-insensitive t ;; case insensitive filesystem
		  :case-preserving t))



		   
