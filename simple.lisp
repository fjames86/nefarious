;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; providers

(in-package #:nefarious)

;; -------------------------------------------------------------
;; Handles 

(defun pack-fh (id)
  (let ((v (nibbles:make-octet-vector 4)))
    (setf (nibbles:ub32ref/be v 0) id)
    v))
(defun unpack-fh (fh)
  (nibbles:ub32ref/be fh 0))

(defstruct (handle (:constructor %make-handle))
  id ;; file-id 
  fh ;; nfs file handle i.e. an octet array
  pathname
  directory-p
  parent ;; nfs directory handle of the parent (or nil if toplevel export directory)
  children ;; nfs handles of children
  info
  )

(defun make-handle (dhandle name id)
  (declare (type handle dhandle)
	   (type string name))
  (let* ((pathname (cl-fad:merge-pathnames-as-file (handle-pathname dhandle)
						   name))
	 (handle 
	  (%make-handle :id id
			:fh (pack-fh id)
			:pathname pathname
			:parent (handle-fh dhandle)
			:directory-p (or (cl-fad:directory-exists-p pathname)
					 (string= name ".")
					 (string= name ".."))
			:info (nefarious.finfo:get-file-information pathname))))
    (when (handle-directory-p handle)
      (nfs-log :info "Attempt to make a file handle from a directory")
      (return-from make-handle nil))

    handle))

(defun make-dhandle (dhandle name id)
  (declare (type handle dhandle)
	   (type string name))
  (let* ((pathname (cl-fad:merge-pathnames-as-directory (handle-pathname dhandle)
							(let ((lc (char name (1- (length name)))))
							  (if (string= lc #\/)
							      name
							      (concatenate 'string name "/")))))
	 (handle 
	  (%make-handle :id id
			:fh (pack-fh id)
			:pathname pathname
			:parent (handle-fh dhandle)
			:directory-p (and (cl-fad:directory-pathname-p pathname)
					  (cl-fad:directory-exists-p pathname)
					  t)
			:info (nefarious.finfo:get-file-information pathname))))
    handle))

(defun make-mount-handle (path)
  (let* ((pathname (cl-fad:pathname-as-directory path))
	 (handle 
	  (%make-handle :id 0
			:fh (pack-fh 0)
			:pathname pathname
			:directory-p (and (cl-fad:directory-pathname-p pathname)
					  (cl-fad:directory-exists-p pathname)
					  t)
			:info (nefarious.finfo:get-file-information pathname))))
    handle))

;; --------------------


;; the provider class
(defclass simple-provider (nfs-provider)
  ((file-id :initform 1 :accessor simple-provider-id)
   (handles :initform nil :accessor simple-provider-handles
	    :documentation "List of handle structures.")
   (mount-handle :initarg :mount-handle :reader simple-provider-mount-handle
		 :documentation "The handle for the exported (toplevel) directory.")))

(defun next-provider-id (provider)
  (let ((id (simple-provider-id provider)))
    (incf (simple-provider-id provider))
    id))

(defun find-handle (provider fh)
  (find fh (simple-provider-handles provider) :key #'handle-fh :test #'equalp))

(defun remove-handle (provider handle)
  (setf (simple-provider-handles provider)
	(remove handle (simple-provider-handles provider))))

(defun lookup-handle (provider pathname)
  (find-if (lambda (handle)
	     (when (cl-fad:pathname-equal (handle-pathname handle) pathname)
	       (return-from lookup-handle handle)))
	   (simple-provider-handles provider))
    nil)

(defun allocate-handle (provider dhandle name)
  "Create an NFS handle for the file named NAME in the directory specified by DHANDLE and store it in 
the handles list. Returns the newly allocated handle."
  ;; first, attempt to find the handle. if it's already there then just return it 
  (let ((h (lookup-handle provider
			  (cl-fad:merge-pathnames-as-file (handle-pathname dhandle) name))))
    (when h (return-from allocate-handle h)))

  (let ((handle (make-handle dhandle name (next-provider-id provider))))
    ;; if the file does not exist then error
    (unless (cl-fad:file-exists-p (handle-pathname handle))
      (nfs-log :info "file doesn't exist ~S" (handle-pathname handle))
      (return-from allocate-handle nil))

    (push handle (simple-provider-handles provider))
    (push (handle-fh handle) (handle-children dhandle))

    handle))

(defun allocate-dhandle (provider dhandle name)
  "allocate a directory handle"
  (let ((lc (char name (1- (length name)))))
    (unless (string= lc #\/)
      (setf name (concatenate 'string name "/"))))

  (let ((h (lookup-handle provider
			  (cl-fad:merge-pathnames-as-directory (handle-pathname dhandle) name))))
    (when h (return-from allocate-dhandle h)))

  (let ((handle (make-dhandle dhandle name (next-provider-id provider))))
    (unless (cl-fad:directory-exists-p (handle-pathname handle))
      (return-from allocate-dhandle nil))

    (push handle (simple-provider-handles provider))
    (push (handle-fh handle) (handle-children dhandle))

    handle))

(defun make-simple-provider (&optional local-path)
  "Make a SIMPLE-PROVIDER instance. PATH should be the local filesystem path to export, EXPORT-PATH should 
be a string naming the mount-point that is exported by NFS."
  (let ((handle (make-mount-handle (or local-path *default-pathname-defaults*))))
    (let ((provider (make-instance 'simple-provider
				   :mount-handle handle)))
      (push handle (simple-provider-handles provider))
      provider)))

;; ----------- file operations -------------------


(defun read-file (handle offset count)
  (with-open-file (f (handle-pathname handle) 
		     :direction :input
		     :element-type '(unsigned-byte 8))
    (file-position f offset)
    (let ((buffer (nibbles:make-octet-vector count)))
      (let ((n (read-sequence buffer f)))
	(values (if (= n count)
		    buffer
		    (subseq buffer 0 n))
		(not (read-byte f nil nil)))))))

(defun write-file (handle offset buffer)
  (with-open-file (f (handle-pathname handle)
		     :direction :output
		     :if-does-not-exist :error
		     :element-type '(unsigned-byte 8))
    (file-position f offset)
    (length (write-sequence buffer f))))

(defun create-file (provider dhandle name)
  (let ((handle (make-handle dhandle name (next-provider-id provider))))
    (with-open-file (f (handle-pathname handle)
		       :direction :output 
		       :if-exists :error
		       :if-does-not-exist :create)
      (file-length f)))
  (allocate-handle provider dhandle name))

(defun remove-file (provider dhandle name)
  (let ((handle (make-handle dhandle name (next-provider-id provider))))
    (delete-file (handle-pathname handle))))

(defun create-directory (provider dhandle name)
  (declare (type handle dhandle)
	   (type string name))
  (let ((handle (make-dhandle dhandle name 0)))
    (when handle
      (ensure-directories-exist (handle-pathname handle))
      (allocate-dhandle provider dhandle name))))

(defun remove-directory (provider dhandle name)
  (let ((handle (make-dhandle dhandle name (next-provider-id provider))))
    (cl-fad:delete-directory-and-files (handle-pathname handle))))

;; ---------------------------------------------------------------

;; imple the various methods 

;; for the mount protocol
(defmethod nfs-provider-mount ((provider simple-provider) client)
  (declare (ignore client))
  (handle-fh (simple-provider-mount-handle provider)))

(defmethod nfs-provider-unmount ((provider simple-provider) client)
  (declare (ignore client))
  nil)

;; for nfs
(defmethod nfs-provider-attrs ((provider simple-provider) fh)
  (let ((handle (find-handle provider fh)))
    (if handle
	(let ((info (handle-info handle))) ;;(nefarious.finfo:get-file-information (handle-pathname handle))))
	  (make-fattr3 :type (if (handle-directory-p handle)
				 :dir
				 :reg)
		       :mode (pack-mode :owner (if (handle-directory-p handle)
                                           '(:read :write :execute)
                                           '(:read :write))
					:group '(:read :write)
					:others '(:read))
		       :nlink (nefarious.finfo:file-information-nlinks info)
		       :uid 1000
		       :gid 1000
		       :size (or (nefarious.finfo:file-information-size info) 0)
		       :used (or (nefarious.finfo:file-information-size info) 0)
		       :fileid (unpack-fh (handle-fh handle))
		       :atime (make-nfs-time3 :seconds (or (nefarious.finfo:file-information-atime info) 0))
		       :mtime (make-nfs-time3 :seconds (or (nefarious.finfo:file-information-mtime info) 0))
		       :ctime (make-nfs-time3 :seconds (or (nefarious.finfo:file-information-ctime info) 0))))
	(error 'nfs-error :stat :bad-handle))))

;; seem to need this for Linux to be able to create files
(defmethod (setf nfs-provider-attrs) (value (provider simple-provider) handle)
  (declare (ignore value handle))
  nil)

(defmethod nfs-provider-lookup ((provider simple-provider) dh name)
  (nfs-log :info "dh: ~S name: ~S" dh name)
  (when (string= name ".") (return-from nfs-provider-lookup dh))

  (let ((dhandle (find-handle provider dh)))
    (if dhandle 	
	(cond
	  ((string= name "..") 
	   (let ((ph (handle-parent dhandle)))
	     (if ph 
		 ph
		 (error 'nfs-error :stat :noent))))
	  (t 	   
	   (nfs-log :info "Searching for ~S ~S" 
		      (handle-pathname dhandle) name)
	   (let ((handle (allocate-dhandle provider dhandle name)))
	     (if handle
		 (handle-fh handle)
		 (let ((handle2 (allocate-handle provider dhandle name)))
		   (if handle2 
		       (handle-fh handle2)
		       (error 'nfs-error :stat :noent)))))))
	(error 'nfs-error :stat :noent))))

(defmethod nfs-provider-access ((provider simple-provider) handle access)
  (declare (ignore handle access))
  '(:read :lookup :modify :extend :delete :execute))

(defmethod nfs-provider-read ((provider simple-provider) fh offset count)
  "Read count bytes from offset from the object."
  (nfs-log :info "Read ~A:~A" offset count)
  (let ((handle (find-handle provider fh)))
    (if handle
	(read-file handle offset count)
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-write ((provider simple-provider) fh offset bytes)
  "Write bytes at offset to the object. Returns the number of bytes written."
  (let ((handle (find-handle provider fh)))
    (cond
      (handle
       (multiple-value-prog1 (write-file handle offset bytes)
	 ;; update the cached file information
	 (setf (handle-info handle)
	       (nefarious.finfo:get-file-information (handle-pathname handle)))))
      (t 
       (error 'nfs-error :stat :bad-handle)))))

(defmethod nfs-provider-create ((provider simple-provider) dh name)
  "Create a new file named NAME in directory DHANDLE."
  (let ((dhandle (find-handle provider dh)))
    (if dhandle
	(let ((handle (create-file provider dhandle name)))
	  (if handle
	      (handle-fh handle)
	      (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-remove ((provider simple-provider) dhandle name)
  "Remove the file named HANDLE."
  (let ((handle (find-handle provider dhandle)))
    (cond
      (handle	
       (remove-file provider handle name)
       (remove-handle provider handle))
      (t 
       (error 'nfs-error :stat :bad-handle)))))

(defmethod nfs-provider-rename ((provider simple-provider) fdh fname tdh tname)
  "Rename the file named by the FROM-DHANDLE/FROM-NAME."
  (let ((fdhandle (find-handle provider fdh))
	(tdhandle (find-handle provider tdh)))
    (if (and fdhandle tdhandle)
	(let ((fpath (handle-pathname (make-handle fdhandle fname (next-provider-id provider))))
	      (tpath (handle-pathname (make-handle tdhandle tname (next-provider-id provider)))))
	  (rename-file fpath tpath)
	  nil)
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-read-dir ((provider simple-provider) dh &key cookie verf count &allow-other-keys)
  "Returns a list of all object (file and directory) names in the directory."
  (declare (ignore cookie verf count))
  (let ((dhandle (find-handle provider dh)))
    (unless dhandle (error 'nfs-error :stat :bad-handle))
    (unless (handle-directory-p dhandle) (error 'nfs-error :stat :notdir))

    (let ((files (cl-fad:list-directory (handle-pathname dhandle))))
      (mapcar (lambda (path)
		(if (cl-fad:directory-pathname-p path)
		    (car (last (pathname-directory path)))
		    (format nil "~A~A~A" 
			    (pathname-name path)
			    (if (pathname-type path)
				"."
				"")
			    (if (pathname-type path)
				(pathname-type path)
				""))))
	      files))))

(defmethod nfs-provider-create-dir ((provider simple-provider) dh name)
  "Create a new directory."
  (let ((dhandle (find-handle provider dh)))
    (if dhandle 
	(let ((handle (create-directory provider dhandle name)))
	  (if handle
	      (handle-fh handle)
	      (error 'nfs-error :stat :server-fault)))
	(error 'nfs-error :stat :bad-handle))))


(defmethod nfs-provider-remove-dir ((provider simple-provider) dh name)
  "Remove a directory."
    (let ((dhandle (find-handle provider dh)))
      (if dhandle
	  (progn
	    (remove-directory provider dhandle name)
	    nil)
	  (error 'nfs-error :stat :bad-handle))))

;; filesystem information
(defmethod nfs-provider-fs-info ((provider simple-provider) handle)
  "Returns dynamic filesystem information, in an FS-INFO structure."
  (declare (ignore handle))
  (make-fs-info :attrs nil ;; attributes of the file 
		:rtmax 32768 ;; maximum read request count
		:rtpref 32768 ;; preferred read count -- should be same as rtmax
		:rtmult 512 ;; suggested multiple for read requests
		:wtmax 32768 ;; maximum write request count 
		:wtpref 32786 ;; preferred write count
		:wtmult 512 ;; suggested multiple for writes
		:dtpref 32768 ;; preferred size for read-dir
		:max-fsize #xffffffff ;; maximum file size
		:time-delta (nefarious:make-nfs-time3 :seconds 1)
		:properties (frpc:enum 'nefarious::nfs-info :homogenous)))


(defmethod nfs-provider-fs-stat ((provider simple-provider) handle)
  "Returns static filesystem information, in an FS-STAT structure."
  (declare (ignore handle))
  (make-fs-stat :attrs nil ;; fileattribvutes
		:tbytes #xffffffff ;; total size of the filesystem
		:fbytes #xffffffff ;; free bytes
		:abytes #xffffffff ;; available bytes
		:tfiles #xffffffff ;; total file slots
		:ffiles #xffffffff ;; total number of free file slots
		:afiles #xffffffff ;; available file slots
		:invarsec 1))

(defmethod nfs-provider-path-conf ((provider simple-provider) handle)
  "Returns a PATH-CONF structure containing information about the filesystem."
  (declare (ignore handle))
  (make-path-conf :attr nil ;; file attributes
		  :link-max 0 ;; max link size
		  :link-max 0 ;; maximum number of hard links to an object
		  :name-max 255 ;; maximum file name
		  :no-trunc t ;; if T the server will reject any request with a name longer than 
		  :chown-restricted t ;; will reject any attempt to chown
		  :case-insensitive nil ;; case insensitive filesystem
		  :case-preserving t))
