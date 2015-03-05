;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; need CL-REGISTRY for this

(defpackage #:nefarious.registry
  (:use #:cl #:nefarious #:cl-registry)
  (:nicknames #:nfs.registry)
  (:export #:make-registry-provider))

(in-package #:nefarious.registry)
		      
;; the provider class
(defclass registry-provider (simple-provider)
  ())

;; basic idea:
;; registry keys correspond to NFS directories.
;; registry values correspond to NFS files.


(defstruct (rhandle (:constructor %make-rhandle))
  tree ;; keyword from *hkey-trees*, :local-machine, :current-user, etc
  key ;; a string naming the full path to the key
  name ;; name of value (if any)
  fh) 

(defun make-rhandle (&key rhandle tree key name)
  (let ((handle 
	 (%make-rhandle :tree (or tree (rhandle-tree rhandle))
			:key (when rhandle
			       (concatenate 'string 
					    (rhandle-key rhandle)
					    (when (and (rhandle-key rhandle) key) "\\")
					    key))
			:name name)))
    (setf (rhandle-fh handle)
	  (frpc:pack #'frpc::write-uint64 
		     (sxhash (concatenate 'string
					  (symbol-name (rhandle-tree handle))
					  (rhandle-key handle)
					  (rhandle-name handle)))))
    handle))

(defun parse-keypath (path)
  "match the path agaisnt the regex <tree>\\<key>[:<value>]"
  (multiple-value-bind (matched matches) 
      (cl-ppcre:scan-to-strings "(HKLM|HKCU|HKCR|HKCC|HKUSERS)\\\\((\\w+\\\\?)*):?(\\w+)?" path)
    (when matched
      (let ((tree (aref matches 0))
	    (key (aref matches 1))
	    (name (aref matches 3)))
	(make-rhandle :tree (cond
			      ((string= tree "HKLM") :local-machine)
			      ((string= tree "HKCU") :current-user)
			      ((string= tree "HKCR") :classes-root)
			      ((string= tree "HKCC") :current-config)
			      ((string= tree "HKUSERS") :users)
			      (t (error "Invalid registry hive")))
		      :key (when (and key (not (string= key "")))
			     key)
		      :name name)))))

(defun rhandle-exists-p (rhandle)
  "Check whether the registry key/value specified by the handle actually exists."
  (handler-case 
      (progn
	(if (rhandle-name rhandle)
	    (reg-get-value (rhandle-tree rhandle) 
			   (rhandle-name rhandle)
			   (rhandle-key rhandle))
	    (with-reg-key (k (rhandle-key rhandle) :key (rhandle-tree rhandle))))
	t)
    (error ()
      nil)))

(defun allocate-rhandle (provider rhandle &key key name)
  (let ((handle (make-rhandle :rhandle rhandle
			      :key key 
			      :name name)))
    (setf (gethash (rhandle-fh handle)
		   (simple-provider-handles provider))
	  handle)
    handle))

(defun find-rhandle (provider h)
  (gethash h (simple-provider-handles provider)))

(defun make-registry-provider (path)
  (let ((mhandle (parse-keypath path)))
    (let ((provider 
	   (make-instance 'registry-provider
			  :mount-handle mhandle)))
      (allocate-rhandle provider mhandle)
      provider)))

;; ------------------------------------------
;; for the mount protocol
(defmethod nfs-provider-mount ((provider registry-provider) client)
  (pushnew client (simple-provider-clients provider) 
	   :test #'equalp)
  (rhandle-fh (simple-provider-mount-handle provider)))

(defmethod nfs-provider-unmount ((provider registry-provider) client)
  (setf (simple-provider-clients provider)
	(remove client (simple-provider-clients provider)
		:test #'equalp))
  nil)

;; for nfs
(defmethod nfs-provider-attrs ((provider registry-provider) fh)
  (let ((rhandle (find-rhandle provider fh)))
    (if rhandle
	(let ((name (rhandle-name rhandle)))
	  (make-fattr3 :type (if name :reg :dir)
		       :mode 0
		       :uid 0
		       :gid 0
		       :size (if name 1024 0)
		       :used (if name 1024 0)
		       :fileid 0
		       :atime (make-nfs-time3)
		       :mtime (make-nfs-time3)
		       :ctime (make-nfs-time3)))
	(error 'nfs-error :stat :bad-handle))))

;; don't support this
;;(defmethod (setf nfs-provider-attrs) (value (provider simple-provider) handle)
;;  nil)

(defmethod nfs-provider-lookup ((provider registry-provider) dh name)
  (let ((dhandle (find-rhandle provider dh)))
    (if dhandle 
	(let ((handle (make-rhandle :rhandle dhandle
				    :key name)))
	  (cond
	    ((rhandle-exists-p handle)
	     (allocate-rhandle provider dhandle :key name)
	     (rhandle-fh handle))
	    (t 
	     (let ((handle (make-rhandle :rhandle dhandle
					 :name name)))
	       (cond
		 ((rhandle-exists-p handle)
		  (allocate-rhandle provider dhandle :name name)
		  (rhandle-fh handle))
		 (t 
		  (error 'nfs-error :stat :noent)))))))
	(error 'nfs-error :stat :noent))))

(defmethod nfs-provider-read ((provider registry-provider) fh offset count)
  "Read count bytes from offset from the object."
  (let ((handle (find-rhandle provider fh)))
    (if (and handle (rhandle-name handle))
	(handler-case 
	    (reg-get-value (rhandle-tree handle)
			   (rhandle-name handle)
			   (rhandle-key handle))
	  (error (e)
	    (log:debug "~A" e)
	    (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-write ((provider registry-provider) fh offset bytes)
  "Write bytes at offset to the object. Returns the number of bytes written."
  (let ((handle (find-rhandle provider fh)))
    (if (and handle (rhandle-name handle))
	(handler-case 
	    (reg-set-key-value (rhandle-tree handle)
			       (rhandle-name handle)
			       bytes
			       :subkey (rhandle-key handle)
			       :type :binary)
	  (error (e)
	    (log:debug "~A" e)
	    (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-create ((provider registry-provider) dh name)
  "Create a new file named NAME in directory DHANDLE."
  (let ((dhandle (find-rhandle provider dh)))
    (if dhandle
	(let ((handle (allocate-rhandle provider
					dhandle
					:name name)))
	  (handler-case 
	      (reg-set-key-value (rhandle-tree handle)
				 name
				 #()
				 :subkey (rhandle-key handle)
				 :type :binary)
	    (error (e)
	      (log:debug "~A" e)
	      (error 'nfs-error :noent))))
	(error 'nfs-error :bad-handle))))

(defmethod nfs-provider-remove ((provider registry-provider) dh name)
  "Remove the file named HANDLE."
  (let ((dhandle (find-rhandle provider dh)))
    (if dhandle
	(handler-case 
	    (with-reg-key (k (rhandle-key dhandle) :key (rhandle-tree dhandle))
	      (reg-delete-value k name))
	  (error (e)
	    (log:debug "~A" e)
	    (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-rename ((provider registry-provider) fdh fname tdh tname)
  "Rename the file named by the FROM-DHANDLE/FROM-NAME."
  (error 'nfs-error :stat :server-fault))

(defmethod nfs-provider-read-dir ((provider registry-provider) dh)
  "Returns a list of all object (file and directory) names in the directory."
  (let ((dhandle (find-rhandle provider dh)))
    (if dhandle
	(handler-case 
	    (append 
	     (reg-enum-key (or (rhandle-key dhandle) "")
			   (rhandle-tree dhandle))
	     (mapcar #'car (reg-enum-value (or (rhandle-key dhandle) "")
					   (rhandle-tree dhandle))))			   
	  (error (e)
	    (log:debug "~A" e)
	    (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-create-dir ((provider registry-provider) dh name)
  "Create a new directory."
  (let ((dhandle (find-rhandle provider dh)))
    (if dhandle 
	(handler-case 
	    (let ((handle (allocate-rhandle provider dhandle :name name)))
	      (reg-set-key-value (rhandle-tree handle)
				 name
				 #()
				 :subkey (rhandle-key handle)
				 :type :binary)
	      (rhandle-fh handle))
	  (error (e)
	    (log:debug "~A" e)
	    (error 'nfs-error :stat :noent)))
	(error 'nfs-error :stat :bad-handle))))


(defmethod nfs-provider-remove-dir ((provider registry-provider) dh name)
  "Remove a directory."
    (let ((dhandle (find-rhandle provider dh)))
      (if dhandle
	  (handler-case 
	      (reg-delete-tree (rhandle-tree dhandle)
			       (rhandle-key dhandle))
	    (error (e)
	      (log:debug "~A" e)
	      (error 'nfs-error :stat :server-fault)))
	  (error 'nfs-error :stat :bad-handle))))


;; filesystem information
(defmethod nfs-provider-fs-info ((provider registry-provider))
  "Returns dynamic filesystem information, in an FS-INFO structure."
  (make-fs-info :attrs nil ;; attributes of the file 
		:rtmax 1024 ;; maximum read request count
		:rtpref 1024 ;; preferred read count -- should be same as rtmax
		:rtmult 4 ;; suggested multiple for read requests
		:wtmax 1024 ;; maximum write request count 
		:wtpref 1024 ;; preferred write count
		:wtmult 4 ;; suggested multiple for writes
		:dtpref #xffffffff ;; preferred size for read-dir
		:max-fsize 1024 ;; maximum file size
		:time-delta (make-nfs-time3 :seconds 1)
		:properties (frpc:enum 'nefarious::nfs-info :homogenous)))


(defmethod nfs-provider-fs-stat ((provider registry-provider))
  "Returns static filesystem information, in an FS-STAT structure."
  (make-fs-stat :attrs nil ;; fileattribvutes
		:tbytes #xffffffff ;; total size of the filesystem
		:fbytes #xffffffff ;; free bytes
		:abytes #xffffffff ;; available bytes
		:tfiles #xffffffff ;; total file slots
		:ffiles #xffffffff ;; total number of free file slots
		:afiles #xffffffff ;; available file slots
		:invarsec 1))

(defmethod nfs-provider-path-conf ((provider registry-provider))
  "Returns a PATH-CONF structure containing information about the filesystem."
  (make-path-conf :attr nil ;; file attributes
		  :link-max 0 ;; max link size
		  :link-max 0 ;; maximum number of hard links to an object
		  :name-max 255 ;; maximum file name
		  :no-trunc t ;; if T the server will reject any request with a name longer than 
		  :chown-restricted t ;; will reject any attempt to chown
		  :case-insensitive t ;; case insensitive filesystem
		  :case-preserving t))


