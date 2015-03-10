;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; need CFFI and CL-PPCRE for this

(defpackage #:nefarious.registry
  (:use #:cl #:nefarious #:cffi)
  (:nicknames #:nfs.registry)
  (:export #:make-registry-provider))

(in-package #:nefarious.registry)

;; -----------------------------------------------


(define-foreign-library advapi
  (:windows "Advapi32.dll"))

(use-foreign-library advapi)

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
	   
		   
;; --------- registry API ------------------

(defctype hkey :uint32)


(defcfun (%reg-open-key "RegOpenKeyExA" :convention :stdcall)
    :uint32
  (key hkey)
  (name :string)
  (options :uint32)
  (desired :uint32)
  (result :pointer))

(defparameter *hkey-trees* 
  '((:classes-root . 2147483648) 
    (:current-user . 2147483649) 
    (:local-machine . 2147483650) 
    (:users . 2147483651) 
    (:current-config . 2147483653)))

(defun resolve-key (key)
  (etypecase key
    (keyword (cdr (assoc key *hkey-trees*)))
    (integer key)))

(defconstant +desire-all-access+ #xf003f)

(defun reg-open-key (name &key key (options 0) (desired +desire-all-access+))
  "Open the registry key named by NAME, which lives under the key named by KEY."
  (with-foreign-object (handle 'hkey)
    (let ((sts (%reg-open-key (resolve-key key)
			      name 
			      options 
			      desired
			      handle)))
      (if (zerop sts)
	  (mem-ref handle 'hkey)
	  (error 'win-error :code sts)))))

(defcfun (reg-close-key "RegCloseKey" :convention :stdcall)
    :long
  (key hkey))


(defmacro with-reg-key ((var name &key key) &body body)
  `(let ((,var (reg-open-key ,name :key ,key)))
     (unwind-protect (progn ,@body)
       (reg-close-key ,var))))

(defcfun (%reg-create-key "RegCreateKeyExA" :convention :stdcall)
    :long
  (key hkey)
  (name :string)
  (reserved :uint32)
  (class :string)
  (options :uint32)
  (desired :uint32)
  (attributes :pointer)
  (result :pointer)
  (disposition :pointer))

(defun reg-create-key (name &key key (options 0) (desired +desire-all-access+))
  "Create a new registry key named NAME underneath the key KEY."
  (with-foreign-object (handle 'hkey)
    (let ((res (%reg-create-key (resolve-key key)
				name 
				0
				(null-pointer)
				options
				desired
				(null-pointer)
				handle
				(null-pointer))))
      (if (= res 0)
	  (mem-ref handle 'hkey)
	  (error 'win-error :code res)))))

(defcfun (%reg-enum-key "RegEnumKeyExA" :convention :stdcall)
    :long
  (key hkey)
  (index :uint32)
  (name-buffer :pointer)
  (name-size :pointer)
  (reserved :pointer)
  (class :pointer)
  (class-size :pointer)
  (last-write :pointer))

(defun reg-enum-key (key &optional tree)
  "Return a list of all subkeys of the key named by KEY. KEY can be a key handle, as returned by REG-OPEN-KEY, or a string naming a key, with TREE a keyword naming a top-level registry tree."
  (if (stringp key)
      (with-reg-key (k key :key tree)
	(reg-enum-key k))
      (with-foreign-object (buffer :char 1024)
	(with-foreign-object (size :uint32)
	  (do ((i 0 (1+ i))
	       (names nil)
	       (done nil))
	      (done names)
	    (setf (mem-ref size :uint32) 1024)
	    (let ((res (%reg-enum-key (resolve-key key)
				      i
				      buffer
				      size
				      (null-pointer)
				      (null-pointer)
				      (null-pointer)
				      (null-pointer))))
	      (if (= res 0)
		  (push (foreign-string-to-lisp buffer :count (mem-ref size :uint32))
			names)
		  (setf done t))))))))

				  

(defcfun (%reg-enum-value "RegEnumValueA" :convention :stdcall)
    :long
  (key hkey)
  (index :uint32)
  (name :pointer)
  (size :pointer)
  (reserved :pointer)
  (type :pointer)
  (data :pointer)
  (data-size :pointer))

(defparameter *reg-types*
  '((:string 1)
    (:expand-string 2)
    (:binary 3)
    (:dword 4)
    (:multi-string 7)))

(defun reg-enum-value (key &optional tree)
  "List all the values of the registry key."
  (if (stringp key)
      (with-reg-key (k key :key tree)
	(reg-enum-value k))
      (with-foreign-objects ((name-buffer :char 1024)
			     (size :uint32)
			     (data :char 1024)
			     (data-size :uint32)
			     (type :uint32))
	(do ((i 0 (1+ i))
	     (vals nil)
	     (done nil))
	    (done vals)
	  (setf (mem-ref size :uint32) 1024
		(mem-ref data-size :uint32) 1024)
	  (let ((res (%reg-enum-value (resolve-key key)
				      i
				      name-buffer
				      size
				      (null-pointer)
				      type
				      data
				      data-size)))
	    (if (= res 0)
		(push 
		 (let ((vec (make-array (mem-ref data-size :uint32)
					:element-type '(unsigned-byte 8))))
		   (dotimes (i (mem-ref data-size :uint32))
		     (setf (aref vec i) (mem-ref data :uint8 i)))
		   (list (foreign-string-to-lisp name-buffer 
						 :count (mem-ref size :uint32))
			 vec 
			 (first 
			  (find (mem-ref type :uint32)
				*reg-types*
				:key #'second))))
		 vals)
		(setf done t)))))))

(defcfun (%reg-set-value "RegSetValueExA" :convention :stdcall)
    :long
  (key hkey)
  (name :pointer)
  (reserved :uint32)
  (type :uint32)
  (data :pointer)
  (size :uint32))

(defun reg-set-value (key name data type)
  "Set the registry value. Data should be an octet vector, type should be a keyword."
  (declare (type vector data)
	   (type symbol type)
	   (type string name))
  (let ((length (length data)))
    (with-foreign-object (buffer :uint8 length)
      (with-foreign-string (nstr name)
	(dotimes (i length)
	  (setf (mem-ref buffer :uint8 i)
		(aref data i)))
	(let ((res (%reg-set-value (resolve-key key)
				   nstr
				   0
				   (second 
				    (find type *reg-types*
					  :key #'first))
				   buffer
				   length)))
	  (if (= res 0)
	      nil
	      (error 'win-error :code res)))))))

(defcfun (%reg-delete-value "RegDeleteValueA" :convention :stdcall)
   :long
  (key hkey)
  (name :string))

(defun reg-delete-value (key name)
  "Delete a registry value."
  (with-foreign-string (nstr name)
    (let ((res (%reg-delete-value key nstr)))
      (if (= res 0)
	  nil
	  (error 'win-error :code res)))))

(defcfun (%reg-set-key-value "RegSetKeyValueA" :convention :stdcall)
    :long
  (key hkey)
  (subkey :string)
  (name :string)
  (type :uint32)
  (data :pointer)
  (length :uint32))

(defun reg-set-key-value (key name data &key subkey (type :string))
  "Set a registry value underneath subkey SUBKEY."
  (let ((length (length data)))
    (with-foreign-object (buffer :uint8 length)
      (with-foreign-strings ((nstr name)
			     (skstr (or subkey "")))
	(let ((res (%reg-set-key-value (resolve-key key)
				       (if subkey skstr (null-pointer))
				       nstr
				       (second (find type *reg-types* :key #'first))
				       buffer
				       length)))
	  (if (= res 0)
	      nil
	      (error 'win-error :code res)))))))


(defcfun (%reg-delete-tree "RegDeleteTreeA" :convention :stdcall)
    :long
  (key hkey)
  (subkey :string))

(defun reg-delete-tree (key &optional subkey)
  "Delete a registry key and all its subkeys/values."
  (with-foreign-string (skstr (or subkey ""))
    (let ((res (%reg-delete-tree (resolve-key key)
				 (if subkey skstr (null-pointer)))))
      (if (= res 0)
	  nil
	  (error 'win-error :code res)))))

(defcfun (%reg-get-value "RegGetValueA" :convention :stdcall)
    :long
  (key hkey)
  (subkey :string)
  (name :string)
  (flags :uint32)
  (type :pointer)
  (data :pointer)
  (size :pointer))

(defun reg-get-value (key name &optional subkey)
  "Get a registry value."
  (if (and subkey (stringp subkey))
      (with-reg-key (k subkey :key key)
	(reg-get-value k name))
      (with-foreign-strings ((nstr name)
			     (skstr (or subkey "")))
	(with-foreign-objects ((buffer :uint8 1024)
			       (sbuff :uint32)
			       (tbuff :uint32))
	  (setf (mem-ref sbuff :uint32) 1024)
	  (let ((res 
		 (%reg-get-value (resolve-key key)
				 (if subkey skstr (null-pointer))
				 nstr
				 #xffff ;; any data type
				 tbuff
				 buffer
				 sbuff)))
	    (if (= res 0)
		(values 
		 (let ((v (make-array (mem-ref sbuff :uint32) :element-type '(unsigned-byte 8))))
		   (dotimes (i (mem-ref sbuff :uint32))
		     (setf (aref v i) (mem-ref buffer :uint8 i)))
		   v)
		 (first (find (mem-ref tbuff :uint32) *reg-types* :key #'second)))
		(error 'win-error :code res)))))))

;; -----------------------------------------------------


		      
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
  fh
  parent) 

(defun make-rhandle (&key rhandle tree key name)
  (let ((handle 
	 (%make-rhandle :tree (or tree (rhandle-tree rhandle))
			:key (when (or (and rhandle (rhandle-key rhandle)) key)
			       (concatenate 'string 
					    (when rhandle (rhandle-key rhandle))
					    (when (and rhandle (rhandle-key rhandle) key) "\\")
					    key))
			:name name
			:parent (when rhandle (rhandle-fh rhandle)))))
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

(defun make-registry-provider (tree &optional key)
  (let ((mhandle (make-rhandle :tree tree :key key)))
    (let ((provider 
	   (make-instance 'registry-provider
			  :mount-handle mhandle)))
      (allocate-rhandle provider mhandle)
      provider)))

;; ------------------------------------------
;; for the mount protocol
(defmethod nfs-provider-mount ((provider registry-provider) client)
  (rhandle-fh (simple-provider-mount-handle provider)))

(defmethod nfs-provider-unmount ((provider registry-provider) client)
  nil)

;; for nfs
(defmethod nfs-provider-attrs ((provider registry-provider) fh)
  (let ((rhandle (find-rhandle provider fh)))
    (if rhandle
	(let* ((name (rhandle-name rhandle))
	       (size (when name 
		       (length 
			(reg-get-value (rhandle-tree rhandle)
				       (rhandle-name rhandle)
				       (rhandle-key rhandle))))))
	  (make-fattr3 :type (if name :reg :dir)
		       :mode 0
		       :uid 0
		       :gid 0
		       :size (if name size 0)
		       :used (if name size 0)
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
	    ((string= name ".") dh)
	    ((string= name "..") 
	     (let ((ph (rhandle-parent handle)))
	       (if ph
		   ph
		   (error 'nfs-error :stat :noent))))
	    ((rhandle-exists-p handle)
	     (allocate-rhandle provider dhandle :key name)
	     (rhandle-fh handle))
	    (t 
	     (let ((handle (make-rhandle :rhandle dhandle
					 :name name)))
	       (log:debug "key: ~A name: ~A" (rhandle-key dhandle) name)
	       (cond
		 ((string= name ".")
		  (rhandle-fh dhandle))
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
	    (let ((bytes 
		   (reg-get-value (rhandle-tree handle)
				  (rhandle-name handle)
				  (rhandle-key handle))))
	      (subseq bytes offset))
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
	     '("." "..")
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


