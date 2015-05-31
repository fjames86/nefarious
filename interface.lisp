;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(defun client-mounted-p (provider client)
  (member client (provider-clients provider)
	  :test #'equalp))

(defun maybe-provider-attrs (provider handle)
  (ignore-errors (nfs-provider-attrs provider handle)))

;;(use-rpc-program +nfs-program+ +nfs-version+)
(defprogram nfs 100003)
(use-rpc-host '*rpc-host* 2049)

;; ------------------------------------------------------
;; void NFSPROC3_NULL(void)                    = 0;

(defun %handle-null (void)
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :void
  (:program nfs 3)
  (:documentation "Test connectivity to the server.")
  (:handler #'%handle-null))


;; ------------------------------------------------------
;; GETATTR3res NFSPROC3_GETATTR(GETATTR3args)         = 1;
 
(defun %handle-get-attrs (fh)
  (destructuring-bind (provider handle) (fh-provider-handle fh)
    (cond
      ((and provider (client-mounted-p provider *rpc-remote-host*))
       (handler-case (make-xunion :ok (nfs-provider-attrs provider handle))
         (nfs-error (e)
           (make-xunion (nfs-error-stat e) nil))
         (error (e)
           (nfs-log :error "~A" e)
           (make-xunion :server-fault nil))))
      (provider 
       ;; client not mounted
       (make-xunion :access nil))
      (t 
       (make-xunion :bad-handle nil)))))

(defrpc call-get-attrs 1 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fattr3)
    (otherwise :void))
  (:program nfs 3)
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Get file attributes.")
  (:handler #'%handle-get-attrs))

;; ------------------------------------------------------
;; SETATTR3res NFSPROC3_SETATTR(SETATTR3args)         = 2;

(defun %handle-set-attrs (args)
  (destructuring-bind (fh new-attrs guard) args
    (declare (ignore guard))
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond 
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (let ((attrs (maybe-provider-attrs provider handle)))
	   (handler-case 
	       (progn
		 (setf (nfs-provider-attrs provider handle) new-attrs)
		 (make-xunion :ok (make-wcc-data :attrs (maybe-provider-attrs provider handle)
						 :old-attrs attrs)))
	     (nfs-error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion (nfs-error-stat e)
			    (make-wcc-data :attrs (maybe-provider-attrs provider handle)
					   :old-attrs attrs)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault
			    (make-wcc-data :attrs (maybe-provider-attrs provider handle)
					   :old-attrs attrs))))))
	(provider
	 ;; client not mounted 
	 (make-xunion :access (make-wcc-data)))
	(t 
	 (make-xunion :bad-handle (make-wcc-data)))))))

(defrpc call-set-attrs 2 
  (:list nfs-fh3 sattr3 (:optional nfs-time3))
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (handle attr &key time) (list handle attr time))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Set attributes.")
  (:handler #'%handle-set-attrs))

;; ------------------------------------------------------
;; LOOKUP3res NFSPROC3_LOOKUP(LOOKUP3args)           = 3;

(defun %handle-lookup (arg)
  (with-slots (dir name) arg
    (destructuring-bind (provider dh) (fh-provider-handle dir)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (handler-case 	      
	     (let ((handle (nfs-provider-lookup provider dh name)))
	       (if handle 
		   (make-xunion :ok (list (provider-handle-fh provider handle)
					  (nfs-provider-attrs provider handle)
					  (nfs-provider-attrs provider dh)))
		   (make-xunion :noent nil)))
	   (nfs-error (e)
	     (nfs-log :error "~A" e)
	     (make-xunion (nfs-error-stat e) nil))
	   (error (e)
	     (nfs-log :error "~A" e)
	     (make-xunion :server-fault nil))))
	(provider 
	 (make-xunion :access nil))
	(t 
	 (make-xunion :bad-handle nil))))))

;; lookup -- lookup filename
(defrpc call-lookup 3
  dir-op-args3 
  (:union nfs-stat3
    (:ok (:list nfs-fh3 post-op-attr post-op-attr))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (dhandle filename) 
    (make-dir-op-args3 :dir dhandle :name filename))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr1 attr2) (xunion-val res)
	  (values fh attr1 attr2))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Get the file handle and attributes. Returns (values file-handle attributes directory-attributes)")
  (:handler #'%handle-lookup))

;; ------------------------------------------------------
;; access -- check access permission

;; ACCESS3res NFSPROC3_ACCESS(ACCESS3args)           = 4;

(defxenum nfs-access
 (:read #x0001)
 (:lookup #x0002)
 (:modify #x0004)
 (:extend #x0008)
 (:delete #x0010)
 (:execute #x0020))

(defun pack-nfs-access (access)
  (do ((i 0)
       (alist access (cdr alist)))
      ((null alist) i)
    (setf i (logior i (enum 'nfs-access (car alist))))))

(defun unpack-nfs-access (access)
  (declare (type fixnum access))
  (mapcan (lambda (sym)
	    (unless (zerop (logand access (enum 'nfs-access sym)))
	      (list sym)))
	  '(:read :lookup :modify :extend :delete :execute)))

(defun %handle-access (args)
  (destructuring-bind (fh access) args
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
     (handler-case 
         (make-xunion :ok
                      (list (maybe-provider-attrs provider handle)
                            (pack-nfs-access (nfs-provider-access provider handle access))))
       (nfs-error (e)
         (make-xunion (nfs-error-stat e)
                      (maybe-provider-attrs provider handle)))
       (error (e)
         (nfs-log :error "~A" e)
         (make-xunion :server-fault nil))))
    (provider 
	 (make-xunion :access nil))
	(t
	 (make-xunion :bad-handle nil))))))

(defrpc call-access 4 
  (:list nfs-fh3 :uint32)
  (:union nfs-stat3 
    (:ok (:list post-op-attr :uint32))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle access)
    (list handle
	  (etypecase access
        (integer access)
        (list (pack-nfs-access access)))))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr access) (xunion-val res)
	  (values (unpack-nfs-access access) attr))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "ACCESS can be either an integer which is a bitwise OR of NFS-ACCESS flags, or a list 
of NFS-ACCESS flag symbols. Returns (values post-op-attr access")
  (:handler #'%handle-access))


;; ------------------------------------------------------
;; readlink -- read from symbolic link
;; READLINK3res NFSPROC3_READLINK(READLINK3args)       = 5;

(defun %handle-readlink (fh)
  (destructuring-bind (provider handle) (fh-provider-handle fh)
    (cond
      ((and provider (client-mounted-p provider *rpc-remote-host*))
       (handler-case 
	   (let ((path (nfs-provider-read-link provider handle)))
	     (make-xunion :ok
			  (list (nfs-provider-attrs provider handle)
				path)))
	 (nfs-error (e)
	   (make-xunion (nfs-error-stat e) nil))
	 (error (e)
	   (nfs-log :error "~A" e)
	   (make-xunion :server-fault nil))))
      (provider 
       (make-xunion :access nil))
      (t 
       (make-xunion :bad-handle nil)))))

(defrpc call-readlink 5 
  nfs-fh3
  (:union nfs-stat3
    (:ok (:list post-op-attr nfs-path3))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr path) (xunion-val res)
	  (values attr path))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Read the contents of a symbolic link. Returns (values attrs path).")
  (:handler #'%handle-readlink))

;; ------------------------------------------------------
;; read -- read from file
;; READ3res NFSPROC3_READ(READ3args)               = 6;

(defun %handle-read (args)
  (destructuring-bind (fh offset count) args
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
;;     (nfs-log :info "READ ~A@~A" count offset)
	 (handler-case 
	     (multiple-value-bind (bytes eof) (nfs-provider-read provider handle offset count)
	       (make-xunion :ok
			    (list (nfs-provider-attrs provider handle)
				  (length bytes)
				  eof
				  bytes)))
	   (nfs-error (e)
	     (nfs-log :error "~A" e)
	     (make-xunion (nfs-error-stat e) 
			  (nfs-provider-attrs provider handle)))
	   (error (e)
	     (nfs-log :error "~A" e)
	     (make-xunion :server-fault
			  (nfs-provider-attrs provider handle)))))
	(provider 
     (nfs-log :error "access")
	 (make-xunion :access nil))
	(t
     (nfs-log :error "bad handle")
	 (make-xunion :bad-handle nil))))))

(defrpc call-read 6 
  (:list nfs-fh3 offset3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr
		count3
		:boolean
		(:varray* :octet)))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle offset count)
    (list handle offset count))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr count eof data) (xunion-val res)
;;	  (declare (ignore count))
	  (unless (= count (length data))
	    (error "Bytes returned ~A does not equal claimed count ~A" 
		   (length data) count))
	  (values data eof attr))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Read COUNT bytes from OFFSET of file. Returns (values bytes eof-p attrs). EOF-P will be T if 
the end of the file was reached.")
  (:handler #'%handle-read))


;; ------------------------------------------------------
;; write -- write to file
;; WRITE3res NFSPROC3_WRITE(WRITE3args)             = 7;

(defxenum stable-how 
 (:unstable 0)
 (:data-sync 1)
 (:file-sync 2))

;; FIXME: we should do something with the STABLE parameter. 
(defun %handle-write (args)
  (destructuring-bind (fh offset count stable data) args
    (declare (ignore stable count))
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (let ((attrs (maybe-provider-attrs provider handle)))
	   (handler-case 
	       (let ((n (nfs-provider-write provider handle offset data)))
		 (make-xunion :ok 
			      (list (make-wcc-data :attrs (maybe-provider-attrs provider handle)
						   :old-attrs attrs)
				    n
				    :file-sync
				    (make-write-verf3))))
	     (nfs-error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion (nfs-error-stat e) 
			    (make-wcc-data :attrs (maybe-provider-attrs provider handle)
					   :old-attrs attrs)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault 
			    (make-wcc-data :attrs (maybe-provider-attrs provider handle)
					   :old-attrs attrs))))))
	(provider 
	 (make-xunion :access (make-wcc-data)))
	(t 
	 (make-xunion :bad-handle (make-wcc-data)))))))

;; define a new type so that we can write a subset of the array without extra consing 
(defxtype octet-array-start-end ()
  ((stream)
   ;; when reading, just cons a new vector 
   (frpc::read-octet-array stream)) 
  ((stream val)
   ;; when writing, if a list of 3 values is passed in then write a subset of the array 
   (etypecase val
     (vector (frpc::write-octet-array stream val))
     (list 
      (destructuring-bind (val start end) val
	(declare (type vector val)
		 (type integer start)
		 (type (or null integer) end))
	(frpc::write-octet-array stream val :start (or start 0) :end end))))))

(defrpc call-write 7 
  (:list nfs-fh3 offset3 count3 stable-how octet-array-start-end) ;;(:varray* :octet)
  (:union nfs-stat3
    (:ok (:list wcc-data count3 stable-how write-verf3))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (handle offset data &key (stable :file-sync) (start 0) end)
    (list handle 
	  offset 
	  (- (or end (length data))
	     start)
	  stable 
	  (list data start end)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (wcc count stable wverf) (xunion-val res)
	  (values count wverf wcc stable))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Write bytes at offset of file. Returns (values count write-verf wcc stable).")
  (:handler #'%handle-write))


;; ------------------------------------------------------
;; create -- create a file
;; CREATE3res NFSPROC3_CREATE(CREATE3args)           = 8;

(defxenum create-mode3 
  (:unchecked 0)
  (:guarded 1)
  (:exclusive 2))

(defun %handle-create (args)
  (destructuring-bind (dirop how) args
    (declare (ignore how))
    (with-slots (dir name) dirop
      (destructuring-bind (provider dh) (fh-provider-handle dir)
	(cond
	  ((and provider (client-mounted-p provider *rpc-remote-host*))
	   (let ((attrs (maybe-provider-attrs provider dh)))
	     (handler-case 
		 (let ((handle (nfs-provider-create provider dh name)))
		   (make-xunion :ok 
				(list (provider-handle-fh provider handle)
				      (nfs-provider-attrs provider handle)
				      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
						     :old-attrs attrs))))
	       (simple-error (e)
		 (nfs-log :info "~A" e)
		 (make-xunion (nfs-error-stat e) 
			      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					     :old-attrs attrs)))
	       (error (e)
		 (nfs-log :info "~A" e)
		 (make-xunion :server-fault 
			      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					     :old-attrs attrs))))))
	  (provider 
	   (make-xunion :access (make-wcc-data)))
	  (t 
	   (make-xunion :bad-handle (make-wcc-data))))))))

(defrpc call-create 8 
  (:list dir-op-args3 
	 (:union create-mode3 
	   ((:unchecked :guarded) sattr3)
	   (:exclusive create-verf3)))
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle filename &key (mode :unchecked) sattr create-verf)
    (list (make-dir-op-args3 :dir dhandle :name filename)
	  (make-xunion mode 
		       (ecase mode
			 ((:unchecked :guarded) 
			  (or sattr (make-sattr3)))
			 (:exclusive 
			  (or create-verf (make-create-verf3)))))))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Create a new file named NAME in directory named by DHANDLE. Returns (values fh attr wcc).")
  (:handler #'%handle-create))


;; ------------------------------------------------------
;; mkdir -- create a directory 
;; MKDIR3res NFSPROC3_MKDIR(MKDIR3args)             = 9;

(defun %handle-mkdir (args)
  (destructuring-bind (dir-op attrs) args
    (declare (ignore attrs))
    (with-slots (dir name) dir-op
      (destructuring-bind (provider dh) (fh-provider-handle dir)
	(cond
	  ((and provider (client-mounted-p provider *rpc-remote-host*))
	   (let ((attrs (maybe-provider-attrs provider dh)))
	     (handler-case 
		 (let ((handle (nfs-provider-create-dir provider dh name)))
		   (make-xunion :ok 
				(list (provider-handle-fh provider handle)
				      (nfs-provider-attrs provider handle)
				      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
						     :old-attrs attrs))))
	       (nfs-error (e)
		 (nfs-log :info "~A" e)
		 (make-xunion (nfs-error-stat e) 
			      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					     :old-attrs attrs)))
	       (error (e)
		 (nfs-log :info "~A" e)
		 (make-xunion :server-fault (make-wcc-data :attrs (maybe-provider-attrs provider dh)
							   :old-attrs attrs))))))
	  (provider 
	   (make-xunion :access (make-wcc-data)))
	  (t 
	   (make-xunion :bad-handle (make-wcc-data))))))))

(defrpc call-mkdir 9 
  (:list dir-op-args3 sattr3) 
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle name &key sattr)
    (list (make-dir-op-args3 :dir dhandle :name name)
	  (or sattr (make-sattr3))))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Create a new directory named NAME in directory named by DHANDLE. Returns (values fh attr wcc).")
  (:handler #'%handle-mkdir))


;; ------------------------------------------------------
;; symlink -- create a symbolic link
;; SYMLINK3res NFSPROC3_SYMLINK(SYMLINK3args)         = 10;

(defun %handle-create-symlink (args)
  (destructuring-bind (dirop attrs path) args
    (with-slots (dir name) dirop
      (destructuring-bind (provider handle) (fh-provider-handle dir)
	(cond
	  ((and provider (client-mounted-p provider *rpc-remote-host*))
	   (handler-case 
	       (let ((lh (nfs-provider-create-symlink provider 
						      handle 
						      name 
						      path 
						      attrs)))
		 (make-xunion :ok 
			      (list lh 
				    (nfs-provider-attrs provider lh)
				    (make-wcc-data))))
	     (nfs-error (e)
	       (make-xunion (nfs-error-stat e)
			    (make-wcc-data)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault (make-wcc-data)))))
	  (provider 
	   (make-xunion :access (make-wcc-data)))
	  (t 
	   (make-xunion :bad-handle (make-wcc-data))))))))

(defrpc call-create-symlink 10 
  (:list dir-op-args3 sattr3 nfs-path3)
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle filename path &key attrs)
    (list (make-dir-op-args3 :dir dhandle 
			     :name filename)
	  attrs
	  path))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Create a symbolic link to a file. DHANDLE and FILENAME name the symlink to create. PATH should contain 
the data to put into the symlink. ATTRS are the initial attributes of the newly created symlink. Returns (values handle attr wcc).")
  (:handler #'%handle-create-symlink))


;; ------------------------------------------------------
;; mknod -- create special device
;; MKNOD3res NFSPROC3_MKNOD(MKNOD3args)             = 11;

(defun %handle-mknod (args)
  (destructuring-bind (dirop spec) args
    (with-slots (dir name) dirop
      (destructuring-bind (provider dh) (fh-provider-handle dir)
	(nfs-log :info "Creating device ~A ~S" name spec)
	(cond
	  ((and provider (client-mounted-p provider *rpc-remote-host*))
	   (handler-case 
	       (let ((fh (nfs-provider-create-device provider 
						     (xunion-tag spec)
						     dh 
						     name
						     :attrs (case (xunion-tag spec)
							      ((:chr :blk) (first (xunion-val spec)))
							      ((:sock :fifo) (xunion-val spec)))
						     :specdata (case (xunion-tag spec)
								 ((:chr :blk) (second (xunion-val spec)))))))
		 (make-xunion :ok 
			      (list fh 
				    (nfs-provider-attrs provider fh)
				    (make-wcc-data))))
	     (nfs-error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion (nfs-error-stat e) (make-wcc-data)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault (make-wcc-data)))))
	  (provider 
	   (make-xunion :access (make-wcc-data)))
	  (t 
	   (make-xunion :bad-handle (make-wcc-data))))))))

(defrpc call-mknod 11 
  (:list dir-op-args3 
	 (:union ftype3
	   ((:chr :blk) (:list sattr3 specdata3))
	   ((:sock :fifo) sattr3)
	   (otherwise :void)))
  (:union nfs-stat3 
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle filename &key sattr specdata (ftype :reg))
    (list (make-dir-op-args3 :dir dhandle :name filename)
	  (make-xunion ftype
		       (case ftype
			 ((:chr :blk) (list sattr specdata))
			 ((:sock :fifo) sattr)))))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Create special device.")
  (:handler #'%handle-mknod))


;; ------------------------------------------------------
;; REMOVE3res NFSPROC3_REMOVE(REMOVE3args)           = 12;

(defun %handle-remove (args)
  (with-slots (dir name) args
    (destructuring-bind (provider dh) (fh-provider-handle dir)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (let ((attrs (maybe-provider-attrs provider dh)))
	   (handler-case 
	       (progn 
		 (nfs-provider-remove provider dh name)
		 (make-xunion :ok 
			      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					     :old-attrs attrs)))
	     (nfs-error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion (nfs-error-stat e) 
			    (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					   :old-attrs attrs)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault 
			    (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					   :old-attrs attrs))))))
	(provider 
	 (make-xunion :access (make-wcc-data)))
	(t 
	 (make-xunion :bad-handle (make-wcc-data)))))))

;; remove -- remove a file
(defrpc call-remove 12 
  dir-op-args3
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle filename)
    (make-dir-op-args3 :dir dhandle :name filename))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Remove a file.")
  (:handler #'%handle-remove))


;; ------------------------------------------------------
;; rmdir -- remove a directory 
;; RMDIR3res NFSPROC3_RMDIR(RMDIR3args)             = 13;

(defun %handle-rmdir (args)
  (with-slots (dir name) args
    (destructuring-bind (provider dh) (fh-provider-handle dir)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (let ((attrs (maybe-provider-attrs provider dh)))
	   (handler-case 
	       (progn
		 (nfs-provider-remove-dir provider dh name)
		 (make-xunion :ok 
			      (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					     :old-attrs attrs)))
	     (nfs-error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion (nfs-error-stat e) 
			    (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					   :old-attrs attrs)))
	     (error (e)
	       (nfs-log :info "~A" e)
	       (make-xunion :server-fault 
			    (make-wcc-data :attrs (maybe-provider-attrs provider dh)
					   :old-attrs attrs))))))
	(provider 
	 (make-xunion :access (make-wcc-data)))
	(t 
	 (make-xunion :bad-handle (make-wcc-data)))))))

(defrpc call-rmdir 13 
  dir-op-args3
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (dhandle name)
    (make-dir-op-args3 :dir dhandle :name name))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Deletes a directory.")
  (:handler #'%handle-rmdir))
	       
;; ------------------------------------------------------
;; rename -- rename a file or directory 
;; RENAME3res NFSPROC3_RENAME(RENAME3args)           = 14;

(defun %handle-rename (args)
  (destructuring-bind (from to) args
    (let ((fdh (dir-op-args3-dir from))
	  (fname (dir-op-args3-name from))
	  (tdh (dir-op-args3-dir to))
	  (tname (dir-op-args3-name to)))
      (destructuring-bind (fprovider fhandle) (fh-provider-handle fdh)
	(destructuring-bind (tprovider thandle) (fh-provider-handle tdh)
	  (cond
	    ((and fprovider tprovider (eq fprovider tprovider) (client-mounted-p fprovider *rpc-remote-host*))
	     (let ((from-attrs (maybe-provider-attrs fprovider fhandle))
		   (to-attrs (maybe-provider-attrs fprovider thandle)))
	       (handler-case 
		   (progn
		     (nfs-provider-rename fprovider fhandle fname thandle tname)
		     (make-xunion :ok 
				  (list (make-wcc-data :attrs (maybe-provider-attrs fprovider fhandle)
						       :old-attrs from-attrs)
					(make-wcc-data :attrs (maybe-provider-attrs fprovider thandle)
						       :old-attrs to-attrs))))
		 (nfs-error (e)
		   (nfs-log :info "~A" e)
		   (make-xunion (nfs-error-stat e)
				(list (make-wcc-data :attrs (maybe-provider-attrs fprovider fhandle)
						     :old-attrs from-attrs)
				      (make-wcc-data :attrs (maybe-provider-attrs fprovider thandle)
						     :old-attrs to-attrs))))
		 (error (e)
		   (nfs-log :info "~A" e)
		   (make-xunion :server-fault
				(list (make-wcc-data :attrs (maybe-provider-attrs fprovider fhandle)
						     :old-attrs from-attrs)
				      (make-wcc-data :attrs (maybe-provider-attrs fprovider thandle)
						     :old-attrs to-attrs)))))))
	    ((and fprovider tprovider (eq fprovider tprovider))
	     (make-xunion :access (list (make-wcc-data) (make-wcc-data))))
	    (t 
	     (make-xunion :bad-handle (list (make-wcc-data) (make-wcc-data))))))))))

(defrpc call-rename 14 
  (:list dir-op-args3 dir-op-args3)
  (:union nfs-stat3
    (:ok (:list wcc-data wcc-data))
    (otherwise (:list wcc-data wcc-data)))
  (:program nfs 3)
  (:arg-transformer (from-dhandle from-filename to-filename &key to-dhandle)
    (list (make-dir-op-args3 :dir from-dhandle 
						    :name from-filename)
				 (make-dir-op-args3 :dir (or to-dhandle from-dhandle)
						    :name to-filename)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (wcc1 wcc2) (xunion-val res)
	  (values wcc1 wcc2))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Rename a file.")
  (:handler #'%handle-rename))



;; ------------------------------------------------------
;; link -- create a link to a file
;; LINK3res NFSPROC3_LINK(LINK3args)               = 15;

(defun %handle-link (args)
  (destructuring-bind (fh dirop) args
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (handler-case 
	     (with-slots (dir name) dirop
	       (destructuring-bind (provider2 dh) (fh-provider-handle dir)
		 (if (and provider2 (eq provider2 provider))
		     (progn 
		       (nfs-provider-link provider handle dh name)
		       (make-xunion :ok
				    (list nil (make-wcc-data))))
		     (make-xunion :bad-handle 
				  (list nil (make-wcc-data))))))
	   (nfs-error (e)
	     (make-xunion (nfs-error-stat e)
			  (list nil (make-wcc-data))))
	   (error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion :server-fault 
			  (list nil (make-wcc-data))))))
	(provider 
	 (make-xunion :access (list nil (make-wcc-data))))
	(t 
	 (make-xunion :bad-handle 
		      (list nil (make-wcc-data))))))))

(defrpc call-link 15 
  (:list nfs-fh3 dir-op-args3)
  (:union nfs-stat3
    (:ok (:list post-op-attr wcc-data))
    (otherwise (:list post-op-attr wcc-data)))
  (:program nfs 3)
  (:arg-transformer (handle dhandle filename)
    (list handle (make-dir-op-args3 :dir dhandle :name filename)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr wcc) (xunion-val res)
	  (values attr wcc))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Create a link to a file. Returns (values attrs wcc).")
  (:handler #'%handle-link))
	
  
;; ------------------------------------------------------
;; read dir -- read from a directory 
;; READDIR3res NFSPROC3_READDIR(READDIR3args)         = 16;

(defxstruct entry3 ()
  (fileid fileid3)
  (name filename3)
  (cookie cookie3 0))

(defxstruct %entry3 ()
  (entry entry3)
  (next-entry (:optional %entry3)))

(defxstruct dir-list3 ()
  (entries (:optional %entry3))
  (eof :boolean))

(defun %handle-read-dir (args)
  (destructuring-bind (dh cookie verf count) args
    (destructuring-bind (provider dhandle) (fh-provider-handle dh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (handler-case 
	     (multiple-value-bind (files cookies verifier morep) 
		 (nfs-provider-read-dir provider dhandle
					:cookie cookie
					:verf verf
					:count count)
	       (let ((dlist3 (make-dir-list3 :eof (not morep))))
		 (do ((%files files (cdr %files))
		      (%cookies cookies (cdr %cookies))
		      (fileid 1 (1+ fileid)))
		     ((null %files))
		   (setf (dir-list3-entries dlist3)
			 (make-%entry3 :entry (make-entry3 :fileid fileid
							   :cookie (or (car %cookies) 0)
							   :name (car %files))
				       :next-entry (dir-list3-entries dlist3))))
		 (make-xunion :ok 
			      (list (maybe-provider-attrs provider dhandle)
				    (or verifier (make-cookie-verf3))
				    dlist3))))
	   (nfs-error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion (nfs-error-stat e) 
			  (maybe-provider-attrs provider dhandle)))
	   (error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion :server-fault 
			  (maybe-provider-attrs provider dhandle)))))
	(provider 
	 (make-xunion :access nil))
	(t 
	 (make-xunion :bad-handle nil))))))

(defrpc call-read-dir 16 
  (:list nfs-fh3 cookie3 cookie-verf3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr cookie-verf3 dir-list3))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (dhandle &key cookie-verf (cookie 0) (count 65536))
    (list dhandle cookie (or cookie-verf (make-cookie-verf3)) count))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr cverf dlist) (xunion-val res)
	  (values (do ((entries (dir-list3-entries dlist) (%entry3-next-entry entries))
		       (elist nil))
		      ((null entries) elist)
		    (push (entry3-name (%entry3-entry entries)) elist))
		  (dir-list3-eof dlist)
		  attr
		  cverf))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "List all the files in the directory. Returns (values file-name* attributes cookie-verf).")
  (:handler #'%handle-read-dir))

;; ------------------------------------------------------
;; read dir plus -- extended read from directory 

(defxstruct entry3-plus ()
  (fileid fileid3)
  (name filename3)
  (cookie cookie3 0)
  (attrs post-op-attr)
  (handle post-op-fh3))

(defxstruct %entry3-plus ()
  (entry entry3-plus)
  (next-entry (:optional %entry3-plus)))

(defxstruct dir-list3-plus ()
  (entries (:optional %entry3-plus))
  (eof :boolean))

;; READDIRPLUS3res NFSPROC3_READDIRPLUS(READDIRPLUS3args) = 17;
(defun %handle-read-dir-plus (args)
  (destructuring-bind (dh cookie verf count max) args
    (declare (ignore count))
    (destructuring-bind (provider dhandle) (fh-provider-handle dh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (handler-case 
	     (multiple-value-bind (files cookies verifier morep) 
		 (nfs-provider-read-dir provider dhandle
					:cookie cookie
					:verf verf
					:count max)
	       (let ((dlist3 (make-dir-list3-plus :eof (not morep))))
		 (do ((%files files (cdr %files))
		      (%cookies cookies (cdr %cookies))
		      (fileid 0 (1+ fileid)))
		     ((null %files))
		   (let* ((name (car %files))
			  (handle (nfs-provider-lookup provider dhandle name)))
		     (when handle 
		       (setf (dir-list3-plus-entries dlist3)
			     (make-%entry3-plus :entry 
						(make-entry3-plus :fileid fileid
								  :name name
								  :cookie (or (car %cookies) 0)
								  :attrs (nfs-provider-attrs provider handle)
								  :handle (provider-handle-fh provider handle))
						:next-entry (dir-list3-plus-entries dlist3))))))
		 (make-xunion :ok 
			      (list (nfs-provider-attrs provider dhandle)
				    (or verifier (make-cookie-verf3))
				    dlist3))))
	   (nfs-error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion (nfs-error-stat e) nil))
	   (error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion :server-fault nil))))
	(provider 
	 (make-xunion :access nil))
	(t 
	 (make-xunion :bad-handle nil))))))

(defrpc call-read-dir-plus 17 
  (:list nfs-fh3 cookie3 cookie-verf3 count3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr cookie-verf3 dir-list3-plus))
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (dhandle &key (count 65507) max (cookie 0) cookie-verf)
    (list dhandle cookie (or cookie-verf (make-cookie-verf3)) count (or max count)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr cverf dlist) (xunion-val res)
	  (values (do ((entries (dir-list3-plus-entries dlist) (%entry3-plus-next-entry entries))
		       (elist nil))
		      ((null entries) elist)
		    (let ((entry (%entry3-plus-entry entries)))
		      (push (list (entry3-plus-name entry)
				  (entry3-plus-handle entry)
				  (entry3-plus-attrs entry))
			    elist)))
		  (dir-list3-plus-eof dlist)
		  attr
		  cverf))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Extended directory read. Same as READ-DIR but also collects file attributes and handles. 
Returns (values (name handle attrs)* eof-p attrs cverf).")
  (:handler #'%handle-read-dir-plus))


;; ------------------------------------------------------
;; fs stat -- get dynamic filesystem info

(defxstruct fs-stat ()
  (attrs post-op-attr)
  (tbytes size3 0)
  (fbytes size3 0)
  (abytes size3 0)
  (tfiles size3 0)
  (ffiles size3 0)
  (afiles size3 0)
  (invarsec :uint32 0))

;; FSSTAT3res NFSPROC3_FSSTAT(FSSTAT3args)           = 18;
(defun %handle-fs-stat (fh)
  (destructuring-bind (provider handle) (fh-provider-handle fh)
    (cond
      ((and provider (client-mounted-p provider *rpc-remote-host*))
       (make-xunion :ok (nfs-provider-fs-stat provider handle)))
      (provider 
       (make-xunion :access nil))
      (t 
       (make-xunion :bad-handle nil)))))

(defrpc call-fs-stat 18 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fs-stat)
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Retreive dynamic file system information. Returns an FS-STAT structure.")
  (:handler #'%handle-fs-stat))


;; ------------------------------------------------------
;; fs info -- get static file system info

(defxenum nfs-info 
  (:link #x0001)
  (:symlink #x0002)
  (:homogenous #x0008)
  (:cansettime #x0010))

(defxstruct fs-info ()
  (attrs post-op-attr)
  (rtmax :uint32)
  (rtpref :uint32)
  (rtmult :uint32)
  (wtmax :uint32)
  (wtpref :uint32)
  (wtmult :uint32)
  (dtpref :uint32)
  (max-fsize size3 0)
  (time-delta nfs-time3 (make-nfs-time3))
  (properties :uint32 0))

;; FSINFO3res NFSPROC3_FSINFO(FSINFO3args)           = 19;

(defun %handle-fs-info (fh)
  (destructuring-bind (provider handle) (fh-provider-handle fh)
    (cond
      ((and provider (client-mounted-p provider *rpc-remote-host*))
       (make-xunion :ok (nfs-provider-fs-info provider handle)))
      (provider 
       (make-xunion :access nil))
      (t
       (make-xunion :bad-handle nil)))))


(defrpc call-fs-info 19 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fs-info)
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Retreive static file system information. Returns an FS-INFO structure.")
  (:handler #'%handle-fs-info))

;; ------------------------------------------------------
;; pstconf -- retrieve posix information

(defxstruct path-conf ()
  (attr post-op-attr)
  (link-max :uint32)
  (name-max :uint32)
  (no-trunc :boolean)
  (chown-restricted :boolean)
  (case-insensitive :boolean)
  (case-preserving :boolean))

;; PATHCONF3res NFSPROC3_PATHCONF(PATHCONF3args)       = 20;
(defun %handle-path-conf (fh)
  (destructuring-bind (provider handle) (fh-provider-handle fh)
    (cond
      ((and provider (client-mounted-p provider *rpc-remote-host*))
       (make-xunion :ok (nfs-provider-path-conf provider handle)))
      (provider 
       (make-xunion :access nil))
      (t
       (make-xunion :bad-handle nil)))))

(defrpc call-path-conf 20 
  nfs-fh3
  (:union nfs-stat3
    (:ok path-conf)
    (otherwise post-op-attr))
  (:program nfs 3)
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Retreive POSIX information. Returns a PATH-CONF object.")
  (:handler #'%handle-path-conf))

;; ------------------------------------------------------
;; commit -- commit cached data on a server to stable storage

;; COMMIT3res NFSPROC3_COMMIT(COMMIT3args)           = 21;
(defun %handle-commit (args)
  (destructuring-bind (fh offset count) args
    (destructuring-bind (provider handle) (fh-provider-handle fh)
      (cond
	((and provider (client-mounted-p provider *rpc-remote-host*))
	 (handler-case 
	     (progn
	       (nfs-provider-commit provider handle offset count)
	       (make-xunion :ok (list (make-wcc-data) (make-write-verf3))))
	   (nfs-error (e)
	     (make-xunion (nfs-error-stat e)
			  (make-wcc-data)))
	   (error (e)
	     (nfs-log :info "~A" e)
	     (make-xunion :server-fault (make-wcc-data)))))
	(provider 
	 (make-xunion :access (make-wcc-data)))
	(t 
	 (make-xunion :bad-handle (make-wcc-data)))))))


(defrpc call-commit 21 
  (:list nfs-fh3 offset3 count3)
  (:union nfs-stat3
    (:ok (:list wcc-data write-verf3))
    (otherwise wcc-data))
  (:program nfs 3)
  (:arg-transformer (handle offset count) (list handle offset count))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (wcc verf) (xunion-val res)
	  (values wcc verf))
	(error 'nfs-error :stat (xunion-tag res))))
  (:documentation "Commit cached data on a server to stable storage. Returns (values wcc-data verf).")
  (:handler #'%handle-commit))


