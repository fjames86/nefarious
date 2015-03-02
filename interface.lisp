;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(use-rpc-program +nfs-program+ +nfs-version+)
(use-rpc-port 2049)

;; ------------------------------------------------------
;; void NFSPROC3_NULL(void)                    = 0;
(defrpc call-null 0 :void :void)

(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; ------------------------------------------------------
;; GETATTR3res NFSPROC3_GETATTR(GETATTR3args)         = 1;

;; getattr  - get file attributes
;;(defxtype* get-attr-args () nfs-fh3)
;;(defxtype* get-attr-res-ok () fattr3)
;;(defxunion get-attr-res (nfs-stat3)
;;  ((:ok get-attr-res-ok)
;;   (otherwise :void)))

(defrpc call-get-attr 1 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fattr3)
    (otherwise :void))
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "GETATTR failed: ~A" (xunion-tag res))))
  (:documentation "Get file attributes"))
 
(defhandler %handle-get-attr (fh 1)
  (let ((handle (find-handle fh)))
    (if handle
	(make-xunion :ok
		     (let ((size (file-size handle)))
		     (make-fattr3 :type (if (handle-directory-p handle)
					    :dir
					    :reg)
				  :mode 0
				  :uid 0
				  :gid 0
				  :size size
				  :used size
				  :rdev (make-specdata3)
				  :fileid 0
				  :atime (make-nfs-time3)
				  :mtime (make-nfs-time3)
				  :ctime (make-nfs-time3))))
	(make-xunion :bad-handle nil))))


;; ------------------------------------------------------
;; SETATTR3res NFSPROC3_SETATTR(SETATTR3args)         = 2;

;; setattr -- set file attributes
;;(defxtype* sattr-guard () (:optional nfs-time3))
;;(defxstruct set-attr-args ()
;;  ((object nfs-fh3)
;;   (new-attrs sattr3)
;;   (guard sattr-guard)))

(defrpc call-set-attr 2 
  (:list nfs-fh3 sattr3 (:optional nfs-time3))
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:arg-transformer (handle attr &key time) (list handle attr time))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "SETATTR failed: ~A" (xunion-tag res))))
  (:documentation "Set attributes"))

;;(defhandler %handle-set-attr (args 2)
;;  (destructuring-bind (fh new-attrs guard) args
;;    (declare (ignore fh new-attrs guard))
;;    (make-xunion :ok (make-wcc-data))))


;; ------------------------------------------------------
;; LOOKUP3res NFSPROC3_LOOKUP(LOOKUP3args)           = 3;

;; lookup -- lookup filename
(defrpc call-lookup 3
  dir-op-args3 
  (:union nfs-stat3
    (:ok (:list nfs-fh3 post-op-attr post-op-attr))
    (otherwise post-op-attr))
  (:arg-transformer (dhandle filename) 
    (make-dir-op-args3 :dir dhandle :name filename))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr1 attr2) (xunion-val res)
	  (values fh attr1 attr2))
	(error "LOOKUP failed: ~A" (xunion-tag res)))))

(defhandler %handle-lookup (arg 3)
  (with-slots (dir name) arg
    (let ((dhandle (find-handle dir)))
      (if dhandle
	  (let ((handle (allocate-handle dhandle name)))
	    (if handle
		(make-xunion :ok
			     (list (handle-fh handle)
				   nil
				   nil))
		(make-xunion :noent nil)))
	  (make-xunion :bad-handle nil)))))

;; ------------------------------------------------------
;; access -- check access permission

;; ACCESS3res NFSPROC3_ACCESS(ACCESS3args)           = 4;

(defxenum nfs-access
 ((:read #x0001)
  (:lookup #x0002)
  (:modify #x0004)
  (:extend #x0008)
  (:delete #x0010)
  (:execute #x0020)))

;;(defxstruct access-args ()
;;  ((object nfs-fh3)
;;   (access :uint32)))

(defrpc call-access 4 
  (:list nfs-fh3 :uint32)
  (:union nfs-stat3 
    (:ok (:list post-op-attr :uint32))
    (otherwise post-op-attr))
  (:arg-transformer (handle access)
    (list handle
	  (if (integerp access)
	      access
	      (reduce (lambda (val sym)
			(logior val (enum 'nfs-access sym)))
		      access))))
  (:documentation "ACCESS can be either an integer which is a bitwise OR of NFS-ACCESS flags, or a list 
of NFS-ACCESS flag symbols."))

(defhandler %handle-access (args 4)
  (destructuring-bind (fh access) args
    (declare (ignore fh))
    (make-xunion :ok (list nil access))))

;; ------------------------------------------------------
;; readlink -- read from symbolic link
;; READLINK3res NFSPROC3_READLINK(READLINK3args)       = 5;

(defrpc call-readlink 5 
  nfs-fh3
  (:union nfs-stat3
    (:ok (:list post-op-attr nfs-path3))
    (otherwise post-op-attr))
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr path) (xunion-val res)
	  (values attr path))
	(error "READLINK failed: ~A" (xunion-tag res)))))

(defhandler %handle-readlink (handle 5)
  (declare (ignore handle))
  (make-xunion :ok (list nil "")))

;; ------------------------------------------------------
;; read -- read from file
;; READ3res NFSPROC3_READ(READ3args)               = 6;

(defrpc call-read 6 
  (:list nfs-fh3 offset3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr
		count3
		:boolean
		(:varray* :octet)))
    (otherwise post-op-attr))
  (:arg-transformer (handle offset count)
    (list handle offset count))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr count eof data) (xunion-val res)
	  (declare (ignore count))
	  (values data eof attr))
	(error "READ failed: ~A" (xunion-tag res)))))

(defhandler %handle-read (args 6)
  (destructuring-bind (fh offset count) args
    (let ((handle (find-handle fh)))
      (if handle
	  (let ((buffer (read-file handle offset count)))
	    (make-xunion :ok
			 (list nil
			       (length buffer)
			       (if (< (length buffer) count)
				   t
				   nil)
			       buffer)))
	  (make-xunion :bad-handle nil)))))

;; ------------------------------------------------------
;; write -- write to file
;; WRITE3res NFSPROC3_WRITE(WRITE3args)             = 7;

(defxenum stable-how 
  ((:unstable 0)
   (:data-sync 1)
   (:file-sync 2)))

(defrpc call-write 7 
  (:list nfs-fh3 offset3 count3 stable-how (:varray* :octet))
  (:union nfs-stat3
    (:ok (:list wcc-data count3 stable-how write-verf3))
    (otherwise wcc-data))
  (:arg-transformer (handle offset data &key (stable :file-sync))
    (list handle offset (length data) stable data))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(apply #'values (xunion-val res))
	(error "WRITE failed: ~A" (xunion-tag res)))))

(defhandler %handle-write (args 7)
  (destructuring-bind (fh offset count stable data) args
    (declare (ignore stable))
    (declare (ignore count))
    (let ((handle (find-handle fh)))
      (if handle
	  (let ((length (write-file handle offset data)))
	    (make-xunion :ok
			 (list (make-wcc-data)
			       length
			       :file-sync
			       nil)))
	  (make-xunion :bad-handle nil)))))
	    


;; ------------------------------------------------------
;; create -- create a file
;; CREATE3res NFSPROC3_CREATE(CREATE3args)           = 8;

(defxenum create-mode3 
  ((:unchecked 0)
   (:guarded 1)
   (:exclusive 2)))

;;(defxunion create-how3 (create-mode3)
;;  (((:unchecked :guarded) sattr3)
;;   (:exclusive create-verf3)))
;;(defxstruct create-args ()
;;  ((where dir-op-args3)
;;   (how create-how3)))
;;(defxstruct create-res-ok ()
;;  ((obj post-op-fh3)
;;   (obj-attrs post-op-attr)
;;   (dir-wcc wcc-data)))
;;(defxtype* create-res-fail () wcc-data)
;;(defxunion create-res (nfs-stat3)
;;  ((:ok create-res-ok)
;;   (otherwise create-res-fail)))

(defrpc call-create 8 
  (:list dir-op-args3 
	 (:union create-mode3 
	   ((:unchecked :guared) sattr3)
	   (:exclusive created-verf3)))
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
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
	(error "CREATE failed: ~A" (xunion-tag res)))))

(defhandler %handle-create (args 8)
  (destructuring-bind (dirop how) args
    (declare (ignore how))
    (with-slots (dir name) dirop
      (let ((handle (create-file dir name)))
	(if handle
	    (make-xunion :ok
			 (list (handle-fh handle)
			       nil
			       (make-wcc-data)))
	    (make-xunion :noent (make-wcc-data)))))))

;; ------------------------------------------------------
;; mkdir -- create a directory 
;; MKDIR3res NFSPROC3_MKDIR(MKDIR3args)             = 9;

(defrpc call-mkdir 9 
  (:list dir-op-args3 sattr3) 
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:arg-transformer (dhandle filename &key sattr)
    (list (make-dir-op-args3 :dir dhandle :name filename)
	  (or sattr (make-sattr3))))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error "MKDIR failed: ~A" (xunion-tag res)))))

(defhandler %handle-mkdir (args 9)
  (destructuring-bind (dir-op attrs) args
    (declare (ignore attrs))
    (with-slots (dir name) dir-op
      (let ((dhandle (find-handle dir)))
	(if dhandle 
	    (let ((handle (create-directory dhandle name)))
	      (if handle
		  (make-xunion :ok
			       (list (handle-fh handle)
				     nil
				     (make-wcc-data)))
		  (make-xunion :server-fault (make-wcc-data))))
	    (make-xunion :bad-handle (make-wcc-data)))))))

;; ------------------------------------------------------
;; symlink -- create a symbolic link
;; SYMLINK3res NFSPROC3_SYMLINK(SYMLINK3args)         = 10;

;;(defxtype* symlink-data3 ()
;;  (:alist (:attrs sattr3)
;;	  (:data nfs-path3)))

(defrpc call-create-symlink 10 
  (:list dir-op-args3 sattr3 nfs-path3)
  (:union nfs-stat3
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
  (:arg-transformer (dhandle filename path &key attrs)
    (list (make-dir-op-args3 :dir dhandle 
			     :name filename)
	  attrs
	  path))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (fh attr wcc) (xunion-val res)
	  (values fh attr wcc))
	(error "CREATE-SYMLINK failed: ~A" (xunion-tag res))))
  (:documentation 
  "Create a symblink.

DHANDLE: directory handle to create symlink in. 

FILENAME: name to be associated with the link.

PATH: the symbolic link data.

ATTRS: initial attributes for the symlink."))


;;(defhandler %handle-create-symlink (args 10)
;;  (destructuring-bind (dirop attrs path) args
;;    (declare (ignore dirop attrs path))
;;    (make-xunion :ok (list nil nil (make-wcc-data)))))

;; ------------------------------------------------------
;; mknod -- create special device
;; MKNOD3res NFSPROC3_MKNOD(MKNOD3args)             = 11;

(defrpc call-mknod 11 
  (:list dir-op-args3 
	 (:union ftype3
	   ((:chr :blk) (:list sattr3 specdata3))
	   ((:sock :fifo) sattr3)
	   (otherwise :void)))
  (:union nfs-stat3 
    (:ok (:list post-op-fh3 post-op-attr wcc-data))
    (otherwise wcc-data))
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
	(error "MKNOD failed: ~A" (xunion-tag res))))
  (:documentation "Create special device."))

;; dont support this
;;(defhandler %handle-mknod (args 11)
;;  (destructuring-bind (dirop spec) args
;;    (declare (ignore dirop spec))
;;    (make-xunion :ok (list nil nil (make-wcc-data)))))


;; ------------------------------------------------------
;; remove -- remove a file

;; REMOVE3res NFSPROC3_REMOVE(REMOVE3args)           = 12;
(defrpc call-remove 12 
  dir-op-args3
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:arg-transformer (dhandle filename)
    (make-dir-op-args3 :dir dhandle :name filename))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "REMOVE failed: ~A" (xunion-tag res))))
  (:documentation "Remove a file."))
	
(defhandler %handle-remove (args 12)
  (with-slots (dir name) args
    (let ((dhandle (find-handle dir)))
      (if dhandle
	  (progn
	    (remove-file dhandle name)
	    (make-xunion :ok (make-wcc-data)))
	  (make-xunion :bad-handle (make-wcc-data))))))
	       
;; ------------------------------------------------------
;; rmdir -- remove a directory 


;; RMDIR3res NFSPROC3_RMDIR(RMDIR3args)             = 13;
(defrpc call-rmdir 13 
  dir-op-args3
  (:union nfs-stat3
    (:ok wcc-data)
    (otherwise wcc-data))
  (:arg-transformer (dhandle name)
    (make-dir-op-args3 :dir dhandle :name name))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "RMDIR failed: ~A" (xunion-tag res)))))

(defhandler %handle-rmdir (args 13)
  (with-slots (dir name) args
    (let ((dhandle (find-handle dir)))
      (if dhandle
	  (progn
	    (remove-directory dhandle name)
	    (make-xunion :ok (make-wcc-data)))
	  (make-xunion :bad-handle (make-wcc-data))))))
	       
;; ------------------------------------------------------
;; rename -- rename a file or directory 

;; RENAME3res NFSPROC3_RENAME(RENAME3args)           = 14;
(defrpc call-rename 14 
  (:list dir-op-args3 dir-op-args3)
  (:union nfs-stat3
    (:ok (:list wcc-data wcc-data))
    (otherwise (:list wcc-data wcc-data)))
  (:arg-transformer (from-dhandle from-filename to-filename &key to-dhandle)
    (list (make-dir-op-args3 :dir from-dhandle 
						    :name from-filename)
				 (make-dir-op-args3 :dir (or to-dhandle from-dhandle)
						    :name to-filename)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (wcc1 wcc2) (xunion-val res)
	  (values wcc1 wcc2))
	(error "RENAME failed: ~A" (xunion-tag res)))))

(defhandler %handle-rename (args 14)
  (destructuring-bind (from to) args
    (let ((fdh (dir-op-args3-dir from))
	  (fname (dir-op-args3-name from))
	  (tdh (dir-op-args3-dir to))
	  (tname (dir-op-args3-name to)))
      (let ((fdhandle (find-handle fdh))
	    (tdhandle (find-handle tdh)))
	(if (and fdhandle tdhandle)
	    (let ((fpath (handle-pathname (make-handle fdhandle fname)))
		  (tpath (handle-pathname (make-handle tdhandle tname))))
	      (rename-file fpath tpath)
	      (make-xunion :ok (list (make-wcc-data) (make-wcc-data))))
	    (make-xunion :bad-handle
			 (list (make-wcc-data) (make-wcc-data))))))))


;; ------------------------------------------------------
;; link -- create a link to a file

;; LINK3res NFSPROC3_LINK(LINK3args)               = 15;
(defrpc call-link 15 
  (:list nfs-fh3 dir-op-args3)
  (:union nfs-stat3
    (:ok (:list post-op-attr wcc-data))
    (otherwise (:list post-op-attr wcc-data)))
  (:arg-transformer (handle dhandle filename)
    (list handle (make-dir-op-args3 :dir dhandle :name filename)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr wcc) (xunion-val res)
	  (values attr wcc))
	(error "LINK failed: ~A" (xunion-tag res)))))
	
;;(defhandler %handle-link (args 15)
;;  (destructuring-bind (handle dirop) args
;;    (declare (ignore handle dirop))
;;    (make-xunion :ok (list nil (make-wcc-data)))))

;; ------------------------------------------------------
;; read dir -- read from a directory 

(defxstruct entry3 ()
  ((fileid fileid3)
   (name filename3)
   (cookie cookie3 0)))

(defxstruct %entry3 ()
  ((entry entry3)
   (next-entry (:optional %entry3))))

(defxstruct dir-list3 ()
  ((entries (:optional %entry3))
   (eof :boolean)))

;; READDIR3res NFSPROC3_READDIR(READDIR3args)         = 16;
(defrpc call-read-dir 16 
  (:list nfs-fh3 cookie3 cookie-verf3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr cookie-verf3 dir-list3))
    (otherwise post-op-attr))
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
	(error "READ-DIR failed: ~A" (xunion-tag res))))
  (:documentation "List all the files in the directory."))

(defhandler %handle-read-dir (args 16)
  (destructuring-bind (dh cookie verf count) args
    (declare (ignore cookie verf count))
    (let ((dhandle (find-handle dh)))
      (if dhandle 
	  (let ((files (cl-fad:list-directory (handle-pathname dhandle)))
		(dlist3 (make-dir-list3 :eof t)))
	    (do ((%files files (cdr %files))
		 (fileid 0 (1+ fileid)))
		((null %files))
	      (let ((path (car %files)))
		(setf (dir-list3-entries dlist3)
		      (make-%entry3 :entry (make-entry3 :fileid fileid
							:name (if (cl-fad:directory-pathname-p path)
								  (car (last (pathname-directory path)))
								  (format nil "~A~A~A" 
								      (pathname-name path)
								      (if (pathname-type path)
									  "."
									  "")
								      (if (pathname-type path)
									  (pathname-type path)
									  ""))))
				    :next-entry (dir-list3-entries dlist3)))))
	    (make-xunion :ok 
			 (list nil (make-cookie-verf3) dlist3)))
	    ;; remove this
;;	    (do ((entries (dir-list3-entries dlist3) (%entry3-next-entry entries))
;;		 (elist nil))
;;		((null entries) elist)
;;	      (push (%entry3-entry entries) elist)))
	  (make-xunion :bad-handle nil)))))

;; ------------------------------------------------------
;; read dir plus -- extended read from directory 

;;(defxstruct read-dir-plus-args ()
;;  ((dir nfs-fh3)
;;   (cookie cookie3)
;;   (verf cookie-verf3)
;;   (count count3)
;;   (max count3)))

(defxstruct entry3-plus ()
  ((fileid fileid3)
   (name filename3)
   (cookie cookie3 0)
   (attrs post-op-attr)
   (handle post-op-fh3)))

(defxstruct %entry3-plus ()
  ((entry entry3-plus)
   (next-entry (:optional %entry3-plus))))

(defxstruct dir-list3-plus ()
  ((entries (:optional %entry3-plus))
   (eof :boolean)))

;; READDIRPLUS3res NFSPROC3_READDIRPLUS(READDIRPLUS3args) = 17;
(defrpc call-read-dir-plus 17 
  (:list nfs-fh3 cookie3 cookie-verf3 count3 count3)
  (:union nfs-stat3
    (:ok (:list post-op-attr cookie-verf3 dir-list3-plus))
    (otherwise post-op-attr))
  (:arg-transformer (dhandle count &key max (cookie 0) cookie-verf)
    (list dhandle cookie (or cookie-verf (make-cookie-verf3)) count (or max count)))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (attr cverf dlist) (xunion-val res)
	  (list attr cverf 
		(make-dir-list3-plus 
		 :eof (dir-list3-plus-eof dlist)
		 :entries
		 (do ((entries (dir-list3-plus-entries dlist) (%entry3-plus-next-entry entries))
		      (elist nil))
		     ((null entries) elist)
		   (push (%entry3-plus-entry entries) elist)))))
	(error "READ-DIR-PLUS failed: ~A" (xunion-tag res)))))

(defhandler %handle-read-dir-plus (args 17)
  (destructuring-bind (dh cookie verf count max) args
    (declare (ignore cookie verf count max))
    (let ((dhandle (find-handle dh)))
      (if dhandle 
	  (let ((files (cl-fad:list-directory (handle-pathname dhandle)))
		(dlist3 (make-dir-list3-plus :eof t)))
	    (do ((%files files (cdr %files))
		 (fileid 0 (1+ fileid)))
		((null %files))
	      (let* ((path (car %files))
		     (handle (allocate-handle dhandle (or (pathname-name path)
							  (car (last (pathname-directory path)))))))
		(when handle 
		  (setf (dir-list3-plus-entries dlist3)
			(make-%entry3-plus :entry 
					   (make-entry3-plus :fileid fileid
							     :name (if (cl-fad:directory-pathname-p path)
								       (car (last (pathname-directory path)))
								       (format nil "~A~A~A" 
									       (pathname-name path)
									       (if (pathname-type path)
										   "."
										   "")
									       (if (pathname-type path)
										   (pathname-type path)
										   "")))
							     :handle (handle-fh handle))
					   :next-entry (dir-list3-plus-entries dlist3))))))
	    (make-xunion :ok 
			 (list nil (make-cookie-verf3) dlist3)))
	  (make-xunion :bad-handle nil)))))

;; ------------------------------------------------------
;; fs stat -- get dynamic filesystem info

(defxstruct fs-stat ()
  ((attrs post-op-attr)
   (tbytes size3 0)
   (fbytes size3 0)
   (abytes size3 0)
   (tfiles size3 0)
   (ffiles size3 0)
   (afiles size3 0)
   (invarsec :uint32 0)))

;; FSSTAT3res NFSPROC3_FSSTAT(FSSTAT3args)           = 18;
(defrpc call-fs-stat 18 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fs-stat)
    (otherwise post-op-attr))
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "FS-STAT failed: ~A" (xunion-tag res)))))

(defhandler %handle-fs-stat (handle 18)
  (declare (ignore handle))
  (make-xunion :ok (make-fs-stat)))

;; ------------------------------------------------------
;; fs info -- get static file system info

(defxenum nfs-info 
  ((:link #x0001)
   (:symlink #x0002)
   (:homogenous #x0008)
   (:cansettime #x0010)))

(defxstruct fs-info ()
  ((attrs post-op-attr)
   (rtmax :uint32)
   (rtpref :uint32)
   (rtmult :uint32)
   (wtmax :uint32)
   (wrpref :uint32)
   (wtmulf :uint32)
   (dtpref :uint32)
   (max-fsize size3 0)
   (time-delta nfs-time3 (make-nfs-time3))
   (properties :uint32 0)))

;; FSINFO3res NFSPROC3_FSINFO(FSINFO3args)           = 19;
(defrpc call-fs-info 19 
  nfs-fh3 
  (:union nfs-stat3
    (:ok fs-info)
    (otherwise post-op-attr))
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "FS-INFO failed: ~A" (xunion-tag res)))))

(defhandler %handle-fs-info (handle 19)
  (declare (ignore handle))
  (make-xunion :ok (make-fs-info)))

;; ------------------------------------------------------
;; pstconf -- retrieve posix information

(defxstruct path-conf ()
  ((attr post-op-attr)
   (link-max :uint32)
   (name-max :uint32)
   (no-trunc :boolean)
   (chown-restricted :boolean)
   (case-insensitive :boolean)
   (case-preserving :boolean)))

;; PATHCONF3res NFSPROC3_PATHCONF(PATHCONF3args)       = 20;
(defrpc call-path-conf 20 
  nfs-fh3
  (:union nfs-stat3
    (:ok path-conf)
    (otherwise post-op-attr))
  (:arg-transformer (handle) handle)
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(xunion-val res)
	(error "PATH-CONF failed: ~A" (xunion-tag res)))))

(defhandler %handle-path-conf (handle 20)
  (declare (ignore handle))
  (make-xunion :ok (make-path-conf :name-max 255
				   :link-max 255)))


;; ------------------------------------------------------
;; commit -- commit cached data on a server to stable storage

;; COMMIT3res NFSPROC3_COMMIT(COMMIT3args)           = 21;
(defrpc call-commit 21 
  (:list nfs-fh3 offset3 count3)
  (:union nfs-stat3
    (:ok (:list wcc-data write-verf3))
    (otherwise wcc-data))
  (:arg-transformer (handle offset count) (list handle offset count))
  (:transformer (res)
    (if (eq (xunion-tag res) :ok)
	(destructuring-bind (wcc verf) (xunion-val res)
	  (values wcc verf))
	(error "COMMIT failed: ~A" (xunion-tag res)))))


;; dont support this
;;(defhandler %handle-commit (args 21)
;;  (destructuring-bind (handle offset count) args
;;    (declare (ignore handle offset count))
;;    (make-xunion :ok (list (make-wcc-data) (make-write-verf3)))))
