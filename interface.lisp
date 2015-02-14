;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)

(use-rpc-program +nfs-program+)
(use-rpc-version +nfs-version+)

;; ------------------------------------------------------
;; void NFSPROC3_NULL(void)                    = 0;
(defrpc %call-null (:void :void) 0)
(defhandler %handle-null (void) 0
  (declare (ignore void))
  nil)

;; ------------------------------------------------------
;; GETATTR3res NFSPROC3_GETATTR(GETATTR3args)         = 1;

;; getattr  - get file attributes
(defxtype* get-attr-args () nfs-fh3)
(defxtype* get-attr-res-ok () fattr3)
(defxunion get-attr-res (nfs-stat3)
  ((:ok get-attr-res-ok)
   (otherwise :void)))

(defrpc call-getattr (get-attr-args get-attr-res) 1)
(defhandler %handle-getattr (handle) 1
  (declare (ignore handle))
  (make-xunion :ok
	       (make-fattr3 :mode 0
			    :uid 0
			    :gid 0
			    :size 0
			    :used 0
			    :rdev (make-specdata3)
			    :fileid 0
			    :atime (make-nfs-time3)
			    :mtime (make-nfs-time3)
			    :ctime (make-nfs-time3))))


;; ------------------------------------------------------
;; SETATTR3res NFSPROC3_SETATTR(SETATTR3args)         = 2;

;; setattr -- set file attributes
(defxtype* sattr-guard () (:optional nfs-time3))
(defxstruct set-attr-args ()
  ((object nfs-fh3)
   (new-attrs sattr3)
   (guard sattr-guard)))
(defxtype* set-attr-res-ok () wcc-data)
(defxtype* set-attr-res-fail () wcc-data)
(defxunion set-attr-res (nfs-stat3)
 ((:ok set-attr-res-ok)
  (otherwise set-attr-res-fail)))

(defrpc call-setattr (set-attr-args set-attr-res) 2)

(defhandler %handle-setattr (args) 2
  (with-slots (handle new-attrs guard) args
    (declare (ignore handle new-attrs guard))
    (make-xunion :ok (make-wcc-data))))


;; ------------------------------------------------------
;; LOOKUP3res NFSPROC3_LOOKUP(LOOKUP3args)           = 3;

;; lookup -- lookup filename
(defxtype* lookup-args () dir-op-args3)
(defxstruct lookup-res-ok ()
  ((object nfs-fh3)
   (obj-attrs post-op-attr)
   (dir-attrs post-op-attr)))
(defxtype* lookup-res-fail () post-op-attr)

(defxunion lookup-res (nfs-stat3)
  ((:ok lookup-res-ok)
   (otherwise lookup-res-fail)))

(defrpc call-lookup (lookup-args lookup-res) 3)

(defhandler %handle-lookup (arg) 3
  (with-slots (dir name) arg
    (declare (ignore name))
    (make-xunion :ok 
		 (make-lookup-res-ok :object dir))))
				   
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

(defxstruct access-args ()
  ((object nfs-fh3)
   (access :uint32)))

(defxstruct access-res-ok ()
  ((obj-attrs post-op-attr)
   (access :uint32)))

(defxtype* access-res-fail () post-op-attr)

(defxunion access-res (nfs-stat3)
  ((:ok access-res-ok)
   (otherwise access-res-fail)))

(defrpc call-access (access-args access-res) 4)
(defhandler %handle-access (args) 4
  (with-slots (object access) args
    (declare (ignore object access))
    (make-xunion :ok
		 (make-access-res-ok))))

;; ------------------------------------------------------
;; readlink -- read from symbolic link
;; READLINK3res NFSPROC3_READLINK(READLINK3args)       = 5;

(defxtype* readlink-args () nfs-fh3)

(defxstruct readlink-res-ok () 
  ((slink-attrs post-op-attr)
   (data nfs-path3)))

(defxtype* readlink-res-fail () post-op-attr)

(defxunion readlink-res (nfs-stat3)
  ((:ok readlink-res-ok)
   (otherwise readlink-res-fail)))

(defrpc call-readlink (readlink-args readlink-res) 5)
(defhandler %handle-readlink (handle) 5
  (declare (ignore handle))
  (make-xunion :ok
	       (make-readlink-res-ok :data "")))

;; ------------------------------------------------------
;; read -- read from file
;; READ3res NFSPROC3_READ(READ3args)               = 6;

(defxstruct read-args ()
  ((file nfs-fh3)
   (offset offset3)
   (count count3)))

(defxstruct read-res-ok ()
  ((fattrs post-op-attr)
   (count count3)
   (eof :boolean)
   (data (:varray :octet))))

(defxtype* read-res-fail () post-op-attr)

(defxunion read-res (nfs-stat3)
  ((:ok read-res-ok)
   (otherwise read-res-fail)))

(defrpc call-read (read-args read-res) 6)

(defhandler %handle-read (args) 6
  (with-slots (file offset count) args
    (declare (ignore file offset count))
    (make-xunion :ok 
		 (make-read-res-ok :count 0
				   :eof t))))
				 
;; ------------------------------------------------------
;; write -- write to file
;; WRITE3res NFSPROC3_WRITE(WRITE3args)             = 7;

(defxenum stable-how 
  ((:unstable 0)
   (:data-sync 1)
   (:file-sync 2)))

(defxstruct write-args ()
  ((file nfs-fh3)
   (offset offset3)
   (count count3)
   (stable stable-how)
   (opaque (:varray :octet))))

(defxstruct write-res-ok ()
  ((file-wcc wcc-data)
   (count count3)
   (committed stable-how)
   (verf write-verf3)))

(defxtype* write-res-fail () wcc-data)

(defxunion write-res (nfs-stat3)
  ((:ok write-res-ok)
   (otherwise write-res-fail)))

(defrpc call-write (write-args write-res) 7)
(defhandler %handle-write (args) 7
  (with-slots (file offset count stable opaque) args
    (declare (ignore file offset count stable opaque))
    (make-xunion :ok
		 (make-write-res-ok :count 0
				    :verf (make-array +nfs-write-verf-size+ 
						      :initial-element 0)))))

;; ------------------------------------------------------
;; create -- create a file
;; CREATE3res NFSPROC3_CREATE(CREATE3args)           = 8;

(defxenum create-mode3 
  ((:unchecked 0)
   (:guarded 1)
   (:exclusive 2)))

(defxunion create-how3 (create-mode3)
  (((:unchecked :guarded) sattr3)
   (:exclusive create-verf3)))

(defxstruct create-args ()
  ((where dir-op-args3)
   (how create-how3)))

(defxstruct create-res-ok ()
  ((obj post-op-fh3)
   (obj-attrs post-op-attr)
   (dir-wcc wcc-data)))

(defxtype* create-res-fail () wcc-data)

(defxunion create-res (nfs-stat3)
  ((:ok create-res-ok)
   (otherwise create-res-fail)))

(defrpc call-create (create-args create-res) 8)
(defhandler %handle-create (args) 8
  (with-slots (where how) args
    (declare (ignore where how))
    (make-xunion :ok
		 (make-create-res-ok))))

;; ------------------------------------------------------
;; mkdir -- create a directory 
;; MKDIR3res NFSPROC3_MKDIR(MKDIR3args)             = 9;

(defxstruct mkdir-args ()
  ((where dir-op-args3)
   (attrs sattr3)))

(defxstruct mkdir-res-ok ()
  ((obj post-op-fh3)
   (attrs post-op-attr)
   (dir-wcc wcc-data)))

(defxtype* mkdir-res-fail () wcc-data)

(defxunion mkdir-res (nfs-stat3)
  ((:ok mkdir-res-ok)
   (otherwise mkdir-res-fail)))

(defrpc call-mkdir (mkdir-args mkdir-res) 9)
(defhandler %handle-mkdir (args) 9
  (with-slots (where attrs) args
    (declare (ignore where attrs))
    (make-xunion :ok 
		 (make-mkdir-res-ok))))

;; ------------------------------------------------------
;; symlink -- create a symbolic link
;; SYMLINK3res NFSPROC3_SYMLINK(SYMLINK3args)         = 10;

(defxstruct symlink-data3 ()
  ((attrs sattr3)
   (data nfs-path3)))

(defxstruct symlink-args ()
  ((where dir-op-args3)
   (slink symlink-data3)))

(defxstruct symlink-res-ok ()
  ((obj post-op-fh3)
   (attrs post-op-attr)
   (data wcc-data)))

(defxtype* symlink-res-fail () wcc-data)

(defxunion symlink-res (nfs-stat3)
  ((:ok symlink-res-ok)
   (otherwise symlink-res-fail)))

(defrpc call-symlink (symlink-args symlink-res) 10)
(defhandler %handle-symlink (args) 10
  (with-slots (attrs data) args
    (declare (ignore attrs data))
    (make-xunion :ok
		 (make-symlink-res-ok))))

;; ------------------------------------------------------
;; mknod -- create special device
;; MKNOD3res NFSPROC3_MKNOD(MKNOD3args)             = 11;

(defxstruct device-data3 ()
  ((attrs sattr3)
   (spec specdata3)))

(defxunion mknod-data3 (ftype3)
  (((:chr :blk) device-data3)
   ((:sock :fifo) satt3)
   (otherwise :void)))

(defxstruct mknod-args ()
  ((where dir-op-args3)
   (what mkdor-data3)))

(defxstruct mknod-res-ok ()
  ((obj post-op-fh3)
   (attrs post-op-attr)
   (data wcc-data)))

(defxtype* mknod-res-fail () wcc-data)

(defxunion mknod-res (nfs-stat3)
  ((:ok mknod-res-ok)
   (otherwise mknod-res-fail)))

(defrpc call-mknod (mknod-args mknod-res) 11)
(defhandler %handle-mknod (args) 11
  (with-slots (attrs spec) args
    (declare (ignore attrs spec))
    (make-xunion :ok
		 (make-mknod-res-ok))))


;; ------------------------------------------------------
;; remove -- remove a file

(defxtype* remove-args () dir-op-args3)
(defxtype* remove-res-ok () wcc-data)
(defxtype* remove-res-fail () wcc-data)
(defxunion remove-res (nfs-stat3)
  ((:ok remove-res-ok)
   (otherwise remove-res-fail)))

;; REMOVE3res NFSPROC3_REMOVE(REMOVE3args)           = 12;
(defrpc call-remove (remove-args remove-res) 12)
(defhandler %handle-remove (args) 12
  (with-slots (dir name) args
    (declare (ignore dir name))
    (make-xunion :ok nil)))
	       
;; ------------------------------------------------------
;; rmdir -- remove a directory 

(defxtype* rmdir-args () dir-op-args3)
(defxtype* rmdir-res-ok () wcc-data)
(defxtype* rmdir-res-fail () wcc-data)
(defxunion rmdir-res (nfs-stat3)
  ((:ok rmdir-res-ok)
   (otherwise rmdir-res-fail)))

;; RMDIR3res NFSPROC3_RMDIR(RMDIR3args)             = 13;
(defrpc call-rmdir (rmdir-args rmdir-res) 13)
(defhandler %handle-rmdir (args) 13
  (with-slots (dir name) args
    (declare (ignore dir name))
    (make-xunion :ok nil)))
	       
;; ------------------------------------------------------
;; rename -- rename a file or directory 

(defxstruct rename-args ()
  ((from dir-op-args3)
   (to dir-op-args3)))

(defxstruct rename-res-ok ()
  ((from wcc-data)
   (to wcc-data)))

(defxstruct rename-res-fail ()
  ((from wcc-data)
   (to wcc-data)))

(defxunion rename-res (nfs-stat3)
  ((:ok rename-res-ok)
   (otherwise rename-res-fail)))

;; RENAME3res NFSPROC3_RENAME(RENAME3args)           = 14;
(defrpc call-rename (rename-args rename-res) 14)
(defhandler %handle-rename (args) 14
  (with-slots (from to) args
    (declare (ignore from to))
    (make-xunion :ok
		 (make-rename-res-ok))))


;; ------------------------------------------------------
;; link -- create a link to a file

(defxstruct link-args ()
  ((file nfs-fh3)
   (link dir-op-args3)))

(defxstruct link-res-ok ()
  ((attrs post-op-attr)
   (data wcc-data)))

(defxstruct link-res-fail () 
  ((attrs post-op-attr)
   (data wcc-data)))

(defxunion link-res (nfs-stat3)
  ((:ok link-res-ok)
   (otherwise link-res-fail)))

;; LINK3res NFSPROC3_LINK(LINK3args)               = 15;
(defrpc call-link (link-args link-res) 15)
(defhandler %handle-link (args) 15
  (with-slots (file link) args
    (declare (ignore file link))
    (make-xunion :ok
		 (make-link-res-ok))))

;; ------------------------------------------------------
;; read dir -- read from a directory 

(defxstruct read-dir-args ()
  ((dir nfs-fh3)
   (cookie cookie3)
   (verf cookie-verf3)
   (count count3)))

(defxstruct entry3 ()
  ((fileid fileid3)
   (name filename3)
   (cookie cookie3)
   (next-entry (:optional entry3))))

(defxstruct dir-list3 ()
  ((entries (:optional entry3))
   (eof :boolean)))

(defxstruct read-dir-res-ok ()
  ((attrs post-op-attr)
   (verf cookie-verf3)
   (reply dir-list3)))

(defxtype* read-dir-res-fail () post-op-attr)

(defxunion read-dir-res (nfs-stat3)
  ((:ok read-dir-res-ok)
   (otherwise read-dir-res-fail)))
  
;; READDIR3res NFSPROC3_READDIR(READDIR3args)         = 16;
(defrpc call-read-dir (read-dir-args read-dir-res) 16)
(defhandler %handle-read-dir (args) 16
  (with-slots (dir cookie verf count) args
    (declare (ignore dir cookie verf count))
    (make-xunion :ok
		 (make-read-dir-res-ok))))

;; ------------------------------------------------------
;; read dir plus -- extended read from directory 

(defxstruct read-dir*-args ()
  ((dir nfs-fh3)
   (cookie cookie3)
   (verf cookie-verf3)
   (count count3)
   (max count3)))

(defxstruct entry3* ()
  ((fileid fileid3)
   (name filename3)
   (cookie cookie3)
   (attrs post-op-attr)
   (handle post-op-fh3)
   (next-entry (:optional entry3*))))

(defxstruct dir-list3* ()
  ((entries (:optional entry3*))
   (eof :boolean)))

(defxstruct read-dir*-res-ok ()
  ((attrs post-op-attr)
   (cookie cookie-verf3)
   (reply dir-list3*)))

(defxtype* read-dir*-res-fail () post-op-attr)

(defxunion read-dir*-res (nfs-stat3)
  ((:ok read-dir*-res-ok)
   (otherwise read-dir*-res-fail)))

;; READDIRPLUS3res NFSPROC3_READDIRPLUS(READDIRPLUS3args) = 17;
(defrpc call-read-dir* (read-dir*-args read-dir*-res) 17)
(defhandler %handle-read-dir* (args) 17
  (with-slots (dir cookie verf count max) args
    (declare (ignore dir cookie verf count max))
    (make-xunion :ok 
		 (make-read-dir*-res-ok))))

;; ------------------------------------------------------
;; fs stat -- get dynamic filesystem info

(defxtype* fs-stat-args () nfs-fh3)

(defxstruct fs-stat-res-ok ()
  ((attrs post-op-attr)
   (tbytes size3)
   (fbytes size3)
   (abytes size3)
   (tfiles size3)
   (ffiles size3)
   (afiles size3)
   (invarsec :uint32)))

(defxtype* fs-stat-res-fail () post-op-attr)

(defxunion fs-stat-res (nfs-stat3)
  ((:ok fs-stat-res-ok)
   (otherwise fs-stat-res-fail)))

;; FSSTAT3res NFSPROC3_FSSTAT(FSSTAT3args)           = 18;
(defrpc call-fs-stat (fs-stat-args fs-stat-res) 18)
(defhandler %handle-fs-stat (handle) 18
  (declare (ignore handle))
  (make-xunion :ok 
	       (make-fs-stat-res-ok)))

;; ------------------------------------------------------
;; fs info -- get static file system info

(defxenum nfs-info 
  ((:link #x0001)
   (:symlink #x0002)
   (:homogenous #x0008)
   (:cansettime #x0010)))

(defxtype* fs-info-args () nfs-fh3)

(defxstruct fs-info-res-ok ()
  ((attrs post-op-attr)
   (rtmax :uint32)
   (rtpref :uint32)
   (rtmult :uint32)
   (wtmax :uint32)
   (wrpref :uint32)
   (wtmulf :uint32)
   (dtpref :uint32)
   (max-fsize size3)
   (time-delta nfs-time3)
   (properties :uint32)))

(defxtype* fs-info-res-fail () post-op-attr)

(defxunion fs-info-res (nfs-stat3)
  ((:ok fs-info-res-ok)
   (otherwise fs-info-res-fail)))

;; FSINFO3res NFSPROC3_FSINFO(FSINFO3args)           = 19;
(defrpc call-fs-info (fs-info-args fs-info-res) 19)
(defhandler %handle-fs-info (handle) 19
  (declare (ignore handle))
  (make-xunion :ok
	       (make-fs-info-res-ok)))

;; ------------------------------------------------------
;; pstconf -- retrieve posix information

(defxtype* path-conf-args () nfs-fh3)

(defxstruct path-conf-res-ok ()
  ((attr post-op-attr)
   (link-max :uint32)
   (name-max :uint32)
   (no-trunc :boolean)
   (chown-restricted :boolean)
   (case-insensitive :boolean)
   (case-preserving :boolean)))

(defxtype* path-conf-res-fail () post-op-attr)

(defxunion post-conf-res (nfs-stat3)
  ((:ok path-conf-res-ok)
   (otherwise path-conf-res-fail)))

;; PATHCONF3res NFSPROC3_PATHCONF(PATHCONF3args)       = 20;
(defrpc call-path-conf (path-conf-args path-conf-res) 20)
(defhandler %handle-path-conf (handle) 20
  (declare (ignore handle))
  (make-xunion :ok
	       (make-path-conf-res-ok)))

;; ------------------------------------------------------
;; commit -- commit cached data on a server to stable storage

(defxstruct commit-args ()
  ((file nfs-fh3)
   (offset offset3)
   (count count3)))

(defxstruct commit-res-ok ()
  ((data wcc-data)
   (verf write-verf3)))

(defxtype* commit-res-fail () wcc-data)

(defxunion commit-res (nfs-stat3)
  ((:ok commit-res-ok)
   (otherwise commit-res-fail)))

;; COMMIT3res NFSPROC3_COMMIT(COMMIT3args)           = 21;
(defrpc call-commit (commit-args commit-res) 21)
(defhandler %handle-commit (args) 21
  (with-slots (file offset count) args
    (declare (ignore file offset count))
    (make-xunion :ok
		 (make-commit-res-ok))))

