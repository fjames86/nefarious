;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; nfs protocol
(defpackage #:nefarious
 (:use #:cl #:frpc)
 (:nicknames #:nfs)
 (:export #:call-null
	  #:call-getattr
	  #:call-setattr
	  #:call-lookup
	  #:call-access
	  #:call-readlink
	  #:call-read
	  #:call-write
	  #:call-create
	  #:call-mkdir
	  #:call-create-symlink
	  #:call-mknod
	  #:call-remove
	  #:call-rmdir
	  #:call-rename
	  #:call-link
	  #:call-read-dir
	  #:call-read-dir-plus
	  #:call-fs-stat
	  #:call-fs-info
	  #:call-path-conf
	  #:call-commit
	  #:start
	  #:stop))

;; for the mount protocol
(defpackage #:nefarious.mount
  (:use #:cl #:frpc)
  (:nicknames #:nfs.mount)
  (:export #:call-null
	   #:call-mount
	   #:call-dump
	   #:call-unmount
	   #:call-unmount-all
	   #:call-export
	   #:*mount-port*))
