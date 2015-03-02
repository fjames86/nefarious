;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; nfs protocol
(defpackage #:nefarious
 (:use #:cl #:frpc #:nefarious.handles #:trivial-gray-streams)
 (:nicknames #:nfs)
 (:export #:call-null
	  #:call-get-attr
	  #:call-set-attr
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
	  #:stop
	  #:with-nfs-mount
	  #:with-nfs-file
	  #:nfs-file-stream
	  #:make-nfs-file-stream))

