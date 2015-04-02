;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; nfs protocol
(defpackage #:nefarious
 (:use #:cl #:frpc #:trivial-gray-streams)
 (:nicknames #:nfs)
 (:export #:call-null
	  #:call-get-attrs
	  #:call-set-attrs
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

	  ;; some of the essential NFS structures
	  #:make-fattr3
	  #:fattr3-type
	  #:fattr3-mode
	  #:fattr3-nlink
	  #:fattr3-uid
	  #:fattr3-gid
	  #:fattr3-size
	  #:fattr3-used
	  #:fattr3-rdev
	  #:fattr3-fsid
	  #:fattr3-fileid
	  #:fattr3-atime 
	  #:fattr3-mtime 
	  #:fattr3-ctime 

	  #:make-nfs-time3
	  #:make-fs-info
	  #:make-fs-stat
	  #:make-path-conf
	  #:pack-mode
	  #:unpack-mode

	  ;; errors
	  #:nfs-error

	  ;; server
	  #:start
	  #:stop

	  ;; client utilities
	  #:copy-remote-file 
	  #:copy-remote-directory 

	  ;; providers
	  #:nfs-provider
	  #:register-provider
	  #:unregister-provider
	  #:find-provider
	  #:provider-handle-fh
	  #:fh-provider-handle
	  #:provider-clients
	  #:provider-path
	  #:provider-id

	  ;; provider generic functions
	  #:nfs-provider-mount
	  #:nfs-provider-unmount
	  #:nfs-provider-attrs
	  #:nfs-provider-lookup
	  #:nfs-provider-access
	  #:nfs-provider-read-link
	  #:nfs-provider-create-symlink
	  #:nfs-provider-link
	  #:nfs-provider-read
	  #:nfs-provider-write
	  #:nfs-provider-create
	  #:nfs-provider-remove
	  #:nfs-provider-rename
	  #:nfs-provider-read-dir
	  #:nfs-provider-create-dir
	  #:nfs-provider-remove-dir
	  #:nfs-provider-fs-info
	  #:nfs-provider-fs-stat
	  #:nfs-provider-path-conf
	  #:nfs-provider-create-device ;; this is for mknod
	  #:nfs-provider-commit 

	  ;; the simple (default) provider
	  #:make-simple-provider
	  #:simple-provider

	  ;; logging
	  #:nfs-log
	  ))

