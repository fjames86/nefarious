;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;; nfs protocol
(defpackage #:nefarious
 (:use #:cl #:frpc #:trivial-gray-streams)
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

	  ;; some of the essential NFS structures
	  #:fattr3
	  #:make-fattr3
	  #:nfs-time3
	  #:make-nfs-time3
	  #:fs-info
	  #:make-fs-info
	  #:fs-stat
	  #:make-fs-stat
	  #:path-conf
	  #:make-path-conf

	  ;; errors
	  #:nfs-error

	  ;; server
	  #:start
	  #:stop

	  ;; client
	  #:with-nfs-mount
	  #:with-nfs-file

	  ;; client streams
	  #:nfs-file-stream
	  #:make-nfs-file-stream


	  ;; providers
	  #:nfs-provider
	  #:register-provider
	  #:unregister-provider
	  #:find-provider
	  #:provider-handle-fh
	  #:fh-provider-handle

	  ;; provider generic functions
	  #:nfs-provider-mount
	  #:nfs-provider-unmount
	  #:nfs-provider-attrs
	  #:nfs-provider-lookup
	  #:nfs-provider-access
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

	  ;; the simple (default) provdider
	  #:make-simple-provider
	  #:simple-provider
	  #:simple-provider-handles
	  #:simple-provider-clients
	  #:simple-provider-mount-handle

	  ))

