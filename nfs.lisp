
(in-package #:nefarious)


;; constants
(defconstant +nfs-program+ 100003)
(defconstant +nfs-version+ 3)

(defconstant +nfs-fh-size+ 64)
(defconstant +nfs-cookie-verf-size+ 8)
(defconstant +nfs-create-verf-size+ 8)
(defconstant +nfs-write-verf-size+ 8)


;; typedefs

(defxtype* filename3 ()
  (:varray :string))

(defxtype* nfs-path3 ()
  (:varray :string))

(defxtype* file-id3 () :uint64)

(defxtype* cookie3 () :uint64)

(defxtype* cookied-verf3 () 
  (:array :octet +nfs-cookie-verf-size+))

(defxtype* create-verf3 ()
  (:array :octet +nfs-create-verf-size+))

(defxtype* write-verf3 ()
  (:array :octet +nfs-write-verf-size+))

(defxtype* uid3 () :uint32)
(defxtype* gid3 () :uint32)
(defxtype* size3 () :uint64)   
(defxtype* offset3 () :uint64)
(defxtype* mode3 () :uint32)
(defxtype* count3 () :uint32)

(defxenum nfs-stat3 
  ((:ok 0)
  (:perm 1)
  (:noent 2)
  (:io 5)
  (:nxio 6)
  (:access 13)
  (:exist 17)
  (:xdev 18)
  (:nodev 19)
  (:notdir 20)
  (:isdir 21)
  (:inval 22)
  (:fbig 27)
  (:nospc 28)
  (:rofs 30)
  (:mlink 31)
  (:name-too-long 63)
  (:not-empty 66)
  (:dquot 69)
  (:stale 70)
  (:remote 71)
  (:bad-handle 10001)
  (:not-sync 10002)
  (:bad-cookie 10003)
  (:not-supp 10004)
  (:too-small 10005)
  (:server-fault 10006)
  (:bad-type 10007)
  (:juke-box 10008)))

(defxenum ftype3 
  ((:reg 1)
  (:dir 2)
  (:blk 3)
  (:chr 4)
  (:lnk 5)
  (:sock 6)
  (:fifo 7)))

(defxstruct specdata3 ()
  ((data1 :uint32)
   (data2 :uint32)))

(defxtype* nfs-fh3 () (:varray :octet +nfs-fh-size+))

(defxstruct nfs-time3 ()
  ((seconds :uint32)
   (nseconds :uint32)))

(defxstruct fattr3 ()
  ((type ftype3)
   (mode mode3)
   (nlink :uint32)
   (uid uid3)
   (gid gid3)
   (size size3)
   (used size3)
   (rdev specdata3)
   (fsid :uint64)
   (fileid fileid3)
   (atime nfs-time3)
   (mtime nfs-time3)
   (ctime nfs-time3)))

(defxtype* post-op-attr ()
  (:optional fattr3))

(defxstruct wcc-attr ()
  ((size size3)
   (mtime nfs-time3)
   (ctime nfs-time3)))

(defxtype* pre-op-attr () (:optional wcc-attr))

(defxstruct wcc-data ()
  ((before pre-op-attr)
   (after post-op-attr)))

(defxtype* post-op-fh3 () (:optional nfs-fh3))

(defxenum time-how 
  ((:dont-change 0)
   (:set-to-server-time 1)
   (:set-to-client-time 2)))

(defxtype* set-mode3 () (:optional mode3))

(defxtype* set-uid3 () (:optional uid3))

(defxtype* set-gid3 () (:optional gid3))

(defxtype* set-size3 () (:optional size3))

(defxunion set-atime (time-how)
  ((:set-to-client-time nfs-time3) 
   (otherwise :void)))

(defxunion set-mtime (time-how)
  ((:set-to-client-time nfs-time3)
   (otherwise :void)))

(defxstruct sattr3 ()
  ((mode set-mode3)
   (uid set-uid3)
   (gid set-gid3)
   (size set-size3)
   (atime set-atime)
   (mtime set-mtime)))

(defxstruct dir-op-args3 ()
  ((dir nfs-fh3)
   (name filename3)))


