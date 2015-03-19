;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:nefarious)


;; constants
(defconstant +nfs-program+ 100003)
(defconstant +nfs-version+ 3)

(defconstant +nfs-fh-size+ 64)
(defconstant +nfs-cookie-verf-size+ 8)
(defconstant +nfs-create-verf-size+ 8)
(defconstant +nfs-write-verf-size+ 8)

(defparameter *nfs-port* 2049)

;; typedefs

(defxtype* filename3 () :string)
(defxtype* nfs-path3 () :string)
(defxtype* fileid3 () :uint64)
(defxtype* cookie3 () :uint64)

(defxtype* cookie-verf3 () (:array :octet +nfs-cookie-verf-size+))
(defun make-cookie-verf3 ()
  (nibbles:make-octet-vector +nfs-cookie-verf-size+))

(defxtype* create-verf3 () (:array :octet +nfs-create-verf-size+))
(defun make-create-verf3 ()
  (nibbles:make-octet-vector +nfs-create-verf-size+))

(defxtype* write-verf3 () (:array :octet +nfs-write-verf-size+))
(defun make-write-verf3 ()
  (nibbles:make-octet-vector +nfs-write-verf-size+))

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

(defxtype* nfs-fh3 () (:varray* :octet +nfs-fh-size+))

(defxstruct nfs-time3 ()
  ((seconds :uint32)
   (nseconds :uint32)))

(defxstruct fattr3 ()
  ((type ftype3 :reg)
   (mode mode3 0)
   (nlink :uint32)
   (uid uid3 0)
   (gid gid3 0)
   (size size3 0)
   (used size3 0)
   (rdev specdata3 (make-specdata3))
   (fsid :uint64)
   (fileid fileid3 0)
   (atime nfs-time3 (make-nfs-time3))
   (mtime nfs-time3 (make-nfs-time3))
   (ctime nfs-time3 (make-nfs-time3))))

(defxtype* post-op-attr () (:optional fattr3))

(defxstruct wcc-attr ()
  ((size size3 0)
   (mtime nfs-time3 (make-nfs-time3))
   (ctime nfs-time3 (make-nfs-time3))))

(defxtype* pre-op-attr () (:optional wcc-attr))

;; weak cache consistency data.
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
   (atime set-atime (make-xunion :dont-change nil))
   (mtime set-mtime (make-xunion :dont-change nil))))

(defxstruct dir-op-args3 ()
  ((dir nfs-fh3)
   (name filename3)))


(defun pack-mode (&key owner group others)
  (let ((m 0))
    (dolist (o owner)
      (setf m
            (logior m (ecase o
                        (:read #x00100)
                        (:write #x00080)
                        (:execute #x00040)))))
    (dolist (g group)
      (setf m 
            (logior m (ecase g 
                        (:read #x00020)
                        (:write #x00010)
                        (:execute #x00008)))))
    (dolist (o others)
      (setf m 
            (logior m (ecase o
                        (:read #x00004)
                        (:write #x00002)
                        (:execute #x00001)))))
    m))

(defun unpack-mode (mode)
  (list :owner (mapcan (lambda (name val)
                         (when (logand val mode)
                           (list name)))
                       '(:read :write :execute)
                       '(#x00100 #x00080 #x00040))
        :group (mapcan (lambda (name val)
                         (when (logand val mode)
                           (list name)))
                       '(:read :write :execute)
                       '(#x00020 #x00010 #x00008))
        :others (mapcan (lambda (name val)
                          (when (logand val mode)
                            (list name)))
                        '(:read :write :execute)
                        '(#x00004 #x00002 #x00001))))
