;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Codes to implement the Network Lock Manager (NLM) protocol
;;; This is used to implement a form a file locking for NFS.
;;; Unfortunately NLM seems to be a very poorly documented protocol.
;;; 
;;; We can safely assume that the RPC server is singly-threaded, because 
;;; these handlers are only expected to be executed by the nefarious RPC 
;;; server. This makes it much easier to implement because we are ensured 
;;; to always be running in a single thread. We also assume we are the only 
;;; process on the local host machine to be accessing the files. Otherwise
;;; we'd have to use actual platform-specific syscalls (flock, LockFileEx) 
;;; to do the lockings. 

(defpackage #:nefarious.nlm
  (:use #:cl #:frpc)
  (:nicknames #:nlm)
  (:export #:call-null
;;	   #:call-test
	   #:call-lock
	   #:call-cancel
	   #:call-unlock
	   #:call-granted
;;	   #:call-test-msg
;;	   #:call-lock-msg
;;	   #:call-cancel-msg
;;	   #:call-unlock-msg
;;	   #:call-granted-msg
;;	   #:call-test-res
;;	   #:call-lock-res
;;	   #:call-cancel-res
;;	   #:call-unlock-res
;;	   #:call-granted-res
;;	   #:call-share
;;	   #:call-nm-lock
;;	   #:call-free-all
	   
	   ))


(in-package #:nefarious.nlm)

(use-rpc-program 100021 4)
(use-rpc-host '*rpc-host* '*rpc-port*)


;; -------------------------

(defparameter *locks* nil)

(defstruct lock 
  cookie 
  fh 
  offset 
  len
  exclusive)

(defun find-lock (fh)
  (find fh *locks* :key #'lock-fh :test #'equalp))

(defun acquire-lock (fh &key cookie offset len exclusive)
  (let ((lock (find-lock fh)))
    (cond
      ((not lock)
       ;; no lock held on that file 
       (let ((l (make-lock :cookie cookie
			   :fh fh
			   :offset offset
			   :len len
			   :exclusive exclusive)))
	 (push l *locks*)
	 l))
      ((not (lock-exclusive lock))
       ;; a lock is held, but it's not exclusive
       lock)
      (t 
       ;; cannot grant a lock 
       nil))))

(defun release-lock (fh)
  (setf *locks* (remove fh *locks* :key #'lock-fh :test #'equalp)))

		       
;; -------------------------


(defconstant +max-netobj+ 1024)
(defxtype* netobj () (:varray* :octet +max-netobj+))
(defun make-netobj ()
  (nibbles:make-octet-vector +max-netobj+))

(defxenum nlm4-stat
  ((:granted 0)
   (:denied 1)
   (:denied-no-locks 2)
   (:blocked 3)
   (:denied-grace-period 4)
   (:deadlock 5)
   (:rofs 6)
   (:stale-fh 7)
   (:fbig 8)
   (:failed 9)))

(defxenum fsh-mode 
  ((:deny-none 0)
   (:deny-read 1)
   (:deny-write 2)
   (:deny-rw 3)))

(defxenum fsh-access 
  ((:none 0)
   (:read-only 1)
   (:write-only 2)
   (:read-write 3)))

(defxtype* nlm4-res ()
  (:list netobj nlm4-stat)) ;; cookie stat

(defxstruct nlm4-holder ()
  ((exclusive :boolean)
   (svid :int32)
   (oh netobj)
   (offset :uint64)
   (len :uint64)))

(defxstruct nlm4-lock ()
  ((name :string)
   (fh netobj)
   (oh netobj)
   (uppid :int32)
   (offset :uint64)
   (len :uint64)))

(defxstruct nlm4-share ()
  ((name :string)
   (fh netobj)
   (oh netobj)
   (mode fsh4-mode)
   (access fsh4-mode)))

(defxstruct nlm4-lock-args ()
  ((cookie netobj)
   (block :boolean)
   (exclusive :boolean)
   (alock nlm4-lock)
   (reclaim :boolean)
   (state :int32)))

(defxtype* nlm4-cancel-args ()
  (:list netobj :boolean :boolean nlm4-lock)) ;; cookie block exclusive alock

(defxtype* nlm4-test-args ()
  (:list netobj :boolean nlm4-lock)) ;; cookie exclusive alock

(defxtype* nlm4-test-res () ;; cookie stat
  (:list netobj
         (:union nlm4-stat
           (:denied nlm4-holder)
           (otherwise :void))))

(defxtype* nlm4-unlock-args ()
  (:list netobj nlm4-lock)) ;; cookie alock

(defxtype* nlm4-share-args ()
  (:list netobj nlm4-share :boolean)) ;; cookie share reclaim

(defxtype* nlm4-share-res ()
  (:list netobj nlm4-stat :int32)) ;; cookie stat seqno

(defxtype* nlm4-notify ()
  (:list :string :uint64)) ;; name state

;; ----------------

;; there is very little information on how to implement these
;; I can't find any RFC defining them, only the NFSv3 RFC seems to mention
;; them and it only says that they are the same as in the NFSv2 version
;; but NFSv2 RFC doesn't mention them at all.
;; see the linux kernel sources for examples of how to implenent them:
;; http://git.kernel.org/cgit/linux/kernel/git/stable/linux-stable.git/tree/fs/lockd/svcproc.c
;; There is also a page on the Wireshark wiki that has some useful information


;; syncronous procedures 

;; --------------------------------------

;; NULL 

(defrpc call-null 0 :void :void
  (:documentation "Test connectivity to the NLM server."))
(defhandler %handle-null (void 0)
  (declare (ignore void))
  nil)

;; --------------------------------------

;; TEST ::  test whether a lock would be granted 

;;(defrpc call-test 1 nlm4-test-args ntlm-test-res
;;  (:documentation "Check for conflicting lock. 
;;
;;Most NLM servers do not implement this function, and most clients never use it, so it is not implemented here."))

;;(defhandler %handle-test (args 1)
;;  (destructuring-bind (cookie exclusive alock) args
;;    (declare (ignore exclusive alock))
;;    (list cookie (make-xunion :granted nil))))

;; ----------------------------------------

;; LOCK :: acquire a lock. If the lock is currently held elsewhere 
;; then a :BLOCKED response should be sent. Once the lock has become available
;; the server should sent a GRANTED message to the client, informing it that the 
;; lock has been granted to the client.

(defrpc call-lock 2 nlm4-lock-args nlm4-res
  (:documentation "Lock the specified file."))

(defhandler %handle-lock (args 2)
  (with-slots (cookie exclusive alock) args
    (let ((lock (acquire-lock (nlm4-lock-fh alock)
			      :cookie cookie
			      :exclusive exclusive)))
      (if lock 
	  (list cookie :granted)
	  (list cookie :blocked)))))

;; -----------------------------------------

;; CANCEL :: cancel a lock. This is largely the same as the UNLOCK
;; procedure, and most NLM servers just call that function here. 
;; Semantically they are not the same, because CANCEL is typically 
;; used by the system to clean up clients that didn't exit properly.

(defrpc call-cancel 3 nlm4-cancel-args nlm4-res
  (:documentation "Cancel a previously blocked request."))

(defhandler %handle-cancel (args 3)
  (destructuring-bind (cookie block exclusive alock) args
    (declare (ignore block exclusive))
    (release-lock (nlm4-lock-fh alock))
    (list cookie :granted)))

;; ------------------------------------------

;; UNLOCK :: release a lock 

(defrpc call-unlock 4 nlm4-unlock-args nlm4-res
  (:documentation "Release a lock."))

(defhandler %handle-unlock (args 4)
  (destructuring-bind (cookie alock) args
    (release-lock (nlm4-lock-fh alock))
    (list cookie :granted)))

;; --------------------------------------------

;; GRANTED :: message sent from the NLM server to 
;; the client, informing it that it has now acquired the lock

;; server NLM callback procedure to grant lock
(defrpc call-granted 5 nlm4-test-args nlm4-res)

(defhandler %handle-granted (args 5)
  (destructuring-bind (cookie exclusive alock) args
    (declare (ignore exclusive alock))
    (list cookie :granted)))

;; -----------------------------------------


;; asyncronous requests and responses
;; should send the async replies in the body of the handler
;; i.e. before the original request was sent. this is the same 
;; thing that the linux kernel does so it should be ok 
;;(defrpc call-test-msg 6 nlm4-test-args :void)
;;(defrpc call-lock-msg 7 nlm4-lock-args :void)
;;(defrpc call-cancel-msg 8 nlm4-cancel-args :void)
;;(defrpc call-unlock-msg 9 nlm4-unlock-args :void)
;;(defrpc call-granted-msg 10 nlm4-test-args :void)
;;(defrpc call-test-res 11 nlm4-test-res :void)
;;(defrpc call-lock-res 12 nlm4-res :void)
;;(defrpc call-cancel-res 13 nlm4-res :void)
;;(defrpc call-unlock-res 14 nlm4-res :void)
;;(defrpc call-granted-res 15 nlm4-res :void)

;; syncronous non-monitored lock and DOS file-sharing procedures 
;; don't support these
;;(defrpc call-share 20 nlm4-share-args nlm4-share-res)
;;(defrpc call-unshare 21 nlm4-share-args nlm4-share-res)
;;(defrpc call-nm-lock 22 nlm4-lock-args nlm4-res)
;;(defrpc call-free-all 23 nlm4-notify :void)

