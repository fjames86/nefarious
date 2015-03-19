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
  (:export #:make-nlm-lock
           #:make-nlm-share
           
           ;; rpcs 
           #:call-null
           #:call-test
           #:call-lock
           #:call-cancel
           #:call-unlock
           #:call-granted
           #:call-test-msg
           #:call-lock-msg
           #:call-cancel-msg
           #:call-unlock-msg
           #:call-granted-msg
           #:call-test-res
           #:call-lock-res
           #:call-cancel-res
           #:call-unlock-res
           #:call-granted-res
           #:call-share
           #:call-nm-lock
           #:call-free-all
	   
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

(defxenum nlm-stat
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

(defxstruct nlm-lock ()
  ((name :string)
   (fh netobj)
   (oh netobj)
   (uppid :int32)
   (offset :uint64)
   (len :uint64)))

(defxstruct nlm-holder ()
  ((exclusive :boolean)
   (id :int32)
   (oh netobj)
   (offset :uint64)
   (len :uint64)))

(defxstruct nlm-share ()
  ((name :string)
   (fh netobj)
   (oh netobj)
   (mode fsh4-mode)
   (access fsh4-mode)))

;; --------------------------------

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

(defrpc call-test 1 
  (:list netobj :boolean nlm-lock)
  (:list netobj 
         (:union nlm-stat 
           (:denied nlm-holder)
           (otherwise :void)))
  (:arg-transformer (lock &key cookie exclusive)
    (list cookie exclusive lock))
  (:documentation "Test whether the monitored lock is available to the client."))

;;(defhandler %handle-test (args 1)
;;  (destructuring-bind (cookie exclusive alock) args
;;    (declare (ignore exclusive alock))
;;    (list cookie (make-xunion :granted nil))))

;; ----------------------------------------

;; LOCK :: acquire a lock. If the lock is currently held elsewhere 
;; then a :BLOCKED response should be sent. Once the lock has become available
;; the server should sent a GRANTED message to the client, informing it that the 
;; lock has been granted to the client.

(defrpc call-lock 2 
  (:list netobj :boolean :boolean nlm-lock :boolean :int32)
  (:list netobj nlm-stat)
  (:arg-transformer (lock &key cookie block exclusive reclaim (state 0))
    (list cookie block exclusive lock reclaim state))
  (:documentation "Attempt to establish a monitored lock. If BLOCK is non-nil, then if the lock request cannot bec granted immediately the server will return a status of :BLOCKED. When the request can be granted, trhe server will make a callback to the client with the NLM-GRANTED procedure call. If BLOCK is nil and the lock cannot be granted immediately, then the call returns a status of :DENIED.

If RECLAIM is true, the server will assume this is an attempt to re-establish a previous lock (for instance, after a server crash). During the grace-period, the server will only accept locks with RECLAIM of true.

STATE contains the state of the client's NSM. This information is kept by the server implementation, so if the client crashes the server can determine which locks should be discarded by checking the state against the NSM crash notification sent by NSM."))

(defhandler %handle-lock (args 2)
  (with-slots (cookie exclusive alock) args
    (let ((lock (acquire-lock (nlm-lock-fh alock)
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

(defrpc call-cancel 3 
  (:list netobj :boolean :boolean nlm-lock)
  (:list netobj nlm-stat)
  (:arg-transformer (lock &key cookie block exclusive)
    (list cookie block exclusive lock))
  (:documentation "Cancels an outstanding blocked lock request. If the client made a LOCK procedure call with BLOCKED true and the procedure was blocked by the server (it returned a status of :BLOCKED), then the client can cancel the outstanding lock request by using this procedure.

The BLOCK, EXCLUSIVE and LOCK arguments must exactly match those in the corresponding LOCK request."))

(defhandler %handle-cancel (args 3)
  (destructuring-bind (cookie block exclusive alock) args
    (declare (ignore block exclusive))
    (release-lock (nlm-lock-fh alock))
    (list cookie :granted)))

;; ------------------------------------------

;; UNLOCK :: release a lock 

(defrpc call-unlock 4 
  (:list netobj nlm-lock)
  (:list netobj nlm-stat)
  (:arg-transformer (lock &key cookie)
    (list cookie lock))
  (:documentation "Release a lock. The information in LOCK should match the information in the LOCK which created the lock."))

(defhandler %handle-unlock (args 4)
  (destructuring-bind (cookie alock) args
    (release-lock (nlm-lock-fh alock))
    (list cookie :granted)))

;; --------------------------------------------

;; GRANTED :: message sent from the NLM server to 
;; the client, informing it that it has now acquired the lock

;; server NLM callback procedure to grant lock
(defrpc call-granted 5 
  (:list netobj :boolean nlm-lock)
  (:list netobj nlm-stat)
  (:arg-transformer (lock &key cookie exclusive)
    (list cookie exclusive lock))
  (:documentation "This procedure is the callback from the server running NLM to the host that requested a lock that could not be immediately honoured."))

(defhandler %handle-granted (args 5)
  (destructuring-bind (cookie exclusive alock) args
    (declare (ignore exclusive alock))
    (list cookie :granted)))

;; -----------------------------------------


;; asyncronous requests and responses
;; should send the async replies in the body of the handler
;; i.e. before the original request was sent. this is the same 
;; thing that the linux kernel does so it should be ok 


(defrpc call-test-msg 6 
  (:list netobj :boolean nlm-lock)
  :void
  (:arg-transformer (lock &key cookie exclusive)
    (list cookie exclusive lock))
  (:documentation "The async version of TEST. Results are returned asyncronously by a call from TEST-RES."))

(defrpc call-lock-msg 7 
  (:list netobj :boolean :boolean nlm-lock :boolean :int32)
  :void
  (:arg-transformer (lock &key cookie block exclusive reclaim (state 0))
    (list cookie block exclusive lock reclaim state))
  (:documentation "Results returned asyncronously by a call from LOCK-RES."))

(defrpc call-cancel-msg 8 
  (:list netobj :boolean :boolean nlm-lock)
  :void
  (:arg-transformer (lock &key cookie block exclusive)
    (list cookie block exclusive lock))
  (:documentation "Asyncronous version of CANCEL."))

(defrpc call-unlock-msg 9 
  (:list netobj nlm-lock)
  :void
  (:arg-transformer (lock &key cookie)
    (list cookie lock))
  (:documentation "Asyncronous version of UNLOCK."))

(defrpc call-granted-msg 10
  (:list netobj :boolean nlm-lock)
  :void
  (:arg-transformer (lock &key cookie exclusive)
    (list cookie exclusive lock))
  (:documentation "Asyncronous version of GRANTED."))

(defrpc call-test-res 11 
  (:list netobj 
         (:union nlm-stat 
           (:denied nlm-holder)
           (otherwise :void)))
  :void
  (:arg-transformer (stat cookie &key holder)
    (list cookie
          (make-xunion stat
                       (if (eq stat :denied)
                           holder
                           nil))))
  (:documentation "Asyncronous reply to TEST-MSG."))

(defrpc call-lock-res 12
  (:list netobj nlm-stat)
  :void
  (:arg-transformer (stat cookie)
    (list cookie stat))
  (:documentation "asyncronous reply to LOCK-MSG."))

(defrpc call-cancel-res 13
  (:list netobj nlm-stat)
  :void
  (:arg-transformer (stat cookie) (list cookie stat))
  (:documentation "Asyncronous reply to CANCEL-MSG."))

(defrpc call-unlock-res 14
  (:list netobj nlm-stat)
  :void
  (:arg-transformer (stat cookie) (list cookie stat))
  (:documentation "asyncronous reply to UNLOCK-MSG."))

(defrpc call-granted-res 15 
  (:list netobj nlm-stat)
  :void
  (:arg-transformer (stat cookie) (list cookie stat))
  (:documentation "Asyncronous reply to GRANTED-MSG."))

;; syncronous non-monitored lock and DOS file-sharing procedures 
;; don't support these
(defrpc call-share 20 
  (:list netobj nlm-share :boolean)
  (:list netobj nlm-stat :int32)
  (:arg-transformer (share &key cookie reclaim)
    (list cookie share reclaim))
  (:documentation "Indicates that a client wishes to open a file using DOS 3.1 and above filesharing modes. 

This procedure does not block: it is the responsibility of the client to retry any failed requests."))

(defrpc call-unshare 21 
  (:list netobj nlm-share :boolean)
  (:list netobj nlm-stat :int32)
  (:arg-transformer (share &key cookie)
    (list cookie share nil))
  (:documentation "This procedure informs NLM that the client has closed the file named by the share, and the corresponding share reservation should be released. The reclaim field is unused in the procedure and should be ignored. It is included for symmetry with the NLM-SHARE procedure."))

(defrpc call-nm-lock 22 
  (:list netobj :boolean :boolean nlm-lock :boolean :int32)
  (:list netobj nlm-stat)
  (:arg-transformer (lock &key cookie block exclusive reclaim (state 0))
    (list cookie block exclusive lock reclaim state))
  (:documentation "This proceudre should only be called by clients that do not run NSM. This procedure has the same functionality as the LOCK procedure except there is no monitoring from NSM. Locks created with this procedure should be released with the normal UNLOCK procedure.

Because NLM has no way to detect client crashes while locks are in effect, the client should call FREE-ALL to release all the locks it may have held before the crash."))

(defrpc call-free-all 23 
  (:list :string :uint32)
  :void
  (:arg-transformer (name) (list name 0))
  (:documentation "This procedure informs the server that the client NAME has rebooted and that all file-sharing reservations and locks held by the client should be discarded."))



