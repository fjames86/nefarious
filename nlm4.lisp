;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:nefarious.nlm4
  (:use #:cl #:frpc)
  (:nicknames #:nfs.nlm)
  (:export #:call-null
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
	   #:call-free-all))

(in-package #:nefarious.nlm4)

(defconstant +nlm4-program+ 100021)
(defconstant +nlm4-version+ 4)

(defconstant +max-netobj+ 1024)
(defxtype* netobj () (:varray* :octet +max-netobj+))

(use-rpc-program +nlm4-program+ +nlm4-version+)

(defxenum nlm4-stats 
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

(defxstruct nlm4-res ()
  ((cookie netobj)
   (stat nlm4-stats)))

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

(defxstruct nlm4-cancel-args ()
  ((cookie netobj)
   (block :boolean)
   (exclusive :boolean)
   (alock nlm4-lock)))

(defxstruct nlm4-test-args ()
  ((cookie netobj)
   (exclusive :boolean)
   (alock nlm4-lock)))

(defxstruct nlm4-test-res ()
  ((cookie netobj)
   (stat (:union nlm4-stats 
	   (:denied nlm4-holder)
	   (otherwise :void)))))

(defxstruct nlm4-unlock-args ()
  ((cookie netobj)
   (alock nlm4-lock)))

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

(defxstruct nlm4-share-args ()
  ((cookie netobj)
   (share nlm4-share)
   (reclaim :boolean)))

(defxstruct nlm4-share-res ()
  ((cookie netobj)
   (stat nlm4-stats)
   (seqno :int32)))

(defxstruct nlm4-notify ()
  ((name :string)
   (state :uint64)))

;; ----------------

(defrpc call-null 0 :void :void)
(defrpc call-test 1 nlm4-test-args ntlm-test-res)
(defrpc call-lock 2 nlm4-lock-args nlm4-res)
(defrpc call-cancel 3 nlm4-cancel-args nlm4-res)
(defrpc call-unlock 4 nlm4-unlock-args nlm4-res)
(defrpc call-granted 5 nlm4-test-args nlm4-res)
(defrpc call-test-msg 6 nlm4-test-args :void)
(defrpc call-lock-msg 7 nlm4-lock-args :void)
(defrpc call-cancel-msg 8 nlm4-cancel-args :void)
(defrpc call-unlock-msg 9 nlm4-unlock-args :void)
(defrpc call-granted-msg 10 nlm4-test-args :void)
(defrpc call-test-res 11 nlm4-test-res :void)
(defrpc call-lock-res 12 nlm4-res :void)
(defrpc call-cancel-res 13 nlm4-res :void)
(defrpc call-unlock-res 14 nlm4-res :void)
(defrpc call-granted-res 15 nlm4-res :void)
(defrpc call-share 20 nlm4-share-args nlm4-share-res)
(defrpc call-nm-lock 22 nlm4-lock-args nlm4-res)
(defrpc call-free-all 23 nlm4-notify :void)

