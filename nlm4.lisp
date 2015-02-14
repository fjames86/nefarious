
(defpackage #:nefarious.nlm4
  (:use #:cl #:frpc)
  (:export #:call-nlm4-null
	   #:call-nlm4-test
	   #:call-nlm4-lock
	   #:call-nlm4-cancel
	   #:call-nlm4-unlock
	   #:call-nlm4-granted
	   #:call-nlm4-test-msg
	   #:call-nlm4-lock-msg
	   #:call-nlm4-cancel-msg
	   #:call-nlm4-unlock-msg
	   #:call-nlm4-granted-msg
	   #:call-nlm4-test-res
	   #:call-nlm4-lock-res
	   #:call-nlm4-cancel-res
	   #:call-nlm4-unlock-res
	   #:call-nlm4-granted-res
	   #:call-nlm4-share
	   #:call-nlm4-nm-lock
	   #:call-nlm4-free-all))

(in-package #:nefarious.nlm4)

(defconstant +locks-program+ 100021)
(defconstant +locks-version+ 4)

(use-rpc-program +locks-program+)
(use-rpc-version +locks-version+)

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
   (svid :int32)
   (offset :uint64)
   (len :uint64)))

(defxstruct nlm4-share ()
  ((name :string)
   (fh netobj)
   (oh netobj)
   (mode fsh4-mode)
   (access fsh4-mode)))

;; not all the structures are defined in the spec-- wtf are they?

(defrpc call-nlm4-null (:void :void) 0)
(defrpc call-nlm4-test (nlm4-test-args ntlm-test-res) 1)
(defrpc call-nlm4-lock (nlm4-lock-args nlm4-res) 2)
(defrpc call-nlm4-cancel (nlm4-cancel-args nlm4-res) 3)
(defrpc call-nlm4-unlock (nlm4-unlock-args nlm4-res) 4)
(defrpc call-nlm4-granted (nlm4-test-args nlm4-res) 5)
(defrpc call-nlm4-test-msg (nlm4-test-args :void) 6)
(defrpc call-nlm4-lock-msg (nlm4-lock-args :void) 7)
(defrpc call-nlm4-cancel-msg (nlm4-cancel-args :void) 8)
(defrpc call-nlm4-unlock-msg (nlm4-unlock-args :void) 9)
(defrpc call-nlm4-granted-msg (nlm4-test-args :void) 10)
(defrpc call-nlm4-test-res (nlm4-test-res :void) 11)
(defrpc call-nlm4-lock-res (nlm4-res :void) 12)
(defrpc call-nlm4-cancel-res (nlm4-res :void) 13)
(defrpc call-nlm4-unlock-res (nlm4-res :void) 14)
(defrpc call-nlm4-granted-res (nlm4-res :void) 15)
(defrpc call-nlm4-share (nlm4-share-args nlm4-share-res) 20)
(defrpc call-nlm4-nm-lock (nlm4-lock-args nlm4-res) 22)
(defrpc call-nlm4-free-all (nlm4-notify :void) 23)

