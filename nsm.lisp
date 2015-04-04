;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The Network Status Monitor protocol. Used to send notifications
;;; to interested parties when we boot up, and to receive noticications
;;; from other hosts we are interested in.

;; Note: NSM needs to persist a seqno and a list of server names to send 
;; state notifications to. The default file is *default-nsm-pathspec*.


(defpackage #:nefarious.nsm
  (:use #:cl #:frpc)
  (:nicknames #:nsm)
  (:export #:register
           #:unregister
           #:*default-nsm-pathspec*
           #:load-nsm-state
           #:save-nsm-state 
           #:init-nsm

           ;; rpcs  
           #:call-null
           #:call-stat
           #:call-monitor
           #:call-unmonitor
           #:call-unmonitor-all
           #:call-notify))

(in-package #:nefarious.nsm)

;; ------------------------------------------

(defconstant +nsm-program+ 100024)
(defconstant +nsm-version+ 1)

(use-rpc-program +nsm-program+ +nsm-version+)
(use-rpc-host '*rpc-host* '*rpc-port*)

;; return status
(defxenum stat 
  (:success 0)
  (:fail 1))

;; combination of a return status and the state number
(defxtype* stat-res () (:list stat :int32)) ;; stat state

;; Callback info: ID of client to send notifications back to.
(defxstruct my-id ()
  (name :string) ;; hostname of client 
  (program :int32)
  (version :int32)
  (proc :int32))

;; -----------------------------------------

(defparameter *clients* nil
  "List of clients to notify when state change notifications are received.")

(defstruct client
  hostname ;; hostname of monitored machine 
  port ;; callback port -- if sending notifications via RPC
  id ;; my-id -- specifies to send notification via RPC
  function ;; specifies to send notifications via synronous callback
  private ;; client-provided data
  )

(defun register-client (hostname &key function id private)
  "Register to receive notifications on state changes of host named by HOSTNAME. To receive notifications, either supply a function of signature (hostname state private) or a MY-ID structure so receive notifications via RPC. If supplied, PRIVATE should be an array of 16 octets."
  (flet ((get-port (program version)
           (port-mapper:call-get-port program version 
                                      :query-protocol :udp 
                                      :protocol :udp
                                      :timeout nil
                                      :host hostname)))
    (let ((client (make-client :hostname hostname
                                :port (when id 
                                        (get-port (my-id-program id)
                                                  (my-id-version id)))
                                :id id
                                :function function
                                :private (or private (nibbles:make-octet-vector 16)))))
    (push client *clients*))))
                            
(defun unregister-client (hostname &key function id)
  "Stop receiving notifications of state changes for HOSTNAME. You should supply the same FUNCTION or program/version as in the REGISTER-CLIENT call."
  (setf *clients*
        (remove-if (lambda (client)
                     (when (string-equal (client-hostname client) hostname)
                       (or (and function 
                                (eq function (client-function client)))
                           (and id 
                                (= (my-id-program (client-id client)) (my-id-program id))
                                (= (my-id-version (client-id client)) (my-id-version id))))))
                   *clients*)))

;; -----------------------------------------

(defparameter *default-nsm-pathspec* (merge-pathnames "nsm.dat" (user-homedir-pathname)))

(defparameter *servers* nil
  "List of servers to send notifications to when our state changes.")

(defparameter *state* 0
  "The current state number.")

(defxstruct nsm-state ((:reader read-nsm-state) (:writer write-nsm-state))
  (state :int32 0)
  (servers (:varray :string)))

(defun save-nsm-state (&optional pathspec)
  "Save the local state number and list of notification servers to a data file."
  (with-open-file (f (or pathspec *default-nsm-pathspec*)
                     :direction :output 
                     :if-exists :supersede 
                     :element-type '(unsigned-byte 8))
    (write-nsm-state f 
		     (make-nsm-state :state *state*
				     :servers *servers*))))

(defun load-nsm-state (&optional pathspec)
  "Load the local state number and list of notification servers from the data file."
  (handler-case 
      (with-open-file (f (or pathspec *default-nsm-pathspec*)
                         :direction :input 
                         :if-does-not-exist :create 
                         :element-type '(unsigned-byte 8))
        (let ((state (read-nsm-state f)))
          (setf *state* (nsm-state-state state)
                *servers* (nsm-state-servers state))))
    (end-of-file ()
      (save-nsm-state pathspec)
      (load-nsm-state pathspec))))
      
(defun register-server (hostname)
  "Register a server to receive notifications when our state changes."
  (pushnew hostname *servers* :test #'string-equal)
  (save-nsm-state)
  nil)

(defun unregister-server (hostname)
  (setf *servers* (remove hostname *servers* :test #'string-equal))
  (save-nsm-state)
  nil)

(defun notify-servers ()
  "Tell all registered servers that our state has changed."
  (let ((local-hostname (machine-instance)))
    (dolist (server *servers*)
      (handler-case 
          (let ((port (port-mapper:call-get-port +nsm-program+ +nsm-version+
                                                 :query-protocol :udp
                                                 :host server
                                                 :protocol :udp)))
            (when port 
              (call-notify local-hostname *state*
                           :host server
                           :port port
                           :protocol :udp
                           :timeout nil)))
        (error () nil)))))

(defun init-nsm (&optional pathspec)
  "Load and increment the state variable, then notify all servers."
  (load-nsm-state pathspec)
  (incf *state*)
  (save-nsm-state pathspec)
  (notify-servers))

;; ----------------------------------------

(defun register (hostname function &optional private)
  "Register to receive notifications on state change of the host named HOSTNAME. 

The FUNCTION will be executed with arguments (hostname state private)."
  (register-server hostname)
  (register-client hostname :function function :private private))

(defun unregister (hostname function)
  "Unregister for notifications of state changes from HOSTNAME."
  (unregister-server hostname)
  (unregister-client hostname :function function))

;; -----------------------------------------

(defun %handle-null (void)
  (declare (ignore void))
  nil)

(defrpc call-null 0 :void :void
  (:documentation "Test connectivity to the NSM service.")
  (:handler #'%handle-null))

;; ------------------------------------------

(defun %handle-stat (name)
  (declare (ignore name))
  (list :fail 0))

(defrpc call-stat 1 :string stat-res
  (:arg-transformer (hostname) hostname)
  (:transformer (res)
    (destructuring-bind (stat seqno) res
      (values seqno (eq stat :success))))
  (:documentation "Test whether the NSM service will monitor a given host. In many current implementations this procedure is not operative and always returns a :FAIL status. Returns (values state-seqno success-p)")
  (:handler #'%handle-stat))


;; -------------------------------------

(defun %handle-monitor (arg)
  (destructuring-bind (hostname id private) arg
    (register-server hostname)
    (register-client hostname :id id :private private))
  (list :success *state*))

(defrpc call-monitor 2 
  (:list :string my-id (:varray* :octet 16))
  stat-res
  (:arg-transformer (hostname &key (callback-hostname "localhost") (program 0) (version 0) (proc 0) private)
    (list (list hostname 
                (make-my-id :name callback-hostname
                            :program program
                            :version version
                            :proc proc))
          (or private (nibbles:make-octet-vector 16))))
  (:transformer (res)
    (destructuring-bind (stat seqno) res
      (values seqno (eq stat :success))))
  (:documentation "Initiates monitoring of the specified host. When the notifications of state changes are received from the host, the new state is stored and all registered clients are notified.")
  (:handler #'%handle-monitor))

;; -------------------------------------


(defun %handle-unmonitor (arg)
  (destructuring-bind (hostname id) arg
    (unregister-client hostname :id id))
  *state*)

(defrpc call-unmonitor 3 
  (:list :string my-id)
  :int32
  (:arg-transformer (hostname &key (callback-hostname "localhost") (program 0) (version 0) (proc 0))
    (list hostname (make-my-id :name callback-hostname
                               :program program
                               :version version
                               :proc proc)))
  (:documentation "No longer monitor the specified host. Notifications to the specified callback will no longer be sent. Returns the state of the local NSM.")
  (:handler #'%handle-unmonitor))

;; ----------------------------------------


(defun %handle-unmonitor-all (id)
  (declare (ignore id))
  (setf *clients* nil)
  *state*)

(defrpc call-unmonitor-all 4 my-id :int32
  (:arg-transformer (&key (callback-hostname "localhost") (program 0) (version 0) (proc 0))
    (make-my-id :name callback-hostname
                :program program
                :version version
                :proc proc))
  (:documentation "Stops monitoring all hosts for which monitoring was requested.")
  (:handler #'%handle-unmonitor-all))

;; -----------------------------------------

(defun %handle-simulate-crash (void)
  (declare (ignore void))
  (incf *state*)
  (save-nsm-state)
  (notify-servers)
  nil)

(defrpc call-simulate-crash 5 :void :void
  (:documentation "Simulate a crash. The NSM will release all its current state information and reinitialize itself, incrementing its state variable. It reads through its notify list and sends notifications.")
  (:handler #'%handle-simulate-crash))


;; -------------------------------------------

(defxtype* notify-arg ((:reader read-notify-arg) (:writer write-notify-arg))
  (:list :string 
         :int32
         (:varray* :octet 16)))

(defun send-notification (notify-arg hostname program version proc &optional port)
  "Send a notification to the specifed host/program/version/proc. If PORT is not supplied, the portmapper program ion the remote machine is queried."
  ;; if no port specified, then use port-mapper to find it
  (unless port
    (setf port 
	  (port-mapper:call-get-port program version 
				     :query-protocol :udp 
				     :protocol :udp
				     :host hostname)))

  ;; should now have a reply port, send the notification
  (call-rpc #'write-notify-arg 
            notify-arg
            nil
            :host hostname 
            :port port
            :program program
            :version version
            :proc proc
            :protocol :udp
            :timeout nil))

(defun %handle-notify (arg)
  (destructuring-bind (hostname state) arg
    (let ((local-hostname (machine-instance)))
      (dolist (client *clients*)
	(when (string-equal hostname (client-hostname client))
	  (cond
	    ((client-function client)
	     ;; a function was supplied, run it
	     (funcall (client-function client) 
		      hostname 
		      state 
		      (client-private client)))
	    ((client-id client)
	     ;; an RPC was specified, call it 
	     (let ((id (client-id client)))
	       (send-notification (list local-hostname *state* (client-private client))
				  (my-id-name id)
				  (my-id-program id)
				  (my-id-version id)
				  (my-id-proc id)
				  (client-port client)))))))))
  nil)

(defrpc call-notify 6
  (:list :string :int32)
  :void
  (:arg-transformer (name state) (list name state))
  (:documentation "This RPC is used to inform servers that our local state has changed. NAME should be the local hostname, STATE should be the local state number.")
  (:handler #'%handle-notify))



