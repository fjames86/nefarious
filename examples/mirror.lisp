;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;; shows how you might implement a syncronously mirrored block device 

(defpackage #:nefarious.mirror
  (:use #:cl #:nefarious #:nefarious.block)
  (:export #:mirror-provider
           #:make-mirror-provider))

(in-package #:nefarious.mirror)

(defclass mirror-provider (block-provider)
  ((node :initform nil :initarg :remote-node :reader remote-node)))

(defun make-mirror-provider (mapping remote-node)
  ;; we should first mount the remote side so we can write to it 
  (nfs.mount:call-mount "/mirror" :host remote-node)
  ;; return the instance
  (make-instance 'mirror-provider 
                 :mappings (list mapping)
                 :remote-node remote-node))

;; we read locally, this is handled by the block provider 

;; writes are first syncronously mirrored to the remote side 
;; the block provider then writes locally 
(defmethod nfs-provider-write :before ((provider mirror-provider) handle offset bytes)
  ;; write to the remote side. obviously this could fail if the remote side is uncontactable 
  (call-write handle offset bytes :host (remote-node provider)))


;; basically all the other methods can be handled by the block provider






              
