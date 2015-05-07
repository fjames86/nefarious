;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


;;; This shows how to mirror local changes up to a remote NFS server, i.e. we 
;;; are modifying files locally, and wish to push the changes out to a remote NFS so that 
;;; the local changes are reflected there.
;;; This is useful because it means you can "share" files without special treatment from 
;;; your host OS.
;;; A potential usecase might be to aid interactive development with a remote Lisp system.

(defpackage #:nefarious.upload 
  (:use #:cl #:nefarious #:frpc))

(in-package #:nefarious.upload)

;; want to keep track of the state of files and only push the parts that have changed
;; do this by subdividing the files up into blocks of 4k each and computing a hash on each block
;; if the hash changes then the block is pushed out  

(defstruct fblock
  offset 
  size
  hash)
  
(defstruct file 
  name
  size
  blocks)

(defstruct monitor 
  directory 
  files)

(defun hash (array &key (start 0) end)
  (do ((h 0 (mod h (expt 2 32)))
       (i start (1+ i)))
      ((= i (or end (length array)))
       (progn (incf h (ash h 3))
              (setf h (logxor (ash h -11)))
              (incf h (ash h 15))
              (mod h (expt 2 32))))
    (incf h (aref array i))
    (setf h (ash h 10))
    (setf h (logxor h (ash h -6)))))

(defun file-blocks (pathspec)
  (with-open-file (f pathspec :direction :input :element-type '(unsigned-byte 8))
    (let ((size (file-length f))
          (buffer (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
      (do ((offset 0 (+ offset 4096))
           (blocks nil))
          ((>= offset size) (nreverse blocks))
        (let ((n (read-sequence buffer f)))
          (push (make-fblock :offset offset
                            :size n
                            :hash (hash buffer :start 0 :end n))
                blocks))))))


;; (defun push-diffs (client pathspec blocks)
;;   (let ((new-blocks (file-blocks pathspec)))
;;     (do ((old-blocks blocks (cdr old-blocks))
;;          (new-blocks new-blocks (cdr new-blocks)))
;;         ((and (null old-blocks) (null new-blocks)))
;;       (let ((old (car old-blocks))
;;             (new (car new-blocks)))
;;         (cond
;;           ((and old new
;;                 (= (fblock-offset old) (fblock-offset new))
;;                 (= (fblock-size old) (fblock-size new))
;;                 (= (fblock-hash old) (fblock-hash new)))
;;            ;; the same, do nothing
;;            )
;;           (t 
;;            ;; somethnig different, push the block out 
;;            (
