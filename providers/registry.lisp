;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:nefarious.providers.registry
  (:use #:cl #:nefarious #:cffi))

(in-package #:nefarious.providers.registry)

(define-foreign-library advapi
  (:windows "Advapi32.dll"))

(use-foreign-library advapi)

;; for errors
(defcfun (%format-message "FormatMessageA" :convention :stdcall)
    :uint32
  (flags :uint32)
  (source :pointer)
  (msg-id :uint32)
  (lang-id :uint32)
  (buffer :pointer)
  (size :uint32)
  (args :pointer))

(defun format-message (code)
  (with-foreign-object (buffer :char 1024)
    (let ((n (%format-message #x00001000
			      (null-pointer)
			      code
			      0
			      buffer
			      1024
			      (null-pointer))))
      (if (= n 0)
	  (error "Failed to format message")
	  (foreign-string-to-lisp buffer :count (- n 2))))))

(define-condition win-error (error)
  ((code :initform 0 :initarg :code :reader win-error-code))
  (:report (lambda (condition stream)
	     (format stream "ERROR ~A: ~A" 
		     (win-error-code condition)
		     (format-message (win-error-code condition))))))
	   
		   

(defctype hkey :uint32)


(defcfun (%reg-open-key "RegOpenKeyExA" :convention :stdcall)
    :uint32
  (key hkey)
  (name :string)
  (options :uint32)
  (desired :uint32)
  (result :pointer))

(defparameter *hkey-trees* 
  '((:classes-root . 2147483648) 
    (:current-user . 2147483649) 
    (:local-machine . 2147483650) 
    (:users . 2147483651) 
    (:current-config . 2147483653)))

(defconstant +desire-all-access+ #xf003f)

(defun reg-open-key (name &key key (options 0) (desired +desire-all-access+))
  (with-foreign-object (handle 'hkey)
    (let ((sts (%reg-open-key (cond
				((integerp key) key)
				((keywordp key)
				 (cdr (assoc key *hkey-trees*)))
				(t (error "Invalid key")))
			      name 
			      options 
			      desired
			      handle)))
      (if (zerop sts)
	  (mem-ref handle 'hkey)
	  (error 'win-error :code sts)))))

(defcfun (reg-close-key "RegCloseKey" :convention :stdcall)
    :long
  (key hkey))


(defcfun (%reg-create-key "RegCreateKeyExA" :convention :stdcall)
    :long
  (key hkey)
  (name :string)
  (reserved :uint32)
  (class :string)
  (options :uint32)
  (desired :uint32)
  (attributes :pointer)
  (result :pointer)
  (disposition :pointer))

(defun reg-create-key (name &key key (options 0) (desired +desire-all-access+))
  (with-foreign-object (handle 'hkey)
    (let ((res (%reg-create-key (or key (cdr (assoc key *hkey-trees*)))
				name 
				0
				(null-pointer)
				options
				desired
				(null-pointer)
				handle
				(null-pointer))))
      (if (= res 0)
	  (mem-ref handle 'hkey)
	  (error 'win-error :code res)))))

(defcfun (%reg-enum-key "RegEnumKeyExA" :convention :stdcall)
    :long
  (key hkey)
  (index :uint32)
  (name-buffer :pointer)
  (name-size :pointer)
  (reserved :pointer)
  (class :pointer)
  (class-size :pointer)
  (last-write :pointer))

(defun reg-enum-key (key)
  (with-foreign-object (buffer :char 1024)
    (with-foreign-object (size :uint32)
      (do ((i 0 (1+ i))
	   (names nil)
	   (done nil))
	  (done names)
	(setf (mem-ref size :uint32) 1024)
	(let ((res (%reg-enum-key key 
				  i
				  buffer
				  size
				  (null-pointer)
				  (null-pointer)
				  (null-pointer)
				  (null-pointer))))
	  (if (= res 0)
	      (push (foreign-string-to-lisp buffer :count (mem-ref size :uint32))
		    names)
	      (setf done t)))))))

				  

(defcfun (%reg-enum-value "RegEnumValueA" :convention :stdcall)
    :long
  (key hkey)
  (index :uint32)
  (name :pointer)
  (size :pointer)
  (reserved :pointer)
  (type :pointer)
  (data :pointer)
  (data-size :pointer))

(defparameter *reg-types*
  '((:string 1)
    (:expand-string 2)
    (:binary 3)
    (:dword 4)
    (:multi-string 7)))

(defun reg-enum-value (key)
  (with-foreign-objects ((name-buffer :char 1024)
			 (size :uint32)
			 (data :char 1024)
			 (data-size :uint32)
			 (type :uint32))
    (do ((i 0 (1+ i))
	 (vals nil)
	 (done nil))
	(done vals)
      (setf (mem-ref size :uint32) 1024
	    (mem-ref data-size :uint32) 1024)
      (let ((res (%reg-enum-value key 
				  i
				  name-buffer
				  size
				  (null-pointer)
				  type
				  data
				  data-size)))
	(if (= res 0)
	    (push 
	     (let ((vec (make-array (mem-ref data-size :uint32)
				    :element-type '(unsigned-byte 8))))
	       (dotimes (i (mem-ref data-size :uint32))
		 (setf (aref vec i) (mem-ref data :uint8 i)))
	       (list (foreign-string-to-lisp name-buffer 
					     :count (mem-ref size :uint32))
		     vec 
		     (first 
		      (find (mem-ref type :uint32)
			    *reg-types*
			    :key #'second))))
	     vals)
	    (setf done t))))))

(defcfun (%reg-set-value "RegSetValueExA" :convention :stdcall)
    :long
  (key hkey)
  (name :pointer)
  (reserved :uint32)
  (type :uint32)
  (data :pointer)
  (size :uint32))

(defun reg-set-value (key name data type)
  (let ((length (length data)))
    (with-foreign-object (buffer :uint8 length)
      (with-foreign-string (nstr name)
	(dotimes (i length)
	  (setf (mem-ref buffer :uint8 i)
		(aref data i)))
	(let ((res (%reg-set-value key 
				   nstr
				   0
				   (second 
				    (find type *reg-types*
					  :key #'first))
				   buffer
				   length)))
	  (if (= res 0)
	      nil
	      (error 'win-error :code res)))))))

(defcfun (%reg-delete-value "RegDeleteValueA" :convention :stdcall)
   :long
  (key hkey)
  (name :string))

(defun reg-delete-value (key name)
  (with-foreign-string (nstr name)
    (let ((res (%reg-delete-value key nstr)))
      (if (= res 0)
	  nil
	  (error 'win-error :code res)))))


(defmacro with-reg-key ((var name &key key) &body body)
  `(let ((,var (reg-open-key ,name :key ,key)))
     (unwind-protect (progn ,@body)
       (reg-close-key ,var))))


;; ---------------------------------


;; the provider class
(defclass registry-provider (nfs-provider)
  ((handles :initform (make-hash-table :test #'equalp) :reader provider-handles)
   (clients :initform nil :accessor provider-clients)
   (mount-handle :initarg :mount-handle :reader provider-mount-handle)))


