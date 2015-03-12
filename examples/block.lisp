

(defpackage #:nefarious.block
  (:use #:cl #:frpc #:nefarious #:cffi)
  (:nicknames #:nfs.block))

(in-package #:nefarious.block)

(defcfun (%create-file "CreateFileA" :convention :stdcall)
    :pointer 
  (filename :string)
  (access :uint32)
  (mode :uint32)
  (attrs :pointer)
  (disposition :uint32)
  (flags :uint32)
  (template :pointer))

;; access 
;;#define GENERIC_READ                     (0x80000000L)
;;#define GENERIC_WRITE                    (0x40000000L)
;;#define GENERIC_EXECUTE                  (0x20000000L)
;;#define GENERIC_ALL                      (0x10000000L)

;; mode
;; exclusive #x0
;; FILE_SHARE_READ #x01
;; FILE_SHARE_WRITER #x02

;; disposition
;; CREATE_ALWAYS 2
;; CREATE_NEW 1
;; OPEN_ALWAYS 4
;; OPEN_EXISTING 3
;; TRUNCATE_EXISTING 5

;; flags
;; FILE_ATTRIBUTE_NORMAL 128



(defcfun (close-handle "CloseHandle" :convention :stdcall)
    :void
  (handle :pointer))

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
  "Use FormatMessage to convert the error code into a system-defined string."
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
	   



(defcfun (%get-last-error "GetLastError" :convention :stdcall) :long)

(defun get-last-error ()
  (let ((code (%get-last-error)))
    (unless (zerop code)
      (error 'win-error :code code))))

(defun create-file (name &key (access #x10000000) (mode 0) (disposition 3) (flags 0))
  (let ((handle (%create-file name access mode (null-pointer) disposition flags (null-pointer))))
    (if (pointer-eq handle (make-pointer #xffffffffffffffff))
        (get-last-error)
        handle)))


(defcfun (%device-io-control "DeviceIoControl" :convention :stdcall)
    :boolean
  (handle :pointer)
  (code :uint32)
  (buffer :pointer)
  (size :uint32)
  (out-buffer :pointer)
  (out-size :uint32)
  (bytes :pointer)
  (overlapped :pointer))

(defcstruct %disk-geometry 
  (cylinders :uint64)
  (media-type :uint32)
  (tracks-per-cylinder :uint32)
  (sectors-per-track :uint32)
  (bytes-per-sector :uint32))

(defstruct geometry 
  cylinders media-type tracks sectors bytes size)

;; IOCTL_DISK_GET_DRIVE_GEOMETRY == #x70000
(defun get-disk-geometry (path)
  (let ((handle (create-file path :mode #x03)))

    (unwind-protect 
         (with-foreign-objects ((geo '(:struct %disk-geometry))
                                (bytes :uint32))
           (let ((res (%device-io-control handle 
                                          #x70000
                                          (null-pointer)
                                          0
                                          geo
                                          (foreign-type-size '(:struct %disk-geometry))
                                          bytes
                                          (null-pointer))))
             (if res
                 (let ((g (make-geometry)))
                   (setf (geometry-cylinders g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'cylinders)
                         (geometry-media-type g) 
                         (foreign-slot-value geo '(:struct %disk-geometry) 'media-type)
                         (geometry-tracks g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'tracks-per-cylinder)
                         (geometry-sectors g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'sectors-per-track)
                         (geometry-bytes g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'bytes-per-sector)
			 (geometry-size g)
			 (* (geometry-cylinders g)
			    (geometry-tracks g)
			    (geometry-sectors g)
			    (geometry-bytes g)))
                   g)
                 (get-last-error))))
    (close-handle handle))))
  
#|  
typedef enum _MEDIA_TYPE {
    Unknown,                // Format is unknown
    F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512,             // 3.5",  720KB,  512 bytes/sector
    F5_360_512,             // 5.25", 360KB,  512 bytes/sector
    F5_320_512,             // 5.25", 320KB,  512 bytes/sector
    F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    F5_180_512,             // 5.25", 180KB,  512 bytes/sector
    F5_160_512,             // 5.25", 160KB,  512 bytes/sector
    RemovableMedia,         // Removable media other than floppy
    FixedMedia,             // Fixed hard disk media
    F3_120M_512,            // 3.5", 120M Floppy
    F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
    F5_640_512,             // 5.25",  640KB,  512 bytes/sector
    F5_720_512,             // 5.25",  720KB,  512 bytes/sector
    F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
    F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
    F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
    F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
    F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
    F8_256_128,             // 8",     256KB,  128 bytes/sector
    F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
    F3_240M_512,            // 3.5",   240Mb Floppy (HiFD)
    F3_32M_512              // 3.5",   32Mb Floppy
} MEDIA_TYPE, *PMEDIA_TYPE;
|#






(defcstruct overlapped 
  (internal :pointer)
  (internal-high :pointer)
  (offset :uint32)
  (offset-high :uint32)
  (handle :pointer))


(defcfun (%write-file "WriteFile" :convention :stdcall)
    :boolean
  (handle :pointer)
  (buffer :pointer)
  (count :uint32)
  (bytes :pointer)
  (overlapped :pointer))

(defun split-offset (offset)
  (values (logand offset #xffffffff)
	  (ash offset -32)))

(defun write-file (handle offset sequence &key (start 0) end)
  (let ((length (length sequence)))
    (with-foreign-objects ((buffer :uint8 length)
			   (nbytes :uint32)
			   (overlapped '(:struct overlapped)))
      (do ((i start (1+ i)))
	  ((= i (or end length)))
	(setf (mem-aref buffer :uint8 (- i start))
	      (elt sequence i)))
      (multiple-value-bind (offset-low offset-high) (split-offset offset)
	(setf (foreign-slot-value overlapped '(:struct overlapped)
				  'offset)
	      offset-low
	      (foreign-slot-value overlapped '(:struct overlapped)
				  'offset-high)
	      offset-high))
      (let ((res (%write-file handle 
			      buffer
			      length
			      nbytes
			      overlapped)))
	(if res
	    nil
	    (get-last-error))))))

(defcfun (%read-file "ReadFile" :convention :stdcall)
    :boolean
  (handle :pointer)
  (buffer :pointer)
  (count :uint32)
  (bytes :pointer)
  (overlapped :pointer))

(defun read-file (handle sequence offset count &key (start 0) end)
  (with-foreign-objects ((buffer :uint8 count)
			 (nbytes :uint32)
			 (overlapped '(:struct overlapped)))
    (multiple-value-bind (offset-low offset-high) (split-offset offset)
      (setf (foreign-slot-value overlapped '(:struct overlapped)
				'offset)
	    offset-low
	    (foreign-slot-value overlapped '(:struct overlapped)
				'offset-high)
	    offset-high))
    (let ((res (%read-file handle 
			   buffer
			   count 
			   nbytes
			   overlapped)))
      (cond
	(res
	 (do ((i start (1+ i)))
	     ((= i (or end (+ start count))) (mem-ref nbytes :uint32))
	  (setf (elt sequence i)
		(mem-aref buffer :uint8 (- i start)))))
	(t 
	 (get-last-error))))))



;; ---------------------


(defclass block-provider (nfs-provider)
  ((handle :initarg :handle :reader block-handle)
   (geometry :initarg :geometry :reader block-geometry)
   (mount-fh :initform #(0 0 0 0) :reader block-mount-fh)
   (dev-name :initform "dev0" :reader block-dev-name)
   (dev-fh :initform #(0 0 0 1) :reader block-dev-fh)))

(defun make-block-provider (device-path)
  (let ((handle (create-file device-path)))
    (make-instance 'block-provider 
		   :handle handle
		   :geometry (get-disk-geometry device-path))))

(defun close-block-provider (provider)
  (close-handle (block-handle provider)))


;; for the mount protocol
(defmethod nfs-provider-mount ((provider block-provider) client)
  (block-mount-fh provider))

(defmethod nfs-provider-unmount ((provider block-provider) client)
  nil)

;; for nfs
(defmethod nfs-provider-attrs ((provider block-provider) fh)
  (cond
    ((equalp fh (block-mount-fh provider))
     (make-fattr3 :type :dir
		  :mode #xff
		  :uid 0
		  :gid 0
		  :size 0
		  :used 0
		  :fileid 0
		  :atime (make-nfs-time3)
		  :mtime (make-nfs-time3)
		  :ctime (make-nfs-time3)))
    ((equalp fh (block-dev-fh provider))
     (make-fattr3 :type :blk
		  :mode #xff
		  :uid 0
		  :gid 0
		  :size (geometry-size (block-geometry provider))
		  :used (geometry-size (block-geometry provider))
		  :fileid 0
		  :atime (make-nfs-time3)
		  :mtime (make-nfs-time3)
		  :ctime (make-nfs-time3)))
    (t 
     (error 'nfs-error :stat :bad-handle))))

;; don't support this
;;(defmethod (setf nfs-provider-attrs) (value (provider simple-provider) handle)
;;  nil)

(defmethod nfs-provider-lookup ((provider block-provider) dh name)
  (cond
    ((equalp dh (block-mount-fh provider))
     (if (string= name (block-dev-name provider))
	 (block-dev-fh provider)
	 (error 'nfs-error :stat :noent)))
    (t 
     (error 'nfs-error :stat :bad-handle))))

(defmethod nfs-provider-read ((provider block-provider) fh offset count)
  "Read count bytes from offset from the object."
  (unless (equalp fh (block-dev-fh fh))
    (error 'nfs-error :stat :bad-handle))
  (let ((buffer (nibbles:make-octet-vector count)))
    (read-file (block-handle provider)
	       buffer
	       offset 
	       count)
    buffer))

(defmethod nfs-provider-write ((provider block-provider) fh offset bytes)
  "Write bytes at offset to the object. Returns the number of bytes written."
  (unless (equalp fh (block-dev-fh provider))
    (error 'nfs-error :stat :bad-handle))
  (write-file (block-handle provider)
	      offset 
	      bytes))

(defmethod nfs-provider-read-dir ((provider block-provider) dh)
  "Returns a list of all object (file and directory) names in the directory."
  (unless (equalp dh (block-mount-fh provider))
    (error 'nfs-error :stat :bad-handle))
  (list (block-dev-name provider)))

;; filesystem information
(defmethod nfs-provider-fs-info ((provider block-provider))
  "Returns dynamic filesystem information, in an FS-INFO structure."
  (make-fs-info :attrs nil ;; attributes of the file 
		:rtmax 1024 ;; maximum read request count
		:rtpref 1024 ;; preferred read count -- should be same as rtmax
		:rtmult 4 ;; suggested multiple for read requests
		:wtmax 1024 ;; maximum write request count 
		:wtpref 1024 ;; preferred write count
		:wtmult 4 ;; suggested multiple for writes
		:dtpref #xffffffff ;; preferred size for read-dir
		:max-fsize 1024 ;; maximum file size
		:time-delta (make-nfs-time3 :seconds 1)
		:properties (enum 'nefarious::nfs-info :homogenous)))

(defmethod nfs-provider-fs-stat ((provider block-provider))
  "Returns static filesystem information, in an FS-STAT structure."
  (make-fs-stat :attrs nil ;; fileattribvutes
		:tbytes #xffffffff ;; total size of the filesystem
		:fbytes #xffffffff ;; free bytes
		:abytes #xffffffff ;; available bytes
		:tfiles #xffffffff ;; total file slots
		:ffiles #xffffffff ;; total number of free file slots
		:afiles #xffffffff ;; available file slots
		:invarsec 1))

(defmethod nfs-provider-path-conf ((provider block-provider))
  "Returns a PATH-CONF structure containing information about the filesystem."
  (make-path-conf :attr nil ;; file attributes
		  :link-max 0 ;; max link size
		  :link-max 0 ;; maximum number of hard links to an object
		  :name-max 255 ;; maximum file name
		  :no-trunc t ;; if T the server will reject any request with a name longer than 
		  :chown-restricted t ;; will reject any attempt to chown
		  :case-insensitive t ;; case insensitive filesystem
		  :case-preserving t))



		   
