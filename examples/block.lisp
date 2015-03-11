

(defpackage #:nefarious.block
  (:use #:cl #:cffi)
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

(defstruct disk-geometry 
  cylinders media-type tracks sectors bytes)

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
                 (let ((g (make-disk-geometry)))
                   (setf (disk-geometry-cylinders g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'cylinders)
                         (disk-geometry-media-type g) 
                         (foreign-slot-value geo '(:struct %disk-geometry) 'media-type)
                         (disk-geometry-tracks g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'tracks-per-cylinder)
                         (disk-geometry-sectors g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'sectors-per-track)
                         (disk-geometry-bytes g)
                         (foreign-slot-value geo '(:struct %disk-geometry) 'bytes-per-sector))
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





(defcfun (%write-file "WriteFile" :convention :stdcall)
    :boolean
  (handle :pointer)
  (buffer :pointer)
  (count :uint32)
  (bytes :pointer)
  (overlapped :pointer))

(defcstruct overlapped 
  (internal :pointer)
  (internal-high :pointer)
  (offset :uint32)
  (offset-high :uint32)
  (handle :pointer))

(defcfun (%read-file "ReadFile" :convention :stdcall)
    :boolean
  (handle :pointer)
  (buffer :pointer)
  (count :uint32)
  (bytes :pointer)
  (overlapped :pointer))

