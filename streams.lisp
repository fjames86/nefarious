;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:nefarious)


;;
;; stream to read from remote files
;; basic idea: 
;; 1. maintain an internal buffer which is initially filled with the contents of the remote file
;; 2. reads from the stream corrspond to reads from the buffer
;; 3. writes to the stream corrspond to writes to the buffer
;; 4. when the buffer is exhausted (either from reading or writing), its contents it written back
;; to the remote side and the buffer is refilled with the next block of contents
;; 

(defun copy-remote-file (handle pathspec call-args &key (size 4096) attributes)
  "Makes a copy of the file named by HANDLE to the local path named by PATHSPEC. Will copy the file in chunks of SIZE bytes. 

If the file attributes are already known, supply them with the ATTRIBUTES parameter, otherwise they will be retrieved from the server."
  (with-open-file (f pathspec 
		     :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (do ((attrs (or attributes (apply #'call-get-attrs handle call-args)))
	 (offset 0 (+ offset size))
	 (eof-p nil))
	((or eof-p (>= offset (fattr3-size attrs))))
      (multiple-value-bind (bytes eof %attrs) (apply #'call-read handle offset size call-args)
	(declare (ignore %attrs))
	(write-sequence bytes f)
	(when eof (setf eof-p t))))))

(defun copy-remote-directory (dhandle pathspec call-args &key (size 4096))
  "Copy the contents of the remote directory named by DHANDLE to the local path named by PATHSPEC. Will recursively copy all files and directories."
  (ensure-directories-exist pathspec)
  (let ((filenames (apply #'call-read-dir dhandle call-args)))
    (dolist (filename filenames)      
      (multiple-value-bind (handle attrs dattrs) (apply #'call-lookup dhandle filename call-args)
	(declare (ignore dattrs))
	(case (fattr3-type attrs)
	  (:reg (copy-remote-file handle 
				  (cl-fad:merge-pathnames-as-file pathspec filename)
				  call-args 
				  :size size
				  :attributes attrs))
	  (:dir (copy-remote-directory handle 
				       (cl-fad:merge-pathnames-as-directory pathspec (concatenate 'string filename "/"))
				       call-args 
				       :size size)))))))
      


