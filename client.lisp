

(in-package #:nefarious)

;; use trivial-gray-streams to define a nfs-file-stream type


(defmacro with-nfs-mount ((var path &key host port protocol) &body body)
  (alexandria:with-gensyms (gpath ghost gport gprotocol)
    `(let ((,gpath ,path)
	   (,ghost ,host)
	   (,gport ,port)
	   (,gprotocol ,protocol))
       (let ((,var (car (nefarious.mount:call-mount ,path 
						    :host ,ghost 
						    :port ,gport 
						    :protocol ,gprotocol))))
	 (unwind-protect (progn ,@body)
	   (nefarious.mount:call-unmount ,gpath 
					 :host ,ghost 
					 :port ,gport 
					 :protocol ,gprotocol))))))

