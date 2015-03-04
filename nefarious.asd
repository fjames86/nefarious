;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An NFS implementation in Common Lisp"
  :license "MIT"
  :components
  ((:file "package")
   (:file "providers" :depends-on ("package"))
   (:file "nfs" :depends-on ("package"))
   (:file "errors" :depends-on ("nfs"))
   (:file "mount" :depends-on ("providers"))
   (:file "interface" :depends-on ("errors"))
   (:file "server" :depends-on ("interface"))
   (:file "streams" :depends-on ("server"))
   (:module :builtin-providers
	    :pathname "providers"
	    :components 
	    ((:file "simple"))
	    :depends-on ("streams")))
  :depends-on (:frpc :cl-fad :trivial-gray-streams))


