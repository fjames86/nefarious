;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An NFS implementation in Common Lisp"
  :license "MIT"
  :components
  ((:file "handles")
   (:file "mount" :depends-on ("handles"))
   (:file "package" :depends-on ("mount"))
   (:file "nfs" :depends-on ("package"))
   (:file "interface" :depends-on ("nfs"))
   (:file "server" :depends-on ("interface"))
   (:file "streams" :depends-on ("server")))
  :depends-on (:frpc :cl-fad :trivial-gray-streams))

