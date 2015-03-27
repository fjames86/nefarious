;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "An NFS implementation in Common Lisp"
  :license "MIT"
  :components
  ((:file "package")
   (:file "log" :depends-on ("package"))
   (:file "providers" :depends-on ("log"))
   (:file "nfs" :depends-on ("log"))
   (:file "errors" :depends-on ("nfs"))
   (:file "mount" :depends-on ("providers"))
   (:file "interface" :depends-on ("errors"))
   (:file "nsm")
   (:file "server" :depends-on ("interface" "mount" "nsm"))
   (:file "streams" :depends-on ("server"))
   (:file "finfo")
   (:file "simple" :depends-on ("streams" "finfo")))
  :depends-on (:frpc :cl-fad :trivial-gray-streams :cffi))

