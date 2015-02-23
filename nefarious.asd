;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "NFS library"
  :license "MIT"
  :components
  ((:file "mount")
   (:file "package" :depends-on ("mount"))
   (:file "nfs" :depends-on ("package"))
   (:file "interface" :depends-on ("nfs"))
   (:file "server" :depends-on ("package")))
  :depends-on (:frpc))

