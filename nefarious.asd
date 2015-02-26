;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "NFS library"
  :license "MIT"
  :components
  ((:file "package")
   (:file "handles" :depends-on ("package"))
   (:file "mount" :depends-on ("handles"))
   (:file "nfs" :depends-on ("package"))
   (:file "interface" :depends-on ("handles"))
   (:file "server" :depends-on ("interface")))
  :depends-on (:frpc :cl-fad))

