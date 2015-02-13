

(asdf:defsystem :nefarious
  :name "Nefarious"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "NFS library"
  :license "MIT"
  :components
  ((:file "package")
   (:file "nfs" :depends-on ("package")))
  :depends-on (:frpc))

