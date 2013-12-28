(asdf:defsystem #:cl-web-scrape
  :depends-on ("closure-html" "drakma")
  :components
   ((:file "package")
    (:file "core"
	   :depends-on ("package"))
    (:file "find-all"
	   :depends-on ("package"))
    (:file "tag-destruct"
	   :depends-on ("package"))))
