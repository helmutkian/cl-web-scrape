(asdf:defsystem #:cl-web-scrape
  :components
  ((:file "package")
   (:file "find-all"
	  :depends-on ("package"))
   (:file "tag-destruct"
	  :depends-on ("package"))))
