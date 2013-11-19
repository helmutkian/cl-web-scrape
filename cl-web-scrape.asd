(asdf:defsystem #:cl-web-scrape
  :components
  ((:system "closure-html")
   (:file "package")
   (:file "core"
	  :depends-on ("package" "closure-html"))
   (:file "find-all"
	  :depends-on ("package"))
   (:file "tag-destruct"
	  :depends-on ("package"))))
