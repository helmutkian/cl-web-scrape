(asdf:defsystem #:cl-web-scrape
  :depends-on ("closure-html" "cxml" "drakma" "selenium" "cl-coop" "alexandria" "split-sequence")
  :components
   ((:file "package")
    (:file "core"
	   :depends-on ("package"))
    (:file "find-all"
	   :depends-on ("package"))
    (:file "tag-destruct"
	   :depends-on ("package"))))
