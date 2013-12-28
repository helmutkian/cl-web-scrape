(defpackage #:cl-web-scrape-loader
  (:nicknames #:wsl)
  (:use #:cl)
  (:export #:cl-web-scrape-load))

(in-package #:cl-web-scrape-loader)

(defun cl-web-scrape-load (path-str)
  (mapc #'ql:quickload '("closure-html" "drakma"))
  (load (pathname (concatenate 'string path-str "cl-web-scrape.asd")))
  (setf asdf:*central-registry*
	(nunion (list (pathname path-str)) asdf:*central-registry*))
  (asdf:operate 'asdf:load-op "cl-web-scrape")
  (setf *package* 'common-lisp-user))