(defpackage #:cl-web-scrape-loader
  (:nicknames #:wsload)
  (:use #:cl)
  (:export #:cl-web-scrape-load))

(in-package #:cl-web-scrape-loader)

(defun load-system ()
  (ql:quickload '(closure-html drakma cl-coop selenium cl-web-scrape)))