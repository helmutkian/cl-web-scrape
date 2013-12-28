(in-package #:com.helmutkian.cl-web-scrape)

(defun get-html (uri)
  (drakma:http-request uri))

(defun html->lhtml (html)
  "Converts HTML source into LHTML tree "
  (chtml:parse html
	       (chtml:make-lhtml-builder)))

(defun get-lhtml (uri)
  (html->lhtml (get-html uri)))
