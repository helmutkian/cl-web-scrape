(in-package #:com.helmutkian.cl-web-scrape)

(defun make-lhtml-tree (html)
  "Converts HTML source into LHTML tree "
  (chtml:parse html
	       (chtml:make-lhtml-builder)))
