(in-package #:com.helmutkian.cl-web-scrape)

(defparameter *default-browser* "*firefox")

(defun get-raw-source (uri)
  "Returns raw source from a page."
  (drakma:http-request uri))

(defun get-processed-source (uri &key (browser *default-browser*))
  "Returns page source as rendered by a browser"
  ;; TODO: Ensure selenium server is running
  ;(drakma:http-request (puri:render-uri selenium:*selenium-driver-url* nil))
  (selenium:with-selenium-session (selenium:*selenium-session* browser uri)
    (selenium:do-open uri)
    (selenium:do-get-html-source)))

(defun source->dom (html)
  "Converts HTML source into LHTML tree "
  (chtml:parse html
	       (chtml:make-lhtml-builder)))

(defun get-raw-dom (uri)
  (html->lhtml (get-raw-source uri)))

(defun get-processed-dom (uri &key (browser *default-browser*))
  (html->lhtml (get-processed-source uri :browser browser)))


