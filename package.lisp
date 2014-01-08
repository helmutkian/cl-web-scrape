(defpackage #:com.helmutkian.cl-web-scrape
  (:nicknames #:ws)
  (:use :cl)
  (:export #:source->dom
	   #:get-raw-source
	   #:get-raw-dom
	   #:get-processed-source
	   #:get-processed-dom
	   #:find-all
	   #:find-first
	   #:attrib
	   #:a-href
	   #:img-src
	   #:img-alt
	   #:get-class
	   #:text))
