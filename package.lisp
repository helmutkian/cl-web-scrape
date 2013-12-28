(defpackage #:com.helmutkian.cl-web-scrape
  (:nicknames #:ws)
  (:use :cl)
  (:export #:html->lhtml
	   #:get-html
	   #:get-lhtml
	   #:find-all
	   #:find-first
	   #:tag-attrib
	   #:a-href
	   #:img-src
	   #:img-alt
	   #:text))
