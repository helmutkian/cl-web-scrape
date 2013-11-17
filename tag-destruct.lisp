;;; ************************************************************
;;; This module defines an interface for destructuring LHTML
;;; tags, providing readers for their content and attributes.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

;;; ************************************************************
;;; Generic tag attribute reader
;;; ************************************************************

(defun tag-attrib (tag attrib lhtml)
  "Get ATTRIB attribute from TAG. NIL if unsuccessful"
  (when (and (eql (first lhtml) tag)
	     (list-of-lists-p (second lhtml)))
    (second (assoc attrib (second lhtml)))))

;;; ************************************************************
;;; <a ...> Tag
;;; ************************************************************

(defun a-href (lhtml)
  (tag-attrib :a :href lhtml))

;;; ************************************************************
;;; <img ...> Tag
;;; ************************************************************

(defun img-src (lhtml)
  (tag-attrib :img :src lhtml))

(defun img-alt (lhtml)
  (tag-attrib :img :alt lhtml))
