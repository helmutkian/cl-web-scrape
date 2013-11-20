;;; ************************************************************
;;; This module defines an interface for destructuring LHTML
;;; tags, providing accessors for their content and attributes.
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

(defsetf tag-attrib (tag attrib lhtml) (val)
  `(when (and (eql (first ,lhtml) ,tag)
	      (list-of-lists-p (second ,lhtml)))
     (setf (second (assoc ,attrib (second ,lhtml))) ,val)))
	   

;;; ************************************************************
;;; <a ...> Tag
;;; ************************************************************

(defun a-href (lhtml)
  (tag-attrib :a :href lhtml))

(defun (setf a-href) (url lhtml)
  (setf (tag-attrib :a :href lhtml) url))

;;; ************************************************************
;;; <img ...> Tag
;;; ************************************************************

(defun img-src (lhtml)
  (tag-attrib :img :src lhtml))

(defun (setf img-src) (url lhtml)
  (setf (tag-attrib :img :src lhtml) url))

(defun img-alt (lhtml)
  (tag-attrib :img :alt lhtml))

(defun (setf img-alt) (url lhtml)
  (setf (tag-attrib :img :alt lhtml) url))
