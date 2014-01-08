;;; ************************************************************
;;; This module defines an interface for destructuring DOM
;;; tags, providing accessors for their content and attributes.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

;;; ************************************************************
;;; Generic tag attribute reader
;;; ************************************************************

(defun tag-attrib (tag attrib dom)
  "Get ATTRIB attribute from TAG. NIL if unsuccessful"
  (when (and (eql (first dom) tag)
	     (list-of-lists-p (second dom)))
    (second (assoc attrib (second dom)))))

(defsetf tag-attrib (tag attrib dom) (val)
  `(when (and (eql (first ,dom) ,tag)
	      (list-of-lists-p (second ,dom)))
     (setf (second (assoc ,attrib (second ,dom))) ,val)))
	   

;;; ************************************************************
;;; <a ...> Tag
;;; ************************************************************

(defun a-href (dom)
  (tag-attrib :a :href dom))

(defun (setf a-href) (url dom)
  (setf (tag-attrib :a :href dom) url))

;;; ************************************************************
;;; <img ...> Tag
;;; ************************************************************

(defun img-src (dom)
  (tag-attrib :img :src dom))

(defun (setf img-src) (url dom)
  (setf (tag-attrib :img :src dom) url))

(defun img-alt (dom)
  (tag-attrib :img :alt dom))

(defun (setf img-alt) (url dom)
  (setf (tag-attrib :img :alt dom) url))

;;; ************************************************************
;;; Generic interface for pulling text contents from
;;; tag subtrees
;;; ************************************************************

(defgeneric dom-get-text (tree-type dom)
  (:documentation "Generic, low-level interface for pulling
text out of different DOM subtree types"))

(defgeneric (setf dom-get-text) (new-text tree-type dom)
  (:documentation "Generic, low-level interface for replacing
text in different DOM subtree types"))

(defmethod dom-get-text ((tree-type t) dom)
  "Default method. Most tags such as <li ...>, <img...>, <a ...>,
etc have text in the same place."
  (third dom))

(defmethod (setf dom-get-text) (new-text (tree-type t) dom)
  (setf (third dom) new-text))

#|
(defmethod dom-get-text ((tree-type (eql :img)) dom)
  (third dom))

(defmethod dom-get-text ((tree-type (eql :href)) dom)
  (third dom))
|#

(defun text (dom)
  (dom-get-text (car dom) dom))

(defun (setf text) (new-text dom)
  (setf (dom-get-text (car dom) dom) new-text))
