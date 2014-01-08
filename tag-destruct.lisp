;;; ************************************************************
;;; This module defines an interface for destructuring DOM
;;; tags, providing accessors for their content and attributes.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

;;; ************************************************************
;;; Utils
;;; ************************************************************

(defun ensure-tag (thing)
  "Consolidate various ways of representing tags
   For example the tag <a> could be passed around as
   :a 'a \"a\" \"<a>\""
  (typecase thing
    (keyword thing)
    (symbol (intern (string thing) "KEYWORD"))
    (string
     (intern
      (map 'string
	   #'char-upcase
	   (if (char= (char thing 0) #\<)
	       (loop for i from 1
		  until (or (>= i (length thing)) 
			    (char= (char thing i) #\>))
		  finally (return (subseq thing 1 i)))
	       thing))
      "KEYWORD"))))
	       
(defun list-of-lists-p (thing)
  "Is THING a LIST that contains only LISTs?"
  (and (listp thing) (every #'listp thing)))

(defun get-attribs (dom)
  "Returns a CADR valued ALIST of attributes."
  (when (list-of-lists-p (second dom))
    (second dom)))
  

;;; ************************************************************
;;; Generic tag attribute accessor
;;; ************************************************************

(defun attrib (attrib dom &key tag)
  "Get ATTRIB attribute from TAG. NIL if unsuccessful"
  (let ((attribs (get-attribs dom)))
    (when (and attribs
	       (or (null tag) (eql (first dom) (ensure-tag tag))))
      (second (assoc attrib attribs)))))

(defsetf attrib (attrib dom &key tag) (val)
  `(let ((attribs (get-attribs ,dom)))
     (when (and attribs
		(or (null ,tag) (eql (first ,dom) (ensure-tag ,tag))))
       (setf (second (assoc ,attrib attribs))))))


;;; ************************************************************
;;; <a ...> Tag
;;; ************************************************************

(defun a-href (dom)
  (attrib :href dom :tag :a))

(defun (setf a-href) (url dom)
  (setf (attrib href dom :tag :a) url))

;;; ************************************************************
;;; <img ...> Tag
;;; ************************************************************

(defun img-src (dom)
  (attrib :src dom :tag :img))

(defun (setf img-src) (url dom)
  (setf (attrib :src dom :tag :img) url))

(defun img-alt (dom)
  (attrib :alt dom :tag :img))

(defun (setf img-alt) (url dom)
  (setf (attrib :alt dom :tag :img) url))


;;; ************************************************************
;;; Class accessors
;;; ************************************************************

(defun get-class (dom)
  "Returns a list of all the classes of the root of the given
   DOM tree."
  (split-sequence:split-sequence #\space (attrib :class dom)))

(defun (setf get-class) (classes dom)
  (setf (attrib :class dom)
	(reduce (lambda (class str) (concatenate 'string class " " str))
		classes)))

;;; ************************************************************
;;; Generic interface for pulling text contents from
;;; tag subtrees
;;; 
;;; WARNING: DOESN'T WORK YET!
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
