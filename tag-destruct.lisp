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

(declaim (inline get-tag))
(defun get-tag (dom)
  (first dom))

(declaim (inline tag=))
(defun tag= (tag0 tag1)
  (eql (ensure-tag tag0) tag1))

(declaim (inline attrib=))
(defun attrib= (a0 a1)
  (string= a0 a1))

(defun get-attrib (attrib dom &key tag)
  "Get ATTRIB attribute from TAG. NIL if unsuccessful"
  (let ((attribs (get-attribs dom)))
    (when (and attribs
	       (or (null tag) (eql (tag dom) (ensure-tag tag))))
      (second (assoc attrib attribs)))))

(defsetf get-attrib (attrib dom &key tag) (val)
  `(let ((attribs (get-attribs ,dom)))
     (when (and attribs
		(or (null ,tag) (eql (tag ,dom) (ensure-tag ,tag))))
       (setf (second (assoc ,attrib attribs)) ,val))))


;;; ************************************************************
;;; <a ...> Tag
;;; ************************************************************

(defun a-href (dom)
  (get-attrib :href dom :tag :a))

(defun (setf a-href) (url dom)
  (setf (get-attrib :href dom :tag :a) url))

;;; ************************************************************
;;; <img ...> Tag
;;; ************************************************************

(defun img-src (dom)
  (get-attrib :src dom :tag :img))

(defun (setf img-src) (url dom)
  (setf (get-attrib :src dom :tag :img) url))

(defun img-alt (dom)
  (get-attrib :alt dom :tag :img))

(defun (setf img-alt) (url dom)
  (setf (get-attrib :alt dom :tag :img) url))


;;; ************************************************************
;;; Class accessors
;;; ************************************************************

(declaim (inline class=))
(defun class= (class0 class1)
  (string= class0 class1))

(defun get-class (dom)
  "Returns a list of all the classes of the root of the given
   DOM tree."
  (split-sequence:split-sequence #\space (get-attrib :class dom)))

(defun (setf get-class) (classes dom)
  (setf (get-attrib :class dom)
	(reduce (lambda (class str) (concatenate 'string class " " str))
		classes)))

;;; ************************************************************
;;; Generic interface for pulling text contents from
;;; tag subtrees
;;; ************************************************************

(defgeneric dom-get-text (tag dom)
  (:documentation "Generic, low-level interface for pulling
text out of different DOM subtree types"))

(defgeneric (setf dom-get-text) (new-text tag dom)
  (:documentation "Generic, low-level interface for replacing
text in different DOM subtree types"))

(defmethod dom-get-text ((tag t) dom)
  "Default method. Searches to find first string in root of tree. Least
   efficient and reliable way of getting text. Each tag should have its
   own method with better than O(n) retrieval of text, ideally."
  (loop for elm in dom
        if (stringp elm) return elm))

;;; <img>
(defmethod dom-get-text ((tag (eql :img)) dom)
  (third dom))

(defmethod (setf dom-get-text) (new-text (tag (eql :img)) dom)
  (setf (third dom) new-text))

;;; <a>
(defmethod dom-get-text ((tag (eql :a)) dom)
  (third dom))

(defmethod (setf dom-get-text) (new-text (tag (eql :a)) dom)
  (setf (third dom) new-text))

;;; External interface
(defun get-text (dom)
  (dom-get-text (ensure-tag (car dom)) dom))

(defun (setf get-text) (new-text dom)
  (setf (dom-get-text (ensure-tag (car dom)) dom) new-text))
