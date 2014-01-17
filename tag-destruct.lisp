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
  (eql (ensure-tag tag0) (ensure-tag tag1)))

(declaim (inline attrib=))
(defun attrib= (a0 a1)
  (string= a0 a1))

(defun get-attrib (attrib dom &key tag)
  "Get ATTRIB attribute from TAG. NIL if unsuccessful"
  (let ((attribs (get-attribs dom)))
    (when (and attribs
	       (or (null tag) (eql (get-tag dom) (ensure-tag tag))))
      (second (assoc attrib attribs)))))

(defsetf get-attrib (attrib dom &key tag) (val)
  `(let ((attribs (get-attribs ,dom)))
     (when (and attribs
		(or (null ,tag) (eql (get-tag ,dom) (ensure-tag ,tag))))
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
;;; Interface for pulling text contents from
;;; tag subtrees
;;; ************************************************************

(defun get-text (dom)
  "Returns first string in tree"
  (let ((strs (remove-if-not #'stringp (cdr dom))))
    (if (alexandria:length= strs 1)
	(car strs)
	strs)))
