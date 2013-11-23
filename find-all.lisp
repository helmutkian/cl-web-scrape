;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of an LHTML tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

(defun list-of-lists-p (thing)
  "Is THING a LIST that contains only LISTs?"
  (and (listp thing) (every #'listp thing)))

(defun has-attribs-p (source class)
  (and (list-of-lists-p (second source))
       (equalp 
	(second (assoc :class
		       (second source)))
	class)))

(defun lhtml-find (lhtml call-back &key tag class)
  "Generic search function over LHTML tree. Applies
   CALL-BACK when target is found."
  (labels ((inner (source)
	     (when (and (listp source)
			(or (not class)
			    (cdr source)))
	       (if (and (or (not tag)
			    (eql (first source) tag))
			(or (not class)
			    (has-attribs-p source class)))
		   (funcall call-back source)
		   (mapc #'inner (cdr source))))))
    (inner lhtml)))  

(defun find-all (lhtml &key tag class)
  "Find all subtrees of TAG and CLASS"
  (let ((found nil))
    (lhtml-find lhtml
		(lambda (src) (push src found))
		:tag tag
		:class class)
    found))

(defun find-first (lhtml &key tag class)
  "Find first subtree of TAG and CLASS"
  (lhtml-find lhtml
	      (lambda (src) (return-from find-first src))
	      :tag tag
	      :class class))
