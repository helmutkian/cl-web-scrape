;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of a DOM tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

(defun list-of-lists-p (thing)
  "Is THING a LIST that contains only LISTs?"
  (and (listp thing) (every #'listp thing)))

(defun has-attribs-p (source class)
  "Does this LHTML SOURCE tree have an ALIST of attributes?"
  (and (list-of-lists-p (second source))
       (equalp 
	(second (assoc :class
		       (second source)))
	class)))
	

;"Returns coroutine that performs depth-first search of DOM tree,
;yielding each subtree that matches the search criteria."
(cl-coop:defcoro find-all (dom &key tag class) ()
  (labels ((inner (source)
	     (when (and (listp source)
			(or (not class)
			    (cdr source)))
	       (if (and (or (not tag)
			    (eql (first source) tag))
			(or (not class)
			    (has-attribs-p source class)))
		   (cl-coop:yield source)
		   (dolist (elm (cdr source))
		     (inner elm))))))
    (inner dom)))


