;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of a DOM tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

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


