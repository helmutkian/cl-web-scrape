;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of an LHTML tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)

(defun list-of-lists-p (thing)
  "Is THING a LIST that contains only LISTs?"
  (and (listp thing) (every #'listp thing)))

(defun find-all (lhtml &key tag class)
  "Find all LHTML subtrees of TAG and CLASS"
  (let ((found nil))
    (labels ((has-attribs-p (source)
	       (and (list-of-lists-p (second source))
		    (equalp 
		     (second (assoc :class
				    (second source)))
			    class)))
	     (inner (source)
	       (when (and (listp source)
			  (or (not class)
			      (cdr source)))
		 (if (and (or (not tag)
			      (eql (first source) tag))
			  (or (not class)
			      (has-attribs-p source)))
		     (return-from inner (push source found))
		     (mapc #'inner (cdr source))))))
      (inner lhtml)
      found)))
