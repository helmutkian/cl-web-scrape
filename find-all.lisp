;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of an LHTML tree of given properities.
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
	

(defun tcons (x tcell)
  "Adds a new element onto the end of a queue. Based the TCONS function discussed in Norvig's PAIP in the
   picking the right data structure' section of the efficient chapter. A TCELL is a CONS cell whose CAR
   is a LIST of enqueued values and whose CDR is the last CONS cell of that queue."
  (let ((tail (list x)))
    (cond
      ((null tcell)
        (cons tail tail))
      (t 
        (setf (cddr tcell) tail)
        (setf (cdr tcell) tail)))))
			

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
  (let (found)
    (lhtml-find lhtml
		(lambda (src) (setf found (tcons src found)))
		:tag tag
		:class class)
    (car found)))

(defun find-first (lhtml &key tag class)
  "Find first subtree of TAG and CLASS"
  (lhtml-find lhtml
	      (lambda (src) (return-from find-first src))
	      :tag tag
	      :class class))
