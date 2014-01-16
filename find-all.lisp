;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of a DOM tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)


(cl-coop:defcoro find-in-dom (dom tag classes attribs text) ()
  (labels
      ((inner (tree) 
	 (if (and (listp tree) 
		  (or (not tag)
		      (tag= tag (get-tag tree)))
		  (or (not classes)
		      (loop named find-classes
			 with tree-classes = (get-class tree)
			 for c in (alexandria:ensure-list classes)
			 if (not (find c tree-classes :test #'class=))
			   return nil
			 end
			 finally (return-from find-classes t)))
		  (or (not attribs)
		      (loop named find-attribs
			 for a in (alexandria:ensure-list attribs)
			 for attrib-type = (if (listp a) (first a) a)
			 for attrib-val = (when (listp a) (second a))
			 for attrib-found = (get-attrib attrib-type tree)
			 if (or (not attrib-found)
				(and attrib-val 
				     (not (attrib= attrib-val
						   attrib-found))))
			   return nil
			 finally (return-from find-attribs t)))
		  (or (not text)
		      (and (eql text t) (get-text tree))
		      (string= text (get-text tree))))
	     (cl-coop:yield tree)
	     (dolist (node (cdr (alexandria:ensure-list tree)))
	       (inner node)))))
    (inner (alexandria:ensure-list dom))))
		

;;; API Notes:
;;;
;;; :class option can be as a single class or a LIST of classes
;;;
;;; :attrib can be a single attribute, a LIST of attributes, 
;;; or an ALIST of attribs
;;; if looking for a specific attribute-value pair, it must be as an ALIST
;;; regardless of whether its a single pair or multiple.
;;;
;;; :text option can be either an exact string match, or T, meaning
;;; the subtree contains text.
;;; Regex search in text is TODO

			     
(defun find-all (dom &key tag class attrib text)
  (loop with results = (find-in-dom dom tag class attrib text)
        for result = (funcall results)
        until (cl-coop:deadp results)
        collect result))

(defun find-first (dom &key tag class attrib text)
  (funcall (find-in-dom dom tag class attrib text)))


(defun find-all* (dom &key tag class attrib text)
  "Recursive version of FIND-ALL."
  (flet ((find* (tree)
	   (find-all tree :tag tag :class class :attrib attrib :text text)))
    (loop with stack = (find* dom)
          until (null stack)
	  for top = (pop stack)
          for results* = (find* (cddr top))
	  do (loop for r in results*
		   do (push r stack))
	  collect top)))
      
        
       
        