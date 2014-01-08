;;; ************************************************************
;;; This module defines a interface for pulling subtrees
;;; out of a DOM tree of given properities.
;;; ************************************************************

(in-package #:com.helmutkian.cl-web-scrape)


(cl-coop:defcoro find-in-dom (dom tag classes attribs) ()
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
			 if (not (get-attrib a tree))
			   return nil
			 end
			 finally (return-from find-attribs t))))
	     (cl-coop:yield tree)
	     (dolist (node (cdr (alexandria:ensure-list tree)))
	       (inner node)))))
    (inner dom)))
		
			     
(defun find-all (dom &key tag class attrib)
  (loop with results = (find-in-dom dom tag class attrib)
        for result = (funcall results)
        until (cl-coop:deadp results)
        collect result))

(defun find-first (dom &key tag class attrib)
  (funcall (find-in-dom dom tag class attrib)))