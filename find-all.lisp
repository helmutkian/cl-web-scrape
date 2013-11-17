(in-package #:com.helmutkian.cl-web-scrape)

(defun lhtml-tag-p (thing)
  (keywordp thing))

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

(defun get-tag-attrib (tag attrib lhtml)
  (when (and (eql (first lhtml) tag)
	     (list-of-lists-p (second lhtml)))
    (second (assoc attrib (second lhtml)))))

(defun a-href (lhtml)
  (get-tag-attrib :a :href lhtml))

(defun img-src (lhtml)
  (get-tag-attrib :img :src lhtml))

(defun img-alt (lhtml)
  (get-tag-attrib :img :alt lhtml))
