(defun lhtml-tag-p (thing)
  (keywordp thing))

(defun list-of-lists-p (thing)
  "Is THING a LIST that contains only LISTs?"
  (and (listp thing) (every #'listp thing)))

(defgeneric find-all (tag val lhtml)
  (:documentation "Finds every instance of a tag (:BODY, :A, etc) with the given value associated with it within an LHTML tree"))

(defmethod find-all ((tag (eql :class)) val lhtml)
  "Finds all tags that have the attribute (:CLASS VAL)"
  (let ((found nil))
    (labels ((inner (source)
	       (when (and (listp source) (cdr source))
		 (if (and (list-of-lists-p (second source))
			  (equalp (second (assoc :class 
						 (second source)))
				  val))
		     (return-from inner (push source found))
		     (mapc #'inner (cdr source))))))
      (inner lhtml)
      found)))
		     
