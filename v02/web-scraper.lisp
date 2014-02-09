(defun tag (tree)
  (first tree))

(defun attributes (tree)
  (second tree))

(defun child-nodes (tree)
  (cddr tree))

;;; ************************************************************
;;; ************************************************************

(defun make-queue () (cons nil nil))

(defun queue-push (object queue)
  (let ((new-elm (list object)))
    (if (null (car queue))
	(setf (car queue) new-elm
	      (cdr queue) new-elm)
	(setf (cddr queue) new-elm
	      (cdr queue) new-elm))
    queue))

(defun queue-pop (queue)
  (pop (car queue)))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue->list (queue)
  (car queue))


;;; ************************************************************
;;; ************************************************************

(defun traverse-tree (fn tree)
  (loop with stack = (list tree)
        until (null stack)
        for top = (pop stack)
        do (funcall fn top)
        when (listp top)
          do (dolist (elm (reverse (child-nodes top)))
	       (push elm stack))))

;;; ************************************************************
;;; ************************************************************

(defun text (tree)
  (let ((strs (make-queue)))
    (traverse-tree (lambda (elm)
		     (cond ((stringp elm) (queue-push elm strs))
			   ((and (listp elm)
				 (eql (tag elm) :br) (queue-push "\n" strs)))))
		   tree)
    (reduce (lambda (all-text str)
	      (concatenate 'string all-text str))
	    (queue->list strs))))

;;; ************************************************************
;;; ************************************************************

(defgeneric by (method value))

(defmethod by ((method (eql :tag)) value)
  (lambda (elm)
    (and (listp elm)
	 (eql (tag elm) value))))

(defmethod by ((method (eql :attribute)) value)
  (destructuring-bind (attr val) value
    (lambda (elm)
      (let ((result (and (listp elm) 
			 (assoc attr (attributes elm)))))
	(and result
	     (string= val (cadr result)))))))

(defmethod by ((method (eql :class)) value)
  (by :attribute `(:class ,value)))

(defmethod by ((method (eql :name)) value)
  (by :attribute `(:name ,value)))

(defmethod by ((method (eql :id)) value)
  (by :attribute `(:id ,value)))

;;; ************************************************************
;;; ************************************************************

(defun find-element (method tree)
  (traverse-tree
   (lambda (elm)
     (when (funcall method elm)
       (return-from find-element elm)))
   tree))

(defun find-all-elements (method tree)
  (let ((found (make-queue)))
    (traverse-tree
     (lambda (elm)
       (when (funcall method elm)
	 (queue-push elm found)))
     tree)
    (queue->list found)))