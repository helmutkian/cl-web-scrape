(defpackage #:com.helmutkian.cl-web-scraper
  (:nicknames #:ws)
  (:use #:cl)
  (:export #:get-source
	   #:make-tag-tree
           #:tag
	   #:attributes
	   #:child-nodes
	   #:traverse-tree
	   #:text
	   #:by
	   #:find-element
	   #:find-all-elements))

(in-package #:com.helmutkian.cl-web-scraper)

;;; ************************************************************
;;; ************************************************************

(defun get-source (uri)
  "Get document source via HTTP request."
  (drakma:http-request uri))

(defun make-tag-tree (source)
  "Build tag-tree from document source."
  (chtml:parse source (chtml:make-lhtml-builder)))

;;; ************************************************************
;;; ************************************************************

(defun tag (tree)
  "
Returns the tag of a the root of a tag-tree. 

Example: 

    (tag (:a ((:href \"http://www.foo.com/bar\") 
              (:charset \"UTF-8\")) 
           (:i \"FooBar\")))
    => :A
  "
  (first tree))

(defun attributes (tree)
  "
Returns a CADR-valued ALIST of the attributes of the root of a tag-tree.

Example:

   (attributes (:a ((:href \"http://www.foo.com/bar\") 
                    (:charset \"UTF-8\")) 
                 (:i \"FooBar\")))
   => ((:href \"http://www.foo.com/bar\") (:charset \"UTF-8\"))
  "
  (second tree))

(defun child-nodes (tree)
  "
Returns all the subtrees of the given tag-tree.

Example:

  (child-nodes (:a ((:href \"http://www.foo.com/bar\")
                    (:charset \"UTF-8\"))
                 (:i \"FooBar\")))
  => (:i \"FooBar\")
  "
  (cddr tree))

;;; ************************************************************
;;; Queue: Private to package
;;; ************************************************************

(defun make-queue () 
  "Returns empty queue"
  (cons nil nil))

(defun queue-push (object queue)
  "Inserts new element at end of queue."
  (let ((new-elm (list object)))
    (if (null (car queue))
	(setf (car queue) new-elm
	      (cdr queue) new-elm)
	(setf (cddr queue) new-elm
	      (cdr queue) new-elm))
    queue))

(defun queue-pop (queue)
  "Removes element at the front of queue."
  (pop (car queue)))

(defun queue-empty-p (queue)
  "Is the queue empty?"
  (null (car queue)))

(defun queue->list (queue)
  "Returns a list of the elements in the queue."
  (car queue))


;;; ************************************************************
;;; ************************************************************

(defun traverse-tree (fn tree)
  "Visits each sub-tag-tree in the given tag-tree"
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
  "
Returns text from each sub-tree in given tag-tree.

Example:

  (text (:p () 
          (:b \"This is important! \")
          \"Do not click on \" 
          (:a (:href \"http://www.foo.com/bar\") \"this!\")))
  => \"This is important! Do not click on this!\"
  "
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

(defgeneric by (selector value)
  (:documentation 
   "Provides a protocol for retrieving subtrees by a given selector 
    (e.g. tag, attribute, id, name, class, etc.)"))

(defmethod by ((selector (eql :tag)) value)
  "By tag name"
  (lambda (elm)
    (and (listp elm)
	 (eql (tag elm) value))))

(defmethod by ((selector (eql :attribute)) value)
  "Attribute value is a list with the CAR being the attribute KEYWORD
   (e.g. :HREF, :SHAPE, :COORDS, etc.) and its CADR a STRING with the
   attribute's value.
 
   Example: (by :attribute (:SHAPE \"rect\"))
"
  (destructuring-bind (attr val) value
    (lambda (elm)
      (let ((result (and (listp elm) 
			 (assoc attr (attributes elm)))))
	(and result
	     (string= val (cadr result)))))))

(defmethod by ((selector (eql :class)) value)
  "By class name"
  (by :attribute `(:class ,value)))

(defmethod by ((selector (eql :name)) value)
  "By name attribute"
  (by :attribute `(:name ,value)))

(defmethod by ((selector (eql :id)) value)
  "By id attribute"
  (by :attribute `(:id ,value)))

;;; ************************************************************
;;; ************************************************************

(defun find-element (selector tree)
  "Returns the first subtree in the given tag-tree that satisfies the selector."
  (traverse-tree
   (lambda (elm)
     (when (funcall selector elm)
       (return-from find-element elm)))
   tree))

(defun find-all-elements (selector tree)
  "Finds all subtrees in the given tag-tree that satisfies the selector."
  (let ((found (make-queue)))
    (traverse-tree
     (lambda (elm)
       (when (funcall selector elm)
	 (queue-push elm found)))
     tree)
    (queue->list found)))
