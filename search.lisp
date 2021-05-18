;; Fulltext search implementation using Montezuma

(in-package :webinfo)

(defun make-memory-search-index ()
  (make-instance 'montezuma:index))

(defun make-persistent-search-index (path)
  (make-instance 'montezuma:index
		 :path path))

(defclass indexable-info-repository ()
  ((search-index :initarg :search-index
		 :initform nil
		 :accessor repository-search-index)
   (terms-index :initarg :terms-index
		:initform nil
		:accessor repository-terms-index)))

(defmethod initialize-instance :after ((info-repository indexable-info-repository) &rest initargs)
  (declare (ignore initargs))

  (when (not (null (repository-search-index info-repository)))
    (dolist (doc (dir info-repository))
      (fulltext-index-document doc info-repository))))

(defgeneric fulltext-index-document (document info-repository)
  (:documentation "Index DOCUMENT for full text search under INFO-REPOSITORY."))

(defmethod fulltext-index-document (doc (info-repository info-repository))
  (loop for node in (all-nodes doc)
        do
           (montezuma:add-document-to-index
	    (repository-search-index info-repository)
	    (list
	     (cons "node-name"  (node-name node))
	     (cons "node-title" (node-title node))
	     (cons "content" (text-contents node))))))

(defclass fulltext-search-node (info-node)
  ((search-term :initarg :search-term :accessor search-term
                :initform (error "Provide the search term"))
   (info-repository :initarg :info-repository
		    :accessor info-repository)
   (matches :accessor matches)
   (source :initarg :source
           :accessor source))
  (:default-initargs
   :name "Fulltext search"))

(defmethod initialize-instance :after ((node fulltext-search-node) &rest initargs)
  (declare (ignore initargs))
  (setf (matches node) nil)
  (montezuma:search-each
   (repository-search-index (info-repository node))
   (format nil "content:~s" (search-term node))
   (lambda (doc score)
     (declare (ignorable score))
     (let ((document (montezuma:get-document (repository-search-index (info-repository node)) doc)))
       (push (cons (montezuma:document-value document "node-name")
                   (montezuma:document-value document "node-title"))
             (matches node))))))

(defmethod render-node-html ((node fulltext-search-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
    (:h2 (who:fmt "Full text search matches for: ~a" (search-term node)))
    (:ul
     (loop for (node-name . node-title) in (matches node)
           do (who:htm (:li (:a :href node-name (who:str node-title)))))))))
