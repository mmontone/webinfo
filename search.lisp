;; Fulltext search implementation using Montezuma

(in-package :webinfo)

(defparameter *search-index* (make-instance 'montezuma:index))

(defun fulltext-index-document (doc)
  (loop for node in (all-nodes doc)
        do
           (montezuma:add-document-to-index *search-index*
                                            (list
                                             (cons "node-name"  (node-name node))
                                             (cons "node-title" (node-title node))
                                             (cons "content" (text-contents node))))))

(defun test-search-index (term)
  (montezuma:search-each *search-index* (format nil "content:~s" term)
                         #'(lambda (doc score)
                             (format T "~&Document ~S found with score of ~S." doc score))))

(defclass fulltext-search-node (info-node)
  ((search-term :initarg :search-term :accessor search-term
                :initform (error "Provide the search term"))
   (search-index :initarg :search-index :accessor search-index-of
                 :initform (error "Provide the search index"))
   (matches :accessor matches)))

(defmethod initialize-instance :after ((node fulltext-search-node) &rest initargs)
  (setf (matches node) nil)
  (montezuma:search-each
   (search-index-of node)
   (format nil "content:~s" (search-term node))
   (lambda (doc score)
     (let ((document (montezuma:get-document *search-index* doc)))
       (push (cons (montezuma:document-value document "node-name")
                   (montezuma:document-value document "node-title"))
             (matches node))))))

(defmethod render-node ((node fulltext-search-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
    (:h1 "Full text search matches")
    (:ul
     (loop for (node-name . node-title) in (matches node)
           do (who:htm (:li (:a :href node-name (who:str node-title)))))))))
