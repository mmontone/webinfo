;;;; webinfo.lisp

(in-package #:webinfo)


(defclass info-document ()
  ((filepath :initarg :filepath
             :accessor filepath)
   (file :initarg :file
         :accessor file)
   (title :initarg :title
          :accessor title)
   (description :initarg :description
                :accessor description)
   (function-index)
   (variable-index)
   (concept-index)
   (top-node :accessor top-node)))

(defclass info-node ()
  ((name :initarg :name :accessor node-name)))

(defmethod print-object ((node info-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a" (node-name node))))

(defclass xml-info-document (info-document)
  ((xml-document :accessor xml-document :initarg :xml)))

(defclass xml-info-node (info-node)
  ((node-xml :accessor node-xml :initarg :node-xml)
   (content-xml :accessor content-xml :initarg :content-xml)))

(defmethod initialize-instance :after ((info-document xml-info-document) &rest initargs)
  (declare (ignore initargs))
  (flet ((resolver (pubid sysid)
           (declare (ignore pubid))
           (when (eq (puri:uri-scheme sysid) :http)
             (drakma:http-request sysid :want-stream t))))
    (setf (xml-document info-document)
          (cxml:parse (filepath info-document)
                      (cxml-dom:make-dom-builder)
                      :entity-resolver #'resolver))))

(defmethod top-node ((info-document xml-info-document))
  (make-instance 'xml-info-node

                 ))

(defparameter *djula-manual*
  (make-instance 'xml-info-document
                 :title "Djula manual"
                 :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")))

(defun make-xml-info-node (xml)
  (make-instance 'xml-info-node
                 :name (dom:get-attribute xml "name")
                 :node-xml xml
                 :content-xml (xpath:first-node (xpath:evaluate "./following-sibling::*" xml))))

(defmethod contents ((node xml-info-node))
  (content-xml node))

(defmethod node-prev ((node xml-info-node))
  (aand
   (xpath:first-node (xpath:evaluate "./nodeprev/text()" (node-xml node)))
   (dom:data it)))

(defmethod node-next ((node xml-info-node))
  (aand
   (xpath:first-node (xpath:evaluate "./nodenext/text()" (node-xml node)))
   (dom:data it)))

(defmethod node-up ((node xml-info-node))
  (aand
   (xpath:first-node (xpath:evaluate "./nodeup/text()" (node-xml node)))
   (dom:data it)))

(defmethod children ((node xml-info-node))
  (mapcar 'make-xml-info-node (xpath:all-nodes (xpath:evaluate "./node" (content-xml node)))))

(defun parse-xml-content (xml)
  (labels ((make-element (x)
             (if (dom:text-node-p x)
                 (dom:data x)
                 ;; else
                 (list* (alexandria:make-keyword (dom:tag-name x))
                        (loop for child across (dom:child-nodes x)
                              collect (make-element child))))))
    (make-element xml)))

(defmethod html-link ((node info-node))
  (node-name node))

(defun render-xml-content (xml stream &key (split t))
  (who:with-html-output (stream)
    (block quit
      (labels ((render-element (x)
                 (flet ((render ()
                          (loop for child across (dom:child-nodes x)
                                do (render-element child)))
                        (render-menu ()
                          (who:htm
                           (:h2 "Menu")
                           (:ol
                            (loop for menuentry in (xpath:all-nodes (xpath:evaluate "./menuentry" x))
                                  do (let ((node-name (dom:data (xpath:first-node (xpath:evaluate "./menunode/text()" menuentry)))))
                                       (who:htm
                                        (:li (:a :href (substitute #\- #\space node-name)
                                                 (who:str node-name)
                                                 )))))))))
                   (if (dom:text-node-p x)
                       (who:str (dom:data x))
                       ;; else
                       (case (alexandria:make-keyword (dom:tag-name x))
                         (:|para| (who:htm (:p (render))))
                         (:|menu| (render-menu))
                         (:|node| (if split
                                      (return-from quit)
                                      (render-element (break "~a" x))))
                         (:|chapter| (render))
                         (:|section| (render))
                         (:|subsection| (render))
                         (:|sectiontitle| )
                         (:|anchor|)
                         (:|top| (render))
                         (:|unnumbered|)
                         (:|findex|)
                         (:|cindex|)
                         (:|vindex|)
                         (:|table| (who:htm (:table (render))))
                         (:|tableentry| (who:htm (:tr (render))))
                         (:|tableterm| (who:htm (:td (render))))
                         (:|tableitem| (who:htm (:td (render))))
                         (:|item| (who:htm (:td (render))))
                         (:|itemformat| (who:htm (:td (render))))
                         (:|itemize| (who:htm
                                      (:ul (render) )))
                         ((:|itemprepend| :|prepend|))
                         (:|listitem| (who:htm (:li (render))))
                         (:|strong| (who:htm (:b (render))))
                         (:|emph| (who:htm (:emph (render))))
                         (:|ref| "TODO:implement")
                         (:|uref| "TODO: implement" )
                         (:|url| (let ((url (dom:data (dom:first-child (dom:first-child x)))))
                                   (who:htm (:a :href url (who:str url)))))
                         (:|verbatim| (who:htm (:pre (:code :class "hljs"
                                                            (who:str
                                                             (who:escape-string (dom:data (dom:first-child
                                                                                    x))))))))
                         (:|code| (who:htm (:code :class "inline" (who:str (who:escape-string (dom:data (dom:first-child x)))))))
                         (t (error "~a" (dom:tag-name x)))
                         )))))
        (render-element xml)))))

(defgeneric render (thing format stream))

(defmethod render ((node xml-info-node) (format (eql :html)) stream)
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:h1 :class "node-title"
               (who:str (node-title node)))
          (:div :class "node-content"
                (render-xml-content (content-xml node) stream))
          (render-node-navigation node stream))))

(defmethod node-type ((node xml-info-node))
  (alexandria:make-keyword (dom:tag-name (content-xml node))))

(defmethod node-title ((node xml-info-node))
  (dom:data (xpath:first-node (xpath:evaluate "./sectiontitle/text()" (content-xml node)))))

(defmethod find-node ((info-document xml-info-document) node-name)
  (aand (xpath:first-node
         (xpath:evaluate (format nil "//node[@name='~a']" node-name)
                         (xml-document info-document)))
        (make-xml-info-node it)))

(defun render-node-navigation (node stream)
  (who:with-html-output (stream)
    (:header :class "node-navigation"
             (:ul 
              (awhen (node-prev node)
                (who:htm
                 (:li :class "node-prev"
                      ;;(who:str "Previous: ")
                      (:ion-icon :name "arrow-back-circle")
                      (:a :href (substitute #\- #\space it) (who:str it)))))
              (awhen (node-up node)
                (who:htm
                 (:li :class "node-up"
                      (:ion-icon :name "arrow-up-circle")
                      (:a :href (substitute #\- #\space it)
                          (who:str it)))))
              (awhen (node-next node)
                (who:htm
                 (:li :class "node-next"
                      (:ion-icon :name "arrow-forward-circle")
                      (:a :href (substitute #\- #\space it) (who:str it)))))))))

(defclass webinfo-acceptor (hunchentoot:acceptor)
  ((info-document :initarg :info-document
                  :accessor info-document
                  :initform (error "Provide the info-document"))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor webinfo-acceptor) request)
  (let* ((node-name (substitute #\- #\space (remove #\/ (hunchentoot:request-uri request))))
         (node (trivia:match node-name
                 ("" (find-node (info-document acceptor) "Top"))
                 (_ (find-node (info-document acceptor) node-name)))))
    (if (not node)
        (format nil "Not found: ~a" node-name)
        (with-output-to-string (s)
          (webinfo-html s
                        (lambda (stream)
                          (render node :html stream)))))))

(defun webinfo-html (stream body)
  (who:with-html-output (stream)
    (:html
     (:head
      (:title "WebInfo")
      (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.0.3/build/styles/default.min.css")
      (:style
       (who:str "
code.inline { 
   background-color: lightgray;
}
ion-icon {
   color: lightblue;
   font-size: 22px;
}
")))
     (:body
      (funcall body stream)
      (:script :src "https://unpkg.com/ionicons@5.4.0/dist/ionicons.js")
      (:script :src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.0.3/build/highlight.min.js")
      (:script :src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.0.3/build/languages/lisp.min.js")
      (:script (who:str "hljs.initHighlightingOnLoad();"))))))

(defun start-webinfo (&rest args)
  (hunchentoot:start (apply #'make-instance 'webinfo-acceptor args)))

(defun start-demo ()
  (webinfo:start-webinfo :port 9090 :info-document (make-instance 'webinfo:xml-info-document :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml") :title "Djula manual")))
