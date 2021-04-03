(in-package :webinfo)

(defclass xml-info-document (info-document)
  ((xml-document :accessor xml-document :initarg :xml))
  (:documentation "info document loaded from texi2any xml files"))

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
                      :entity-resolver #'resolver)))


  (initialize-indexes info-document))

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

(defmethod all-nodes ((doc xml-info-document))
  (labels ((all-descendants (node)
             (cons node (alexandria:flatten (mapcar #'all-descendants (children node))))))
    (let* ((top-nodes (mapcar 'make-xml-info-node (xpath:all-nodes (xpath:evaluate "/texinfo/node" (xml-document doc))))))
      (alexandria:flatten (mapcar #'all-descendants top-nodes)))))

(defmethod toc ((doc xml-info-document))
  (let* ((top-nodes (mapcar 'make-xml-info-node (xpath:all-nodes (xpath:evaluate "/texinfo/node" (xml-document doc))))))
    (loop for node in top-nodes
          collect (toc node))))

(defun parse-xml-content (xml)
  (labels ((make-element (x)
             (if (dom:text-node-p x)
                 (dom:data x)
                 ;; else
                 (list* (alexandria:make-keyword (dom:tag-name x))
                        (loop for child across (dom:child-nodes x)
                              collect (make-element child))))))
    (make-element xml)))

(defmethod text-contents ((node xml-info-node))
  (let ((text ""))
    (labels ((append-text (x)
             (if (dom:text-node-p x)
                 (setf text (concatenate 'string text (dom:data x)))
                 (loop for child across (dom:child-nodes x)
                       do (append-text child)))))
      (append-text (content-xml node))
      text)))

(defun render-xml-content (xml stream &key (split t))
  (who:with-html-output (stream)
    (block quit
      (labels ((render-element (x)
                 (flet ((render ()
                          (loop for child across (dom:child-nodes x)
                                do (render-element child)))
                        (render-menu ()
                          (who:htm
                           (:ol :class "menu"
                                (loop for menuentry in (xpath:all-nodes (xpath:evaluate "./menuentry" x))
                                      do (let* ((node-name (dom:data (xpath:first-node (xpath:evaluate "./menunode/text()" menuentry))))
                                               (node-url-name (substitute #\- #\space node-name)))
                                           (who:htm
                                            (:li (:a :href (if split
                                                               node-url-name
                                                               (format nil "#~a" node-url-name))
                                                     (who:str node-name)
                                                     )))))))))
                   (cond
                     ((dom:text-node-p x)
                      (who:str (dom:data x)))
                     ((dom:comment-p x)
                      ;; do nothing
                      )
                     (t ;; otherwise
                       (case (alexandria:make-keyword (dom:tag-name x))
                         (:|para| (who:htm (:p (render))))
                         (:|menu| ;;(render-menu)
                          (who:htm (:ul :class "menu" (render)))
                          )
                         (:|menuentry| (who:htm (:li (render))))
                         (:|menunode| (let* ((node-name (dom:data (dom:first-child x)))
                                             (node-url-name (substitute #\- #\space node-name)))
                                        (who:htm (:a :href (if split
                                                               node-url-name
                                                               (format nil "#~a" node-url-name))
                                                     (who:str node-name)))))
                         (:|menudescription| (render))
                         (:|pre| (who:htm (:pre (render))))
                         (:|node| (if split
                                      (return-from quit)
                                      (let ((subnode (make-xml-info-node x)))
                                        (render-subnode subnode nil stream)))) 
                         (:|nodename|)
                         (:|macro|) ;; TODO
                         (:|chapter| (render))
                         ((:|section| :|subsection| :|subsubsection|) (render))
                         (:|sectiontitle| (who:htm (:h1 (render))))
                         (:|anchor|)
                         (:|defvr| (who:htm (:div :class "defvr" (render))))
                         (:|definitionterm| (render))
                         (:|defcategory| (who:htm (:span :class "defcategory" (who:fmt "[~a]" (dom:data (dom:first-child x))))))
                         (:|indexterm|)
                         (:|defvariable| (who:htm (:span :class "defvariable" (who:str (dom:data (dom:first-child x))))))
                         (:|definitionitem| (render))
                         (:|deffn| (who:htm (:div :class "deffn" (render))))
                         (:|deffunction| (who:htm (:span :class "deffunction" (who:str (dom:data (dom:first-child x))))))
                         (:|defdelimiter| (who:str (who:escape-string (dom:data (dom:first-child x)))))
                         (:|defparam| (who:htm (:span :class "defparam" (who:str (dom:data (dom:first-child x))))))
                         (:|deftp| (who:htm (:div :class "deftp" (render))))
                         (:|defdatatype| (who:htm (:span :class "defdatatype" (who:str (dom:data (dom:first-child x))))))
                         (:|top| (render))
                         ((:|unnumbered| :|appendix|) (render))
                         ((:|findex| :|cindex| :|vindex| :|tindex|))
                         (:|printindex| (print-index (get-index (info-repository *webinfo-acceptor*)
                                                                (alexandria:make-keyword (string-upcase (dom:get-attribute x "value"))))
                                                     stream))
                         (:|multitable| ;; TODO (who:htm (:table (render)))
                          )
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
                         (:|quotation| (who:htm (:quote (render))))
                         (:|sc| (who:str (dom:data (dom:first-child x)))) ;; smallcaps
                         (:|ref| "TODO:implement")
                         (:|uref| "TODO: implement" )
                         (:|url| (let ((url (dom:data (dom:first-child (dom:first-child x)))))
                                   (who:htm (:a :href url (who:str url)))))
                         (:|verbatim| (who:htm (:pre (:code :class "hljs"
                                                            (who:str
                                                             (who:escape-string (dom:data (dom:first-child
                                                                                           x))))))))
                         (:|code| (who:htm (:code :class "inline" (render))))
                         (:|w| (who:str (who:escape-string (dom:data (dom:first-child x)))))
                         (t (error "~a" (dom:tag-name x)))
                         ))))))
        (render-element xml)))))

(defmethod render-node ((node xml-info-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:div :class "node-content"
                (render-xml-content (content-xml node) stream :split nil))
          (render-node-navigation node stream))))

(defmethod render-subnode ((node xml-info-node) theme stream)
  (who:with-html-output (stream)
    (:div :class "subnode"
          :id (node-name node)
          (:div :class "node-content"
                (render-xml-content (content-xml node) stream :split nil)))))

(defmethod node-type ((node xml-info-node))
  (alexandria:make-keyword (dom:tag-name (content-xml node))))

(defmethod node-title ((node xml-info-node))
  (dom:data (xpath:first-node (xpath:evaluate "./sectiontitle/text()" (content-xml node)))))

(defmethod find-node ((info-document xml-info-document) node-name)
  (aand (xpath:first-node
         (xpath:evaluate (format nil "//node[@name='~a']" node-name)
                         (xml-document info-document)))
        (make-xml-info-node it)))

(defmethod collect-indexes ((node xml-info-node) index-type)
  (let ((node-name (string-downcase (string index-type))))
    (mapcar (lambda (xml-index)
              (dom:data (xpath:first-node (xpath:evaluate "./indexterm/text()" xml-index))))
            (xpath:all-nodes
             (xpath:evaluate (format nil ".//~a" node-name)
                             (content-xml node))))))
