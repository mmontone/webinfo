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
   (indexes :accessor indexes
            :initform nil)
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

(defclass info-repository ()
  ())

(defclass dir-info-repository ()
  ((dir :initarg :dir
        :accessor dir)))

(defclass file-info-repository ()
  ((file :initarg :file
         :accessor file)))

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

(defgeneric info-document-for-uri (info-repository uri))
(defmethod info-document-for-uri ((repo file-info-repository) uri)
  (file repo))

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

(defmethod toc ((node info-node))
  (cons node
        (loop for child in (children node)
              collect (toc child))))

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
                           (:ol :class "menu"
                                (loop for menuentry in (xpath:all-nodes (xpath:evaluate "./menuentry" x))
                                      do (let ((node-name (dom:data (xpath:first-node (xpath:evaluate "./menunode/text()" menuentry)))))
                                           (who:htm
                                            (:li (:a :href (substitute #\- #\space node-name)
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
                         (:|menunode| (let ((node-name (dom:data (dom:first-child x))))
                                        (who:htm (:a :href (substitute #\- #\space node-name)
                                                     (who:str node-name)))))
                         (:|menudescription| (render))
                         (:|pre| (who:htm (:pre (render))))
                         (:|node| (if split
                                      (return-from quit)
                                      (render-element (break "~a" x))))
                         (:|macro|) ;; TODO
                         (:|chapter| (render))
                         (:|section| (render))
                         (:|subsection| (render))
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

(defgeneric render-node (thing theme stream &rest args))

(defmethod render-node ((node xml-info-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
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

(defmethod render-node-navigation (node stream)
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

(defmethod find-node ((info-repository file-info-repository) name)
  (find-node (file info-repository) name))

(defmethod find-node ((info-repository dir-info-repository) name)
  (bind:bind (((manual-name node-name) (split-sequence:split-sequence #\/ name)))
    (let ((info-document (find-info-document info-repository manual-name)))
      (find-node info-document node-name))))

;; Indexes
(defmethod collect-indexes ((info-document info-document) index-type)
  (let ((doc-indexes (list)))
    (loop for node in (all-nodes info-document)
          for node-indexes := (collect-indexes node index-type)
          do
             (loop for node-index in node-indexes
                   do (push (cons node-index node) doc-indexes)))
    doc-indexes))

(defmethod collect-indexes ((node xml-info-node) index-type)
  (let ((node-name (string-downcase (string index-type))))
    (mapcar (lambda (xml-index)
              (dom:data (xpath:first-node (xpath:evaluate "./indexterm/text()" xml-index))))
            (xpath:all-nodes
             (xpath:evaluate (format nil ".//~a" node-name)
                             (content-xml node))))))

(defun initialize-indexes (info-document)
  (setf (aget (indexes info-document) :fn)
        (collect-indexes info-document :findex))
  (setf (aget (indexes info-document) :cp)
        (collect-indexes info-document :cindex))
  (setf (aget (indexes info-document) :vr)
        (collect-indexes info-document :vindex)))

#+nil(defmethod initialize-instance :after ((info-document info-document) &rest initargs)
       (declare (ignore initargs))
       (initialize-indexes info-document))

(defmethod get-index ((doc info-document) index-type)
  (aget (indexes doc) index-type))

(defmethod get-index ((repo file-info-repository) index-type)
  (get-index (file repo) index-type))

(defmethod search-index ((doc info-document) term &key index-type)
  (if index-type
      (loop for (name . node) in (get-index doc index-type)
            when (search term name :test 'equalp)
              collect (cons name node))
      ;; else
      (loop for (name . node) in (apply #'append (mapcar 'cdr (indexes doc)))
            when (search term name :test 'equalp)
              collect (cons name node))))

(defmethod search-index ((repo file-info-repository) term &key index-type)
  (search-index (file repo) term :index-type index-type))

(defun print-index (index stream)
  (who:with-html-output (stream)
    (if (alexandria:emptyp index)
        (who:htm (:p (who:str "No entries")))
        (who:htm
         (:ul :class "menu"
              (loop for (name . node) in index do
                (who:htm (:li (:a :href (node-name node)
                                  (who:str name))
                              (who:str (node-title node))))))))))

(defclass index-matches-node (info-node)
  ((seach-term :initarg :search-term :accessor search-term)
   (matches :initarg :matches :accessor matches)))

(defmethod render-node-navigation ((node index-matches-node) stream)
  )

(defmethod render-node ((node index-matches-node) theme stream &rest args)
  (declare (ignore args))
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:h1 "Index search")
          (if (alexandria:emptyp (matches node))
              (who:htm (:p "No matches"))
              (who:htm
               (:div :class "node-content"
                     (:ul :class "index-matches"
                          (loop for (term . indexed-node) in (matches node) do
                            (who:htm (:li (:a :href (node-name indexed-node)
                                              (who:str term))
                                          (who:str (node-title indexed-node)))))))))
          (render-node-navigation node stream))))

;; Web

(defun webinfo-html (stream body)
  (let ((theme (app-setting :theme)))
    (who:with-html-output (stream)
      (:html
       (:head
        (:title "WebInfo")
        (when theme
          (add-theme-styles theme stream)))
       (:body
        (funcall body stream)
        (when theme
          (add-theme-html theme stream)
          (add-theme-scripts theme stream)))))))

(defclass theme ()
  ((name :initarg :name
         :accessor theme-name)))

(defgeneric add-theme-styles (theme stream))
(defgeneric add-theme-scripts (theme stream))
(defgeneric add-theme-html (theme stream))

(defmethod add-theme-styles (theme stream))
(defmethod add-theme-scripts (theme stream))
(defmethod add-theme-html (theme stream))

(defgeneric initialize-theme (theme app))
(defmethod initialize-theme (theme app))

;; Simple theme
(defclass simple-theme (theme)
  ()
  (:default-initargs
   :name "Simple"))

(defmethod add-theme-styles ((theme simple-theme) stream)
  (who:with-html-output (stream)
    #+nil(:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/mini.css/3.0.1/mini-default.min.css")
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    #+nil(:link :rel "stylesheet" :href "https://www.w3schools.com/w3css/4/w3.css")
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
header.node-navigation ul {
   list-style-type:none;
   padding-left:0;
}
header.node-navigation li {
   display:inline-block;
   padding-right: 20px;
   line-height: 22px;
}
header.node-navigation li a {
   line-height: 22px;
   vertical-align: top;
}
header.node-navigation {
   border-bottom: 1px solid lightgray;
   position: fixed;
   top: 0;
   width: 100%;
   background-color: white;
}
div.node {
   padding-top: 50px;
}
"))))

(defmethod add-theme-scripts ((theme simple-theme) stream)
  (who:with-html-output (stream)
    (:script :src "https://unpkg.com/ionicons@5.4.0/dist/ionicons.js")
    (:script :src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.0.3/build/highlight.min.js")
    (:script :src "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.0.3/build/languages/lisp.min.js")
    (:script (who:str "hljs.initHighlightingOnLoad();"))))

;; ebook theme
(defclass ebook-theme (theme)
  ()
  (:default-initargs
   :name "eBook"))

(defclass nav-theme (simple-theme)
  ((toc :initarg :toc :accessor toc))
  (:default-initargs
   :name "Nav"))

(defmethod render-node :before (node (theme nav-theme) stream &rest args)
  (let ((info-doc (first args)))
    (render-navigation-sidebar info-doc stream)))

(defun render-navigation-sidebar (doc stream)
  (who:with-html-output (stream)
    (:div :class "navsidebar"
          (:div :class "search"
                (:form :action "_is"
                       (:input :name "q")))
          (render-toc (toc doc) stream)
          (:div :class "settings"
                (:a :href "/"
                    (:ion-icon :style "font-size: 32px;" :name "home-outline"))
                (:a :href "_settings"
                    (:ion-icon :style "font-size: 32px;" :name "settings-outline"))
                ))))

(defun render-toc (toc stream)
  (who:with-html-output (stream)
    (labels ((render-toc-level (levels)
               (who:htm (:ul
                         (loop for level in levels
                               when (not (null level)) ;; TODO: FIX
                                 do (who:htm
                                     (:li (:a :href (node-name (first level))
                                              (who:str (node-title (first level))))
                                          (render-toc-level (cdr level)))))))))
      (who:htm
       (:ul :class "toc"
            (loop for level in toc
                  do
                     (who:htm
                      (:li (:a :href (node-name (car level))
                               (who:str (node-title (car level))))
                           (render-toc-level (cdr level))))))))))

(defmethod add-theme-styles :after ((theme nav-theme) stream)
  (who:with-html-output (stream)
    (:style (who:str "
  /* The sidebar menu */
.navsidebar {
  height: 100%;
  margin-bottom: -50px;
  width: 220px; /* Set the width of the sidebar */
  position: fixed; /* Fixed Sidebar (stay in place on scroll) */
  z-index: 1; /* Stay on top */
  top: 0; /* Stay at the top */
  left: 0;
  overflow-x: hidden; /* Disable horizontal scroll */
  background-color:white;
  padding-top: 20px;
  padding-left: 10px;
  border-right: 2px solid lightgray;
}
.navsidebar .settings {
  height: 50px; border-top: 1px solid gray; background-color:white;
  padding: 10px;
}
.navsidebar .settings a {
  margin-right: 8px;
}
.search input {
  width: 200px;
}
ul.toc, ul.toc ul {
  list-style-type: none;
  padding-left: 10px;
}

.toc a {
  text-decoration:none;
  color: black;
}

.toc a:hover {
  background-color: lightblue;
  width: 100%;
}

.navsidebar .toc {
  overflow-y: scroll;
  height: calc(100vh - 140px);
}

.node {
  padding-left: 250px;
}

.deffn, .defvr, .deftp {
  background-color: aliceblue;
  padding: 5px 10px 1px 10px;
  margin-bottom: 10px;
  border: 1px solid lightgray;
  // border-left: 5px solid darkblue;
}
.defvariable, .deffunction, .defdatatype {
  font-weight: bold;
}
.defcategory {
  color: purple;
}
"))))

(defparameter *themes* (list (make-instance 'simple-theme)))

(defmethod make-index-search-node ((repo file-info-repository) uri)
  (let ((params (quri:uri-query-params uri)))
    (alexandria:if-let ((search-term (aget params "q")))
      (make-instance 'index-matches-node
                     :name "Index matches"
                     :search-term search-term
                     :matches (search-index repo search-term))
      (find-node (file repo) "Top"))))      

(defvar +app-settings+
  `((use-icons :type boolean :label "Use icons" :default t)
    (highlight-code :type boolean :label "Highlight code" :default t)
    (theme :type option :label "Theme" :options ,(mapcar 'theme-name *themes*))))

(defun app-setting (name &optional (acceptor hunchentoot:*acceptor*))
  (access:access (app-settings acceptor) name))

(defun set-app-setting (name value)
  (setf (access:access (app-settings hunchentoot:*acceptor*) name) value))

(defsetf app-setting set-app-setting)

(defvar +default-app-settings+ (list (cons :theme (make-instance 'simple-theme))))

(defclass settings-info-node (info-node)
  ())

(defmethod render-node ((node settings-info-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
          (:h1 (who:str "Settings")))))

(defclass webinfo-acceptor (hunchentoot:acceptor)
  ((info-repository :initarg :info-repository
                    :accessor info-repository
                    :initform (error "Provide the info-repository"))
   (app-settings :initarg :app-settings
                 :accessor app-settings
                 :initform +default-app-settings+)))

(defmethod initialize-instance :after ((acceptor webinfo-acceptor) &rest initargs)
  (declare (ignore initargs))
  (awhen (app-setting :theme acceptor)
    (initialize-theme it acceptor)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor webinfo-acceptor) request)
  (let* ((uri (quri:uri (hunchentoot:request-uri request)))
         (node (trivia:match (quri:uri-path uri)
                 ((or nil "" "/") (find-node (info-repository acceptor) "Top"))
                 ((or "_is" "/_is") (make-index-search-node (info-repository acceptor) uri))
                 ((or "_dir" "/_dir") (make-dir-node (info-repository acceptor) request))
                 ((or "_settings" "/_settings")
                  (trivia:match (hunchentoot:request-method request)
                    (:get (make-instance 'settings-info-node))
                    (:post (save-settings acceptor request) nil)))
                 (_
                  (let ((node-name (substitute #\- #\space (remove #\/ (quri:uri-path uri)))))
                    (find-node (info-repository acceptor) node-name)))))
         (info-document (info-document-for-uri (info-repository acceptor)
                                               uri)))
    (if (not node)
        (format nil "Not found")
        (with-output-to-string (s)
          (webinfo-html s
                        (lambda (stream)
                          (render-node node (app-setting :theme acceptor) stream  info-document)))))))

(defvar *webinfo-acceptor*)

(defun start-webinfo (&rest args)
  (setf *webinfo-acceptor*
        (hunchentoot:start (apply #'make-instance 'webinfo-acceptor args))))

(defun stop-webinfo ()
  (hunchentoot:stop *webinfo-acceptor*))

(defun start-demo (&rest args)
  (webinfo:start-webinfo
   :port 9090
   :info-repository
   (make-instance 'file-info-repository
                  :file
                  (make-instance 'webinfo:xml-info-document
                                 :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                 :title "Djula manual"))
   :app-settings (list (cons :theme (make-instance 'nav-theme)))))
