;;;; webinfo.lisp

(in-package #:webinfo)

(defclass info-document ()
  ((name :initarg :name
         :accessor document-name
         :initform (error "Provide the document name"))
   (filepath :initarg :filepath
             :accessor filepath)
   (file :initarg :file
         :accessor file)
   (title :initarg :title
          :accessor title
          :initform (error "Provide a title for the document"))
   (description :initarg :description
                :accessor description
                :initform "")
   (indexes :accessor indexes
            :initform nil)))

(defclass info-node ()
  ((name :initarg :name :accessor node-name
         :initform (error "Provide a name for the node"))
   (description :initarg :description
                :accessor description
                :initform "")))

(defmethod print-object ((node info-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a" (node-name node))))

(defclass info-repository ()
  ())

(defclass dir-info-repository (info-repository)
  ((dir :initarg :dir
        :accessor dir
        :type (or list pathname)
        :documentation "DIR can be either a directory (as a Lisp pathname), a list of pathnames, or a list of INFO-DOCUMENTs"))
  (:documentation "An info repository of multiple documents, listed in a virtual 'dir' top-level node"))

(defclass file-info-repository (info-repository)
  ((file :initarg :file
         :accessor file
         :type (or pathname info-document)
         :documentation "FILE can be either a pathname pointing to a WEBINFO file, or an INFO-DOCUMENT object"))
  (:documentation "A repository of a single file"))

(defgeneric home-node (info-document))
(defgeneric all-nodes (info-document))
(defgeneric top-nodes (info-document))

(defmethod home-node ((repo file-info-repository))
  (find-node (file repo) "Top"))

(defmethod home-node ((repo dir-info-repository))
  (make-instance 'dir-node
                 :dir (dir repo)
                 :name "dir"))

(defmethod top-nodes ((doc info-document))
  (remove-if-not
   (lambda (node)
     (string= (node-up node) "Top"))
   (all-nodes doc)))

(defgeneric info-document-for-uri (info-repository uri))
(defmethod info-document-for-uri ((repo file-info-repository) uri)
  (file repo))

(defmethod info-document-for-uri ((repo dir-info-repository) uri)
  (let ((doc-name (first (split-sequence:split-sequence #\/ (quri:uri-path uri) :remove-empty-subseqs t))))
    (find-info-document repo doc-name)))

(defun find-info-document (dir-info-repository doc-name &key (errorp t))
  (or (find doc-name (dir dir-info-repository) :key 'document-name :test 'equalp)
      (and errorp (error "Document not found: ~a" doc-name))))

(defmethod toc ((node info-node))
  (cons node
        (loop for child in (children node)
              collect (toc child))))

(defmethod html-link ((node info-node))
  (node-name node))

(defgeneric render-node (thing theme stream &rest args))

(defmethod render-node-navigation (node stream)
  (who:with-html-output (stream)
    (:header :class "node-navigation"
             (:ul
              (awhen (node-prev node)
                (who:htm
                 (:li :class "node-prev"
                      (:i :class "bi-arrow-left-circle" :style "margin-right: 5px;")
                      (:a :href (substitute #\- #\space it) (who:str it)))))
              (awhen (node-up node)
                (who:htm
                 (:li :class "node-up"
                      (:i :class "bi-arrow-up-circle" :style "margin-right: 5px;")
                      (:a :href (substitute #\- #\space it)
                          (who:str it)))))
              (awhen (node-next node)
                (who:htm
                 (:li :class "node-next"
                      (:i :class "bi-arrow-right-circle" :style "margin-right: 5px;")
                      (:a :href (substitute #\- #\space it) (who:str it)))))))))

(defmethod find-node ((info-repository file-info-repository) name)
  (find-node (file info-repository) name))

(defmethod find-node ((info-repository dir-info-repository) name)
  (bind:bind (((manual-name &optional (node-name "Top")) (split-sequence:split-sequence #\/ name :remove-empty-subseqs t)))
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

(defmethod search-index ((repo dir-info-repository) term &key index-type)
  (loop for doc in (dir repo)
        appending (search-index doc term :index-type index-type)))

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

(defmethod search-topics ((doc info-document) term)
  (loop for node in (all-nodes doc)
        when (search term (node-title node) :test 'equalp)
          collect (cons (node-title node)
                        node)))

(defmethod search-topics ((repo file-info-repository) term)
  (search-topics (file repo) term))

(defmethod search-topics ((repo dir-info-repository) term)
  (loop for doc in (dir repo)
        appending (search-topics doc term)))

(defclass index-matches-node (info-node)
  ((seach-term :initarg :search-term :accessor search-term)
   (matches :initarg :matches :accessor matches)))

(defmethod toc ((doc info-document))
  (loop for node in (top-nodes doc)
        collect (toc node)))

(defmethod render-node-navigation ((node index-matches-node) stream)
  )

(defmethod render-node ((node index-matches-node) theme stream &rest args)
  (declare (ignore args))
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:h1 "Index matches")
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

(defclass search-node (info-node)
  ((search-term :initarg :search-term :accessor search-term
                :initform (error "Provide the search term"))
   (place :initarg :place :accessor place
          :initform (error "Provide the place where to do the search"))
   (index-type :initarg :index-type
               :initform nil
               :accessor index-type)
   (index-matches :accessor index-matches)
   (topics-matches :accessor topics-matches))
  (:documentation "Search both index and topics at the same time"))

(defmethod initialize-instance :after ((node search-node) &rest initargs)
  (setf (index-matches node)
        (search-index (place node) (search-term node) :index-type (index-type node)))
  (setf (topics-matches node)
        (search-topics (place node) (search-term node))))

(defmethod render-node-navigation ((node search-node) stream)
  )

(defmethod render-node ((node search-node) theme stream &rest args)
  (declare (ignore args))
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:h1 "Index matches")
          (if (alexandria:emptyp (index-matches node))
              (who:htm (:p "No matches"))
              (who:htm
               (:div :class "node-content"
                     (:ul :class "index-matches"
                          (loop for (term . indexed-node) in (index-matches node) do
                            (who:htm (:li (:a :href (node-name indexed-node)
                                              (who:str term))
                                          (who:str (node-title indexed-node)))))))))
          (:h1 "Topics matches")
          (if (alexandria:emptyp (topics-matches node))
              (who:htm (:p "No matches"))
              (who:htm
               (:div :class "node-content"
                     (:ul :class "index-matches"
                          (loop for (term . topic-node) in (topics-matches node) do
                            (who:htm (:li (:a :href (node-name topic-node)
                                              (who:str term))
                                          (who:str (node-title topic-node)))))))))
          (:form :action "_fts"
                 (:input :type "hidden" :name "q" :value (search-term node))
                 (:input :type "submit" :value "Full text search"))
          (render-node-navigation node stream))))

(defclass dir-node (info-node)
  ((dir :initarg :dir :accessor dir)))

(defmethod render-node ((node dir-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:h1 (who:str "(dir)Top"))
    (:p (who:str "This (the Directory node) gives a menu of major topics."))
    (:div :class "search"
          (:form :action "_s"
                 (:input :name "q" :placeholder "Search ...")))
    (:ul :class "menu"
         (loop for doc in (dir node)
               do
                  (who:htm (:li (:a :href (format nil "~a/" (document-name doc))
                                    (who:str (title doc)))
                                (who:str (description doc))))))))

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
    (:link :rel "stylesheet" :href "/public/highlightjs/styles/default.css")
    (:link :rel "stylesheet" :href "/public/node_modules/bootstrap-icons/font/bootstrap-icons.css")
    
    (:style
     (who:str "
code.inline {
   background-color: lightgray;
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
    (:script :src "/public/highlightjs/highlight.pack.js")
    (:script (who:str "hljs.highlightAll();"))))

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
    (when info-doc
      (render-navigation-sidebar info-doc stream))))

(defun render-navigation-sidebar (doc stream)
  (who:with-html-output (stream)
    (:div :class "navsidebar"
          (:div :class "search"
                (:form :action "_s"
                       (:input :name "q" :placeholder "Search ...")))
          (render-toc (toc doc) stream)
          (:div :class "settings"
                (:a :href "/" :alt "Home"
                    (:i :class "bi-house-fill" :style "font-size: 2rem;"))
                (:a :href "_settings" :alt "Settings"
                    (:i :class "bi-gear" :style "font-size: 2rem;"
                        :name "settings-outline"))
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
  margin-right: 5px;
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

(defmethod make-search-node ((repo info-repository) uri)
  (let ((params (quri:uri-query-params uri)))
    (alexandria:if-let ((search-term (aget params "q")))
      (make-instance 'search-node
                     :name "Search matches"
                     :search-term search-term
                     :place repo)
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

(defclass webinfo-acceptor (hunchentoot:easy-acceptor)
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
  (bind:bind
      ((uri (quri:uri (hunchentoot:request-uri request)))
       ((:values node doc)
        (trivia:match (quri:uri-path uri)
          ((or nil "" "/") (home-node (info-repository acceptor)))
          ((or "_is" "/_is") (make-index-search-node (info-repository acceptor) uri))
          ((or "_s" "/_s") (make-search-node (info-repository acceptor) uri))
          ((or "_fts" "/_fts") (make-instance 'fulltext-search-node
                                              :name "Fulltext search"
                                              :search-term (aget (quri:uri-query-params uri) "q")))
          ((or "_dir" "/_dir") (make-dir-node (info-repository acceptor) request))
          ((or "_settings" "/_settings")
           (trivia:match (hunchentoot:request-method request)
             (:get (make-instance 'settings-info-node :name "Settings"))
             (:post (save-settings acceptor request) nil)))
          (_
           (let* ((clean-url (remove #\/ (quri:uri-path uri) :count 1))
                  (node-name (substitute #\- #\space clean-url)))
             (values (find-node (info-repository acceptor) node-name)
                     (info-document-for-uri (info-repository acceptor)
                                            uri)
                     ))))))
    (if (not node)
        (call-next-method)
        (with-output-to-string (s)
          (webinfo-html s
                        (lambda (stream)
                          (render-node node (app-setting :theme acceptor) stream  doc)))))))

(push 
 (hunchentoot:create-folder-dispatcher-and-handler
  "/public/"
  (asdf:system-relative-pathname :webinfo "public/"))
 hunchentoot:*dispatch-table*)

(defvar *webinfo-acceptor*)

(defun start-webinfo (&rest args)
  (setf *webinfo-acceptor*
        (hunchentoot:start (apply #'make-instance 'webinfo-acceptor args))))

(defun stop-webinfo ()
  (hunchentoot:stop *webinfo-acceptor*))

(defun start-doc-demo (&rest args)
  (let ((djula-manual (make-instance 'webinfo:xml-info-document
                                     :name "djula"
                                     :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                     :title "Djula manual")))
    (fulltext-index-document djula-manual)
    
    (webinfo:start-webinfo
     :port 9090
     :info-repository
     (make-instance 'file-info-repository
                    :file djula-manual)
     :app-settings (list (cons :theme (make-instance 'nav-theme))))))

(defun start-dir-demo (&rest args)
  (let ((djula-manual (make-instance 'webinfo:xml-info-document
                                     :name "djula"
                                     :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                     :title "Djula manual"))
        (djula-ref (make-instance 'webinfo:xml-info-document
                                  :name "djula-ref"
                                  :filepath (asdf:system-relative-pathname :webinfo "test/djula-ref.xml")
                                  :title "Djula reference")))
    (fulltext-index-document djula-manual)
    (fulltext-index-document djula-ref)
    
    (webinfo:start-webinfo
     :port 9090
     :info-repository
     (make-instance 'dir-info-repository
                    :dir (list djula-manual djula-ref))
     :app-settings (list (cons :theme (make-instance 'nav-theme))))))
