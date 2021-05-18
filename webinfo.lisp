;;;; webinfo.lisp

(in-package #:webinfo)

(defclass info-document ()
  ((name :initarg :name
         :accessor document-name
         :initform (error "Provide the document name"))
   (filepath :initarg :filepath
             :accessor filepath
             :documentation "The filepath of the info document.")
   (file :initarg :file
         :accessor file)
   (title :initarg :title
          :accessor title
          :initform (error "Provide a title for the document"))
   (direntry :initarg :direntry
             :accessor direntry
             :initform nil
             :documentation "The directory entry of the info document.
See: https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Installing-Dir-Entries.html")
   (description :initarg :description
                :accessor description
                :initform "")
   (indexes :accessor indexes
            :initform nil)))

(defmethod print-object ((info-document info-document) stream)
  (print-unreadable-object (info-document stream :type t :identity t)
    (write-string (document-name info-document) stream)))

(defclass info-node ()
  ((name :initarg :name :accessor node-name
         :initform (error "Provide a name for the node"))
   (description :initarg :description
                :accessor description
                :initform "")))

(defmethod print-object ((node info-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~a" (node-name node))))

(defgeneric text-contents (info-node)
  (:documentation "Returns the NODE contents as plain text.
This is useful for fulltext indexing.

See FULLTEXT-INDEX-DOCUMENT.
"))

(defclass info-repository ()
  ())

(defclass dir-info-repository (info-repository indexable-info-repository)
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

(defmethod dir ((info-repository file-info-repository))
  (list  (file info-repository)))

(defmethod info-document ((info-repository file-info-repository))
  (file info-repository))

(defgeneric dir-category (info-document)
  (:documentation "The category in dir node for INFO-DOCUMENT"))
(defgeneric dir-entry (info-document)
  (:documentation "direntry spec of INFO-DOCUMENT"))
(defgeneric texinfo-filename (info-document)
  (:documentation "The filename specified in Texinfo source of INFO-DOCUMENT"))

(defgeneric home-node (info-repository)
  (:documentation "Home node for INFO-REPOSITORY"))
(defgeneric all-nodes (info-document)
  (:documentation "Returns all of the nodes in INFO-DOCUMENT, including children."))
(defgeneric top-nodes (info-document)
  (:documentation "Returns top-level nodes of INFO-DOCUMENT"))

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

(defmethod find-document ((info-repository dir-info-repository) doc-name &key (errorp t))
  (or (find doc-name (dir info-repository) :key 'document-name :test 'equalp)
      (and errorp (error "Document not found: ~a" doc-name))))

(defmethod toc ((node info-node))
  (cons node
        (loop for child in (children node)
              collect (toc child))))

(defmethod html-link ((node info-node))
  (node-name node))

(defgeneric node-source (node))

(defgeneric render-node (node media-type stream &rest args))

(defgeneric render-node-html (node theme stream &rest args))

(defmethod render-node (node (media-type (eql :html)) stream &rest args)
  (apply #'render-node-html node (app-setting :theme) stream args))

(defmethod render-node (node (media-type (eql :source)) stream &rest args)
  (write-string (node-source node) stream))

(defmethod render-node-navigation (node stream)
  (who:with-html-output (stream)
    (:header :class "node-navigation"
             (:ul
              (awhen (node-prev node)
                (let ((prev-node (find-node *info-document* it)))
                  (when prev-node
                    (who:htm
                     (:li :class "node-prev"
                          (:i :class "bi-arrow-left-circle" :style "margin-right: 5px;")
                          (:a :href (hunchentoot:url-encode it)
                              (who:str (node-title prev-node))))))))
              (awhen (node-up node)
                (let ((up-node (find-node *info-document* it)))
                  (when up-node
                    (who:htm
                     (:li :class "node-up"
                          (:i :class "bi-arrow-up-circle" :style "margin-right: 5px;")
                          (:a :href (hunchentoot:url-encode it)
                              (who:str (node-title up-node))))))))
              (awhen (node-next node)
                (let ((next-node (find-node *info-document* it)))
                  (when next-node
                    (who:htm
                     (:li :class "node-next"
                          (:i :class "bi-arrow-right-circle" :style "margin-right: 5px;")
                          (:a :href (hunchentoot:url-encode it)
                              (who:str (node-title next-node))))))))))))

(defmethod find-node ((info-repository file-info-repository) name)
  (find-node (file info-repository) name))

(defmethod find-node ((info-repository dir-info-repository) name)
  (trivia:match (split-sequence:split-sequence #\/ name :remove-empty-subseqs t)
    ((list manual-name)
     (alexandria:when-let
         ((info-document (find-document info-repository manual-name)))
       (find-node info-document "Top")))
    ((list manual-name node-name)
     (alexandria:when-let
         ((info-document (find-document info-repository manual-name)))
       (find-node info-document node-name)))))

;; Indexes
(defgeneric collect-indexes (info-document index-type)
  (:documentation "Collect indexes in NODE of type INDEX-TYPE.

INDEX-TYPE can be :findex, :cindex, :vindex, :tindex, etc.

The returned list is an alist with (indexterm . node) items."))

(defmethod collect-indexes ((info-document info-document) index-type)
  (let ((doc-indexes (list)))
    (loop for node in (all-nodes info-document)
          for node-indexes := (collect-indexes node index-type)
          do
             (loop for node-index in node-indexes
                   do (push (cons node-index node) doc-indexes)))
    doc-indexes))

(defun initialize-indexes (info-document)
  "Initialize the index of INFO-DOCUMENT.

Indexes are collected in an a list with (<index-type> . <indexes>),
where indexes is a list of (<index-term> . <node>), and where
<index-term> appears repeated in the list for each appearance of the index term."
  (setf (indexes info-document) nil)
  (setf (aget (indexes info-document) :fn)
        (collect-indexes info-document :findex))
  (setf (aget (indexes info-document) :cp)
        (collect-indexes info-document :cindex))
  (setf (aget (indexes info-document) :vr)
        (collect-indexes info-document :vindex))
  (setf (aget (indexes info-document) :tp)
        (collect-indexes info-document :tindex))
  t)

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
  (let ((grouped-index (groupby:groupby (lambda (index)
                                          (aref (first index) 0))
                                        index)))
    (setf grouped-index (sort grouped-index '< :key (lambda (x)
                                                      (char-code (first x)))))
    (who:with-html-output (stream)
      (if (alexandria:emptyp index)
          (who:htm (:p (who:str "No entries")))
          (who:htm
           (:table :class "index"
                   (:tbody
                    (loop for group in grouped-index
                          do
                             (who:htm
                              (:tr
                               (:th :id (format nil "index-group-~a" (first group))
                                    (who:fmt "~a" (first group)))
                               (:td)
                               (:td)))
                             (loop for (name . node) in (cadr group) do
                               (who:htm
                                (:tr
                                 (:td)
                                 (:td  (:a :href (node-name node)
                                           (who:str name)))
                                 (:td (who:str (node-title node))))))
                             (who:htm
                              (:tr (:td :colspan "4"
                                        (:hr))))
                          ))))))))

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

(defmethod render-node-html ((node index-matches-node) theme stream &rest args)
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

(defmethod render-node-html ((node search-node) theme stream &rest args)
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

(defmethod render-node-html ((node dir-node) theme stream &rest args)
  (declare (ignore args))
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
p {
  max-width: 80ch;
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

(defun render-navigation-sidebar-p (args)
  (and (getf args :document)
       (or (not (boundp 'hunchentoot:*request*))
           (null (hunchentoot:get-parameter "_c")))))

(defmethod render-node-html :before (node (theme nav-theme) stream &rest args)
  "We render navigation sidebar in this wrapper method when :document is passed in ARGS"
  (when (render-navigation-sidebar-p args)
    (render-navigation-sidebar (getf args :document) stream)))

;; The calculation and rendering of table of contents is expensive
;; for big documents, so we cache it.
(defparameter *toc-cache* (make-hash-table :test 'equalp))

(defun render-navigation-sidebar (doc stream)
  (who:with-html-output (stream)
    (:div :class "navsidebar"
          (:div :class "search"
                (:form :action "_s"
                       (:input :name "q" :placeholder "Search ...")))
          (render-toc-cached doc stream)
          (:div :class "settings"
                (:a :href "/" :alt "Home"
                    (:i :class "bi-house-fill" :style "font-size: 2rem;"))
                (:a :href "/_settings" :alt "Settings"
                    (:i :class "bi-gear" :style "font-size: 2rem;"
                        :name "settings-outline"))
                ))))

(defun render-toc-cached (doc stream)
  (when (not (gethash (document-name doc) *toc-cache*))
    (let ((rendered-toc (with-output-to-string (s)
                          (render-toc doc s))))
      (setf (gethash (document-name doc) *toc-cache*)
            rendered-toc)))
  (write-string  (gethash (document-name doc) *toc-cache*) stream))

(defun render-toc (doc stream)
  (who:with-html-output (stream)
    (labels ((render-toc-level (levels)
               (who:htm
                (:ul
                 (loop for level in levels
                       when (not (null level)) ;; TODO: FIX
                         do (who:htm
                             (:li (:a :href (hunchentoot:url-encode (node-name (first level)))
                                      (who:str (node-title (first level))))
                                  (render-toc-level (cdr level)))))))))
      (who:htm
       (:ul :class "toc"
            (loop for level in (toc doc)
                  do
                     (who:htm
                      (:li (:a :href (hunchentoot:url-encode (node-name (car level)))
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

(defgeneric make-search-node (info-repository uri)
  (:documentation "Build a virtual node for searching INFO-REPOSITORY."))

(defmethod make-search-node ((repo info-repository) uri)
  (let ((params (quri:uri-query-params uri)))
    (alexandria:if-let ((search-term (aget params "q")))
      (make-instance 'search-node
                     :name "Search matches"
                     :search-term search-term
                     :place repo)
      ;; else
      (find-node (file repo) "Top"))))

(defmethod make-search-node ((doc info-document) uri)
  "Create a virtual node for searching a document"
  (let ((params (quri:uri-query-params uri)))
    (alexandria:if-let ((search-term (aget params "q")))
      (make-instance 'search-node
                     :name "Search matches"
                     :search-term search-term
                     :place doc)
      ;; else
      (find-node doc "Top"))))

(defgeneric make-index-search-node (info-repository uri)
  (:documentation "Build a virtual node for searching INFO-REPOSITORY index."))

(defmethod make-index-search-node ((repo file-info-repository) uri)
  (let ((params (quri:uri-query-params uri)))
    (alexandria:if-let ((search-term (aget params "q")))
      (make-instance 'index-matches-node
                     :name "Index matches"
                     :search-term search-term
                     :matches (search-index repo search-term))
      (find-node (file repo) "Top"))))

(defgeneric make-fulltext-search-node (info-repository uri)
  (:documentation "Build a virtual node for fulltext searching in INFO-REPOSITORY"))

(defmethod make-fulltext-search-node ((repo info-repository) uri)
  (make-instance 'fulltext-search-node
                 :name "Fulltext search"
                 :search-term (aget (quri:uri-query-params uri) "q")
                 :info-repository repo))

(defmethod make-fulltext-search-node ((doc info-document) uri)
  (make-instance 'fulltext-search-node
                 :name "Fulltext search"
                 :source doc
                 :search-term (aget (quri:uri-query-params uri) "q")))

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

(defun render-webinfo-page (acceptor node document)
  (with-output-to-string (s)
    (webinfo-html s
                  (lambda (stream)
                    (let ((request-media-type (request-media-type)))
                      (render-node node request-media-type stream
                                   :document document))))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor webinfo-acceptor) request)
  (or (dispatch-webinfo-request (info-repository acceptor) request acceptor)
      (call-next-method)))

(push
 (hunchentoot:create-folder-dispatcher-and-handler
  "/public/"
  (asdf:system-relative-pathname :webinfo "public/"))
 hunchentoot:*dispatch-table*)

(defun save-settings (request)
  (error "TODO"))

(defvar *accepted-media-types*
  '(("text/html" . :html)
    ("application/xml" . :xml)
    ("application/json" . :json)))

(defun request-media-type (&optional (request hunchentoot:*request*))
  (cond
    ((hunchentoot:get-parameter "_t")
     (string-case:string-case ((hunchentoot:get-parameter "_t"))
       ("html" :html)
       ("xml" :xml)
       ("json" :json)
       ("source" :source)
       (t (error "Invalid media type parameter value: ~a" (hunchentoot:get-parameter "_t")))))
    ((hunchentoot:header-in "accept" request)
     (if (string= (hunchentoot:header-in "accept" request) "*/*")
         :html
         ;; else
         (aand (mimeparse:best-match (mapcar 'car *accepted-media-types*)
                                     (hunchentoot:header-in "accept" request))
               (aget *accepted-media-types* it))))))

(defgeneric dispatch-webinfo-request (info-repository request acceptor))

(defmethod dispatch-webinfo-request ((info-repository file-info-repository) request acceptor)
  (let ((uri (quri:uri (hunchentoot:request-uri request)))
        (doc (info-document info-repository)))
    (trivia:match (quri:uri-path uri)
      ((or nil "" "/")
       (render-webinfo-page acceptor (home-node info-repository) doc))
      ((or "_is" "/_is")
       (render-webinfo-page acceptor (make-index-search-node info-repository uri) doc))
      ((or "_s" "/_s")
       (render-webinfo-page acceptor (make-search-node info-repository uri) doc))
      ((or "_fts" "/_fts")
       (render-webinfo-page acceptor (make-fulltext-search-node info-repository uri) doc))
      ((or "_settings" "/_settings")
       (trivia:match (hunchentoot:request-method request)
         (:get (render-webinfo-page acceptor (make-instance 'settings-info-node :name "Settings") doc))
         (:post (save-settings request)
                (render-webinfo-page acceptor
                                     (home-node info-repository) doc))))
      (_
       ;; TODO: perform a search if a node name is not matched?
       (let ((node-name (hunchentoot:url-decode (subseq (quri:uri-path uri) 1))))
         (alexandria:when-let ((node (find-node info-repository node-name)))
           (awhen (hunchentoot:get-parameter "_n")
             (return-from dispatch-webinfo-request
               (hunchentoot:redirect
                (format nil "/~a"
                        (trivia:match it
                          ("up" (node-up node))
                          ("next" (node-next node))
                          ("prev" (node-prev node))))
                )))
           (render-webinfo-page acceptor node (file info-repository))))))))

(defmethod dispatch-webinfo-request ((info-repository dir-info-repository) request acceptor)
  (let* ((uri (quri:uri (hunchentoot:request-uri request)))
         (path (split-sequence:split-sequence #\/ (quri:uri-path uri)
                                              :remove-empty-subseqs t)))
    (cond
      ((alexandria:emptyp path)
       (render-webinfo-page acceptor (home-node info-repository) nil))
      ((= (length path) 1)
       ;; Global repository url, or manual root node
       (trivia:match (first path)
         ((or nil "" "/")
          (render-webinfo-page acceptor (home-node info-repository) nil))
         ((or "_is" "/_is")
          (render-webinfo-page acceptor (make-index-search-node info-repository uri) nil))
         ((or "_s" "/_s")
          (render-webinfo-page acceptor (make-search-node info-repository uri) nil))
         ((or "_fts" "/_fts")
          (render-webinfo-page acceptor (make-fulltext-search-node info-repository uri) nil))
         ((or "_settings" "/_settings")
          (trivia:match (hunchentoot:request-method request)
            (:get
             (render-webinfo-page acceptor (make-instance 'settings-info-node :name "Settings") nil))
            (:post (save-settings request)
                   (render-webinfo-page acceptor (home-node info-repository) nil))))
         (_
          (alexandria:when-let ((doc (find-document info-repository (first path) :errorp nil)))
            (render-webinfo-page acceptor (find-node doc "Top") doc)))))

      ((= (length path) 2)
       ;; Node url in some doc
       (alexandria:when-let ((doc (find-document info-repository (first path) :errorp nil)))
         (trivia:match (second path)
           ("_is" (render-webinfo-page acceptor (make-index-search-node doc uri) doc))
           ("_s" (render-webinfo-page acceptor (make-search-node doc uri) doc))
           ("_fts"
            (render-webinfo-page
             acceptor
             (make-instance 'fulltext-search-node
                            :name "Fulltext search"
                            :source doc
                            :search-term (aget (quri:uri-query-params uri) "q")
                            :info-repository info-repository)
             doc))
           (_
            ;; TODO: perform a search if a node name is not matched?
            (let ((node-name (hunchentoot:url-decode (second path))))
              (if (not (alexandria:emptyp node-name))
                  (alexandria:when-let ((node (find-node doc node-name)))
                    (awhen (hunchentoot:get-parameter "_n") ;; navigation parameter
                      (return-from dispatch-webinfo-request
                        (hunchentoot:redirect
                         (format nil "/~a/~a" (document-name doc)
                                 (trivia:match it
                                   ("up" (node-up node))
                                   ("next" (node-next node))
                                   ("prev" (node-prev node)))))))
                    (render-webinfo-page acceptor node doc))
                  (render-webinfo-page acceptor (find-node doc "Top") doc))))
           ))))
    ;; TODO: perform a search if a node name is not matched?
    ))

;; TODO: should nodes have a reference to their document??
;; We use the dynamic variable *INFO-DOCUMENT* to know the document being rendered, for now ...
(defvar *info-document* "The document being rendered")

(defmethod render-node-html :around ((node info-node) theme stream &key document)
  (let ((*info-document* document))
    (call-next-method)))


(defvar *webinfo-acceptor*)

(defun start-webinfo (&rest args)
  (when (not (find :port args))
    ;; When port is zero, the acceptor is bound to a random free port
    (setf (getf args :port) 0))
  (setf *webinfo-acceptor*
        (hunchentoot:start (apply #'make-instance 'webinfo-acceptor args))))

(defun stop-webinfo ()
  (hunchentoot:stop *webinfo-acceptor*))
