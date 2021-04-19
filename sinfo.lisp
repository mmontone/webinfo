;; An info file format that preserves markup (unlike .info files).
;; Implemented using s-expressions.
;; All nodes are flattened and serialized, then an index table at the end for fast and memory efficient access.
;; Expressions have the form (tag (&rest args) body)
;; Example:
;; (:setfilename () "djula.sinfo")
;; (:node (:name "Top")
;;        (:section ()
;;                  (:sectiontitle () "Top")
;;                  (:menu ()
;;                         (:menuitem (:name "Introduction")
;;                                    "Introduction to Djula"))))
;; (:node (:name "Introuction")
;;        (:section ()
;;                  (:sectiontitle () "Introduction")))
;; (:tag-table ()
;;             (:node (:name "Top" :at 187))
;;             (:ref (:name "#introduction" :at 554)))

(in-package :webinfo)

(defclass sinfo-info-document (info-document)
  ((filepath :initarg :filepath :accessor filepath
             :initform (error "Provide the filepath"))
   (tag-table :initarg :tag-table
              :accessor tag-table)
   (file :accessor file :documentation "A handle to the document file")
   (toc :documentation "Toc is memoized because it is expensive to calculate"))
  (:documentation "An info document that works with a serialized form in a file"))

(defmethod initialize-instance :after ((doc sinfo-info-document) &rest initargs)
  (declare (ignore initargs))
  (setf (tag-table doc)
        (read-sinfo-tag-table-from-file (filepath doc)))
  ;; Initialize indexes
  (let ((indexes (aget (tag-table doc) :index)))
    (loop for (index-type . entries) in indexes
          do
             (setf (aget indexes index-type)
                   (mapcar (bind:lambda-bind ((term . node-name))
                             (cons term (find-node doc node-name)))
                           entries)))
    (setf (indexes doc) indexes)))

(defmethod toc ((doc sinfo-info-document))
  (when (not (slot-boundp doc 'toc))
    (setf (slot-value doc 'toc) (call-next-method)))
  (slot-value doc 'toc))

;;-----------------------
;;-- xml reader
;;-----------------------

(defclass sinfo-sax-handler (sax:default-handler)
  (elements nodes counter suspended-elements reading-node-p))

(defmethod sax:start-document ((handler sinfo-sax-handler))
  (setf (slot-value handler 'elements) '(()))
  (setf (slot-value handler 'nodes) '())
  (setf (slot-value handler 'counter) 0)
  (setf (slot-value handler 'reading-node-p) nil)
  )

(defmethod sax:end-document ((handler sinfo-sax-handler))
  (with-slots (elements nodes) handler
    (values (reverse (first (slot-value handler 'elements)))
            nodes))
  handler)

(defmethod sax:start-dtd ((handler sinfo-sax-handler) name public-id system-id))

(defmethod sax:comment ((handler sinfo-sax-handler) data))

(defun namespace-p (attribute)
  (string= (sax:attribute-namespace-uri attribute)
           "http://www.w3.org/2000/xmlns/"))

(defun intern-attribute (attribute)
  (intern
   (if (sax:attribute-namespace-uri attribute)
       (concatenate 'string
                    (sax:attribute-namespace-uri attribute)
                    ":"
                    (sax:attribute-local-name attribute))
       (sax:attribute-local-name attribute))
   :keyword))


(defmethod sax:start-element ((handler sinfo-sax-handler)
                              namespace-uri
                              local-name
                              qname
                              attributes)
  (with-slots (elements nodes suspended-elements counter reading-node-p)
      handler
    (let ((name
            (intern
             (if namespace-uri
                 (concatenate 'string namespace-uri ":" local-name)
                 local-name)
             :keyword))
          (attributes (loop for attr in (remove-if #'namespace-p attributes)
                            collect (intern-attribute attr)
                            collect (sax:attribute-value attr))))
      (when (eql name :|node|)
        (if (not reading-node-p)
            (progn
              (setf suspended-elements (copy-tree elements))
              (setf elements '(())))
            (progn
              (push (list (copy-tree elements)) nodes)
              (setf elements '(()))))
        (setf reading-node-p t))
      (when reading-node-p
        (incf counter))
      (push `(,attributes ,name) elements))))

(defmethod sax:end-element ((handler sinfo-sax-handler)
                            namespace-uri
                            local-name
                            qname)
  (with-slots (elements nodes counter suspended-elements reading-node-p)
      handler
    (when reading-node-p
      (decf counter)
      (when (< counter 0)
        (setf elements suspended-elements)
        (setf reading-node-p nil)))

    (let ((element (pop elements)))
      (push (reverse element) (first elements)))))

(defmethod sax:characters ((handler sinfo-sax-handler) data)
  (push data (first (slot-value handler 'elements))))

(defun write-info-document-to-file (doc filepath)
  (let ((tag-table
          (list (cons :nodes nil)
                (cons :index (list (cons :cp nil)
                                   (cons :fn nil)
                                   (cons :vr nil)
                                   (cons :tp nil)))))
        (tag-table-pos))
    (with-open-file (file filepath
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8)
                          :external-format :utf-8)

      (setq file (flex:make-flexi-stream file :external-format :utf-8))

      ;; Nodes
      (loop for node in (all-nodes doc)
            do
               ;; Register node position
               (push (cons (node-name node) (file-position file))
                     (aget tag-table :nodes))

               (let ((node-header (xml-content->lisp
                                   (node-xml node) :include-nodes t))
                     (node-contents (xml-content->lisp (content-xml node))))

                 ;; Extract node title from sectiontitle element in contents
                 (bind:bind (((_ _ &body contents-body) node-contents)
                             ((_ _ section-title) (first contents-body)))
                   (setf (getf (second node-header) :|nodetitle| ) section-title))
                 
                 ;; Serialize node header
                 (prin1 node-header file)
                 (terpri file)

                 ;; Serialize node content
                 (prin1 node-contents file)
                 (terpri file)))

      ;; Index
      (flet ((serialize-indexes (indexes)
               (mapcar (bind:lambda-bind ((term . node))
                         (cons term (node-name node)))
                       indexes)))
        (setf (aget (aget tag-table :index) :fn)
              (serialize-indexes (collect-indexes doc :findex)))
        (setf (aget (aget tag-table :index) :vr)
              (serialize-indexes (collect-indexes doc :vindex)))
        (setf (aget (aget tag-table :index) :tp)
              (serialize-indexes (collect-indexes doc :tindex)))
        (setf (aget (aget tag-table :index) :cp)
              (serialize-indexes (collect-indexes doc :cindex))))
      (setf tag-table-pos (file-position file))
      (prin1 tag-table file)
      (terpri file)

      ;; Pointer to tag-table
      (write-sequence (cl-intbytes:int->octets tag-table-pos 3) file)

      ;; File format version
      (write-sequence (cl-intbytes:int->octets 1 1) file)
      ))
  (probe-file filepath))

(defmethod all-nodes ((doc sinfo-info-document))
  (nreverse
   (loop for node-name in (mapcar 'car (aget (tag-table doc) :nodes))
        collect (find-node doc node-name))))

(defun read-sinfo-tag-table-from-file (filepath)
  (with-open-file (file filepath
                        :direction :input
                        :element-type '(unsigned-byte 8)
                        :external-format :utf-8)
    (let ((file-length (file-length file))
          tag-table-pos file-format-version)
      (setq file (flex:make-flexi-stream file :external-format :utf-8))
      (file-position file (1- file-length))
      ;; File format version
      (let ((octets (make-array 1 :element-type '(unsigned-byte 8))))
        (read-sequence octets file)
        (setf file-format-version (cl-intbytes:octets->uint octets 1)))
      ;; Tag table position
      (file-position file (- file-length 4))
      (let ((octets (make-array 3 :element-type '(unsigned-byte 8))))
        (read-sequence octets file)
        (setf tag-table-pos (cl-intbytes:octets->uint octets 3)))

      (file-position file tag-table-pos)
      (read file)
      )))


(defclass file-info-node (sexp-info-node)
  ((filepath :initarg :filepath :accessor filepath)
   (file-pos :initarg :file-pos :accessor file-pos)
   (info-document :initarg :info-document
                  :accessor info-document
                  :initform (error "Provide info document"))))

(defmethod contents ((node file-info-node))
  (when (or (not (slot-boundp node 'contents))
            (null (slot-value node 'contents)))
    (with-open-file (file (filepath node)
                          :direction :input
                          :element-type '(unsigned-byte 8)
                          :external-format :utf-8)
      (setq file (flex:make-flexi-stream file :external-format :utf-8))
      (file-position file (file-pos node))
      (read file) ;; this read node header
      (setf (slot-value node 'contents) (read file)) ;; read node contents
      ))
  (slot-value node 'contents))

(defmethod node-source ((node file-info-node))
  (prin1-to-string (contents node)))

(defmethod find-node ((doc sinfo-info-document) node-name)
  (let ((entry (find node-name (aget (tag-table doc) :nodes)
                     :key 'car
                     :test 'string=)))
    (when (null entry)
      (return-from find-node nil))
    (with-open-file (file (filepath doc)
                          :direction :input
                          :element-type '(unsigned-byte 8)
                          :external-format :utf-8)
      (setq file (flex:make-flexi-stream file :external-format :utf-8))
      (file-position file (cdr entry))

      (bind:bind
          (((_ attrs &rest body) (read file)) ;; node info
           #+nil(node-contents (read file)))
        (flet ((get-node-info (what)
                 (caddr (find what body :key 'car))))
          #+nil(make-instance 'sexp-info-node
                              :name (get-node-info :|nodename|)
                              :node-up (get-node-info :|nodeup|)
                              :node-prev (get-node-info :|nodeprev|)
                              :node-next (get-node-info :|nodenext|)
                              :contents node-contents)
          (make-instance 'file-info-node
                         :name (getf attrs :|name|)
                         :title (getf attrs :|nodetitle|)
                         :node-up (get-node-info :|nodeup|)
                         :node-prev (get-node-info :|nodeprev|)
                         :node-next (get-node-info :|nodenext|)
                         :filepath (filepath doc)
                         :file-pos (cdr entry)
                         :info-document doc)
          )))))

(defmethod children ((node file-info-node))
  (remove-if-not (lambda (child)
                   (string= (node-up child) (node-name node)))
                 (all-nodes (info-document node))))

(defun start-sinfo-demo (&rest args &key (port 9090))
  (let ((djula-manual (make-instance 'sinfo-info-document :filepath #p"/home/marian/src/webinfo/test/djula.winfo" :name "Djula" :title "Djula")))

    (fulltext-index-document djula-manual)

    (apply #'webinfo:start-webinfo
     :info-repository
     (make-instance 'file-info-repository
                    :file djula-manual)
     :app-settings (list (cons :theme (make-instance 'nav-theme)))
     args)))
