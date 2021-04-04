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
  ((file :initarg :file :accessor file))
  (:documentation "An info document that works with a serialized form in a file"))

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

(make-array 10 :element-type 'octet)
(babel:string-to-octets (prin1-to-string '(:key val)))
(babel:octets-to-string (babel:string-to-octets (prin1-to-string '(:key val))))

(defun bar (pathspec)
  "With a flexi stream."
  (with-open-file (out pathspec
                       :direction :output
                       :if-exists :supersede
                       :external-format '(:latin-1 :eol-style :lf))
    (setq out (flex:make-flexi-stream out :external-format :utf-8))
    (write-line "ÄÖÜ1" out)
    (setf (flex:flexi-stream-external-format out) '(:latin-1 :eol-style :lf))
    (write-line "ÄÖÜ2" out) 
    (write-byte #xeb out)
    (write-sequence #(#xa3 #xa4 #xa5) out)
    (setf (flex:flexi-stream-external-format out) :ucs-2be)
    (write-line "ÄÖÜ3" out)))


(cl-intbytes:int->octets 655 2)
(cl-intbytes:octets->uint (cl-intbytes:int->octets 65000 2) 2)

(defparameter +djula-manual+ (make-instance 'webinfo:xml-info-document
                                            :name "Djula"
                                            :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                            :title "Djula manual"))

(xml-content->lisp (content-xml (find-node +djula-manual+ "Top")))
(babel:string-to-octets (prin1-to-string (xml-content->lisp (content-xml (find-node +djula-manual+ "Top")))))

(defun lisp-contents (xml-node)
  ())

(defun write-info-document-to-file (doc filepath)
  (let ((tag-table
          (list (cons :nodes nil)
                (cons :indexes (list (cons :cp nil)
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
               (push (cons (node-name node) (file-position file))
                     (aget tag-table :nodes))
               (prin1 (xml-content->lisp
                       (node-xml node) :include-nodes t)
                      file)
               (terpri file)
               (prin1 (xml-content->lisp
                       (content-xml node))
                      file)
               (terpri file))

      ;; Index
      (setf (aget (aget tag-table :index) :fn)
            (collect-indexes doc :findex))
      (setf (aget (aget tag-table :index) :vr)
            (collect-indexes doc :vindex))
      (setf (aget (aget tag-table :index) :tp)
            (collect-indexes doc :tindex))
      (setf (aget (aget tag-table :index) :cp)
            (collect-indexes doc :cindex))
      (setf tag-table-pos (file-position file))
      (prin1 tag-table file)
    
      ;; Pointer to tag-table
      (write-sequence (cl-intbytes:int->octets 655 3) file)

      ;; File format version
      (write-sequence (cl-intbytes:int->octets 1 1) file)

      )))
