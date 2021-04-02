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
  (elements))

(defmethod sax:start-document ((handler sinfo-sax-handler))
  (setf (slot-value handler 'elements) '(())))

(defmethod sax:end-document ((handler sinfo-sax-handler))
  (reverse (first (slot-value handler 'elements))))

(defmethod sax:start-dtd ((handler sinfo-sax-handler) name public-id system-id))

(defmethod sax:comment ((handler sinfo-sax-handler) data))

(defmethod sax:processing-instruction ((handler sinfo-sax-handler) target data)
  (with-slots (elements)
      handler
    (push `(,(intern target :keyword)
             ,data)
          (first elements))))

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

#+nil(defun make-namespace (attribute package)
  (let ((value (sax:attribute-value attribute))
        (local-name (sax:attribute-local-name attribute)))
    `(,(intern value package)
       ,value
       ,@(when local-name
           `(,(intern local-name package))))))

(defmethod sax:start-element ((handler sinfo-sax-handler)
                              namespace-uri
                              local-name
                              qname
                              attributes)
  (with-slots (elements)
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
      (push `(,@(when attributes `(,attributes ,name)))
            elements))))

(defmethod sax:end-element ((handler sinfo-sax-handler)
                            namespace-uri
                            local-name
                            qname)
  (with-slots (elements)
      handler
    (let ((element (pop elements)))
      (push (reverse element) (first elements)))))

(defmethod sax:characters ((handler sinfo-sax-handler) data)
  (push data (first (slot-value handler 'elements))))
