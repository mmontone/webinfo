(in-package :webinfo)

(defclass sexp-info-node (info-node)
  ((contents :initarg :contents
             :accessor contents
             :initform nil)
   (node-up :initarg :node-up
            :accessor node-up
            :initform nil)
   (node-prev :initarg :node-prev
              :accessor node-prev
              :initform nil)
   (node-next :initarg :node-next
              :accessor node-next
              :initform nil)
   (title :initarg :title
          :accessor node-title)
   (children :initarg :children
             :accessor children
             :initform nil
             :documentation "Child nodes")))

(defmethod find-node ((doc sexp-info-node) name)
  (find name (nodes doc) :key 'node-name :test 'string=))

(defun render-sexp-content (content stream &key (split t))
  (who:with-html-output (stream)
    (block quit
      (labels ((render-element (x &optional (stream stream))
                 (flet ((render (&optional (stream stream))
                          (loop for child in (cddr x)
                                do (render-element child stream)))
                        (text (body)
                          (cond
                            ((stringp (car body))
                             (car body))
                            ((symbolp (car body))
                             (princ-to-string (car body)))
                            (t (caar body)))))
                   (cond
                     ((stringp x) (who:str x))
                     ((stringp (first x))
                      (who:str (first x)))
                     ((eql (first x) :|comment|)
                      ;; do nothing
                      )
                     (t ;; otherwise
                      (bind:bind (((tag args &rest body) x))
                        (case tag
                          (:|para| (who:htm (:p (render))))
                          (:|@*| (who:htm (:br))) ;; linebreaks
                          (:|menu| ;;(render-menu)
                           (who:htm (:ul :class "menu" (render)))
                           )
                          (:|menuentry| (who:htm (:li (render))))
                          (:|menunode| (let* ((node-name (text body))
                                              (node-url-name (substitute #\- #\space node-name)))
                                         (who:htm (:a :href (if split
                                                                node-url-name
                                                                (format nil "#~a" node-url-name))
                                                      (who:str node-name)))))
                          (:|menudescription| (render))

                          (:|node|
                           (error "This shouldn't happen")
                           #+nil(if split
                                    (return-from quit)
                                    (let ((subnode (make-xml-info-node x)))
                                      (render-subnode subnode nil stream)))
                           )
                          (:|nodename|)
                          (:|macro|) ;; TODO
                          (:|chapter| (render))
                          ((:|section| :|subsection| :|subsubsection|) (render))
                          (:|sectiontitle| (who:htm (:h1 (render))))
                          (:|anchor| (who:htm (:span :class "anchor" :id (getf args :|name|))))
                          (:|defvr| (who:htm (:div :class "defvr" (render))))
                          (:|definitionterm| (render))
                          (:|defcategory| (who:htm (:span :class "defcategory" (who:fmt "[~a]" (text body)))))
                          (:|indexterm|)
                          (:|defvariable| (who:htm (:span :class "defvariable" (who:str (text body)))))
                          (:|definitionitem| (render))
                          (:|deffn| (who:htm (:div :class "deffn" (render))))
                          (:|deffunction| (who:htm (:span :class "deffunction" (who:str (text body)))))
                          (:|defdelimiter| (who:str (who:escape-string (text body))))
                          (:|defparam| (who:htm (:span :class "defparam" (text body))))
                          (:|deftp| (who:htm (:div :class "deftp" (render))))
                          (:|defdatatype| (who:htm (:span :class "defdatatype" (who:str (text body)))))
                          (:|top| (render))
                          ((:|unnumbered| :|appendix|) (render))
                          ((:|findex| :|cindex| :|vindex| :|tindex|))
                          (:|printindex| (print-index (get-index (or *info-document*
                                                                     (info-repository *webinfo-acceptor*))
                                                                 (alexandria:make-keyword (string-upcase (getf args :|value|))))
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
                          ((:|itemprepend| :|prepend| :|beforefirstitem|))
                          (:|listitem| (who:htm (:li (render))))
                          (:|strong| (who:htm (:b (render))))
                          (:|emph| (who:htm (:emph (render))))
                          (:|quotation| (who:htm (:quote (render))))
                          (:|sc| (who:str (text body))) ;; smallcaps
                          (:|url| (let ((url (text body)))
                                    (who:htm (:a :href url (who:str url)))))
                          ((:|verbatim| :|example| :|lisp|)
                           (render))
                          (:|pre| (who:htm (:pre (:code :class "hljs"
                                                        (who:str
                                                         (who:escape-string (text body)))))))

                          (:|code| (who:htm (:code :class "inline" (render))))
                          (:|verb| (who:htm (:code :class "verb" (render))))
                          (:|var| (who:htm (:code :class "var" (render))))
                          (:|w| (who:str (who:escape-string (text body))))
                          (:|linebreak| (who:htm (:br)))
                          (:|verbatiminclude| ;; What to do? We ignore for now ...
                           )
                          (:|xref| (who:htm (:a :href (format nil "#~a" (getf args :|label|)) (render))))
                          (:|ref| (who:htm (:a :href (format nil "#~a" (getf args :|label|)) (render))))
                          ((:|xrefnodename| :|xrefinfoname|))
                          (:|xrefprinteddesc| (render))

                          (:|uref|
                           ;; we assume first element is |urefurl| and second |urefdesc|)
                           (let ((href
                                   (with-output-to-string (stream)
                                     (render-element (first body) stream))))
                             (who:htm (:a :href href (render-element (second body))))))


                          ((:|urefurl| :|urefdesc|) (render))

                          (:|heading| (who:htm (:h2 (render))))
                          (:|majorheading| (who:htm (:h1 (render))))

                          (t (error "Malformed node content: ~s" x))
                          )))))))
        (render-element content)))))

(defmethod text-contents ((node sexp-info-node))
  (let ((text ""))
    (labels ((append-text (x)
               (cond
                 ((stringp x)
                  (setf text (concatenate 'string text x)))
                 ((symbolp x) ;; TODO: don't do this ideally. revise
                  (setf text (concatenate 'string text (princ-to-string x))))
                 (t
                  (bind:bind (((_ _ &body body) x))
                    (loop for child in body
                          do (append-text child)))))))
      (append-text (contents node))
      text)))

(defun text->sexp (text)
  "Transform text to Texinfo paragraphs and linebreaks, in s-expression format"
  (flet ((make-paragraph (lines)
           (let ((last-line (car (last lines))))
             `(:|para| ()
                ,@(loop for line in lines
                        collect line
                        unless (eql line last-line)
                          collect `(:|@*| ()))))))
    (mapcar #'make-paragraph (split-into-paragraphs text))))
