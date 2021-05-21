(in-package :webinfo)

(defvar *lenient-mode* nil
  "In lenient mode, Texinfo commands that are not recognized by the Webinfo renderer, are ignored.
This can be useful as long as Webinfo renderer is incomplete and doesn't understand all the Texinfo commands.")

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

			  (:|titlepage| (render))
			  (:|title| (who:htm (:h1 (render))))
			  (:|subtitle| (who:htm (:h2 (render))))
			  (:|author| (who:htm (:span :class "author" (render))))
                          (:|menu| ;;(render-menu)
                           (who:htm (:ul :class "menu" (render)))
                           )
                          (:|menuentry| (who:htm (:li (render))))
                          (:|menunode| (let* ((node-name (text body))
                                              (node-url-name (hunchentoot:url-encode node-name)))
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
			  (:|defun| (who:htm (:div :class "defun" (render))))
			  ((:|defmac| :|defmacx|) (who:htm (:div :class "defmac" (render))))
			  (:|deffunction| (who:htm (:span :id (text body) :class "deffunction" (who:str (text body)))))
                          (:|defdelimiter| (who:str (who:escape-string (text body))))
                          (:|defparam| (who:htm (:span :class "defparam" (who:str (text body)))))
                          ((:|deftp| :|deftpx|) (who:htm (:div :class "deftp" (render))))
                          (:|defdatatype| (who:htm (:span :id (text body):class "defdatatype" (who:str (text body)))))
                          (:|top| (render))
                          ((:|unnumbered| :|appendix|) (render))

			  ;; No rendering for indexing stuff.
			  ;; These elements are handled by index collecting.
                          ((:|findex| :|cindex| :|vindex| :|tindex|))
			  (:|subentry|)
			  
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
			  (:|asis| (render))
                          ((:|item| :|itemx|) (who:htm (:td (render))))
                          (:|itemformat| (who:htm (:td (render))))
                          (:|itemize| (who:htm
                                       (:ul (render) )))
			  (:|enumerate| (who:htm
                                       (:ol (render) )))
                          ((:|itemprepend| :|prepend| :|beforefirstitem|))
                          (:|listitem| (who:htm (:li (render))))
                          (:|strong| (who:htm (:b (render))))
                          (:|emph| (who:htm (:emph (render))))
                          (:|quotation| (who:htm (:quote (render))))
			  (:|indentedblock| (who:htm (:div :class "indentedblock" (render))))
			  (:|smallindentedblock| (who:htm (:div :class "smallindentedblock" (render))))
			  (:|kbd| (who:htm (:span :class "kbd" (render))))
			  (:|b| (who:htm (:b (render))))
                          (:|sc| (who:htm (:small (who:str (string-upcase (text body)))))) ;; smallcaps
                          (:|url| (let ((url (text body)))
                                    (who:htm (:a :href url (who:str url)))))
                          ((:|verbatim| :|example| :|lisp|)
                           (render))
			  ((:|smallverbatim| :|smallexample| :|smalllisp|)
                           (render)) ;; TODO
                          (:|pre| (who:htm (:pre (:code :class "hljs"
                                                        #+nil(who:str
                                                         (who:escape-string (text body)))
							(render)
							))))

                          (:|code| (who:htm (:code :class "inline" (render))))
                          (:|verb| (who:htm (:code :class "verb" (render))))
                          (:|var| (who:htm (:code :class "var" (render))))
                          (:|w| (who:str (who:escape-string (text body))))
                          (:|linebreak| (who:htm (:br)))
                          (:|verbatiminclude| ;; What to do? We ignore for now ...
                           )
                          (:|xref| (who:htm (:a :href (format nil "#~a" (getf args :|label|)) (who:str "See ") (render))))
                          (:|ref| (who:htm (:a :href (format nil "#~a" (getf args :|label|)) (render))))
			  (:|pxref| (who:htm (:a :href (format nil "#~a" (getf args :|label|)) (who:str "see ") (render))))
                          ((:|xrefnodename| :|xrefinfoname|))
                          (:|xrefprinteddesc| (render))

                          (:|uref|
                           ;; we assume first element is |urefurl| and second |urefdesc|)
                           (let ((href
                                   (with-output-to-string (stream)
                                     (render-element (first body) stream))))
                             (who:htm (:a :href href
					  (if (second body)
					      (render-element (second body))
					      (who:str href))))))


                          ((:|urefurl| :|urefdesc| :|urefreplacement|) (render))

			  (:|dfn| (who:htm (:i (render))))
			  (:|email|
			   ;; we assume first element is |emailaddress| and second |emailname|
			   (let ((href
                                   (format nil "emailto: ~a"
					   (with-output-to-string (stream)
					     (render-element (first body) stream)))))
			     (who:htm (:a :href href (render-element (second body))))))
			  ((:|emailaddress| :|emailname|) (render))

                          (:|heading| (who:htm (:h2 (render))))
			  (:|subheading| (who:htm (:h3 (render))))
                          (:|majorheading| (who:htm (:h1 (render))))

			  (:|insertcopying| ) ;; TODO. ignore
			  (:|menucomment| (render))
			  (:|detailmenu| (render))
			  (:|acronym| (who:htm (:span :class "acronym" (render))))
			  (:|acronymword| (render))

			  (:|accent| (render)) ;; TODO
			  (:|punct| (render))
			  (:|defparamtype| (render))
			  ((:|noeos| :|page|))
			  (:|footnote| );; TODO
			  (:|file| (who:htm (:code :class "file" (render))))
			  (:|command| (who:htm (:code :class "command" (render))))
			  (:|samp| (who:htm (:code :class "samp" (render))))
			  (:|noindent|)
			  (:|spacecmd|) ;;todo

			  (:|clicksequence| (render))
			  (:|click| (who:str "=>"))

			  ((:|float| :|listoffloats|)) ;; todo
			  (:|image|) ;; todo
			  			  		  
                          (t
			   (unless *lenient-mode*
			     (error "Malformed node content: ~s" x)))
                          )))))))
        (render-element content)))))

(defmethod text-contents ((node sexp-info-node))
  "Returns the NODE contents as plain text.
This is useful for fulltext indexing.

See FULLTEXT-INDEX-DOCUMENT.
"
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

(defun sexp2text (sexp)
  "Returns the XML dom node contents as plain text."
  (let ((text ""))
    (labels ((append-text (x)
               (if (stringp x)
                   (setf text (concatenate 'string text x))
                   (loop for child in (cddr x)
                         do (append-text child)))))
      (append-text sexp)
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

(defun split-into-paragraphs (text)
  "Divide TEXT into paragraphs, on each empty line."
  (let ((lines (split-sequence:split-sequence #\newline text))
        (paragraphs nil)
        (paragraph nil))

    (loop for line in lines
          do (if (alexandria:emptyp line)
                 (progn
                   (push (nreverse paragraph) paragraphs)
                   (setf paragraph nil))
                 ;; else
                 (push line paragraph))
          finally (when paragraph
                    (push (nreverse paragraph) paragraphs)))
    (nreverse paragraphs)))

(defmethod render-node-html ((node sexp-info-node) theme stream &key document)
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:div :class "node-content"
                (render-sexp-content (contents node) stream))
          (render-node-navigation node stream))))
