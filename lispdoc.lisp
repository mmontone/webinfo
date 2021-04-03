;; An info document generated dynamically from Common Lisp packages exported definitions.

(in-package :webinfo)

(defclass lisp-info-document (info-document)
  ((nodes :initarg :nodes
          :accessor nodes
          :initform nil))
  (:documentation "An info document generated dynamically from Common Lisp packages exported definitions."))

(defmethod find-node ((doc lisp-info-document) name)
  (find name (nodes doc) :key 'node-name :test 'string=))

(defun collect-package-info (&optional (package *package*))
  (let (docs)
    (do-external-symbols (symbol package)
      (when (fboundp symbol)
        (push (load-function-info symbol) docs))
      (when (boundp symbol)
        (push (load-variable-info symbol) docs))
      (when (safe-class-for-symbol symbol)
        (push (load-class-info symbol) docs)))
    docs))

(defmethod render-node ((node sexp-info-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
          (render-node-navigation node stream)
          (:div :class "node-content"
                (render-sexp-content (contents node) stream :split nil))
          (render-node-navigation node stream))))

(defmethod toc ((node lisp-info-document))
  nil)

;; From docbrowser

(defun nice-princ-to-string (obj)
  (typecase obj
    (string obj)
    (keyword (prin1-to-string obj))
    (t (princ-to-string obj))))

#+sbcl(defmethod documentation ((slotd sb-pcl::condition-effective-slot-definition) (doc-type (eql 't)))
        "This method definition is missing in SBCL as of 1.0.55 at least. Adding it here
will make documentation for slots in conditions work properly."
        (slot-value slotd 'sb-pcl::%documentation))

(defun assoc-cdr (key data &key error-p)
  "Return (CDR (ASSOC KEY DATA)). If ERROR-P is non-NIL, signal an error if KEY is
not available is DATA."
  (let ((v (assoc key data)))
    (when (and error-p
               (not v))
      (error "~s not found in data" key))
    (cdr v)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
  (eq (nth-value 1 (find-symbol (symbol-name (cond ((symbolp symbol)
                                                    symbol)
                                                   ((eq (car symbol) 'setf)
                                                    (cadr symbol))
                                                   (t
                                                    (error "Unknown symbol type: ~s" symbol))))
                                package))
      :external))

(defun prin1-to-string-with-package (obj package)
  (let ((*package* package))
    (prin1-to-string obj)))

(defun format-argument-to-string (arg)
  (etypecase arg
    (symbol (nice-princ-to-string arg))
    (list   (mapcar #'(lambda (entry conversion) (funcall conversion entry))
                    arg (list #'(lambda (v)
                                  (if (listp v)
                                      (nice-princ-to-string (car v))
                                      (nice-princ-to-string v)))
                              #'prin1-to-string
                              #'nice-princ-to-string)))))

(defun load-function-info (symbol)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                              (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                              )
                      (princ-to-string (swank-backend:arglist symbol))))
        (cons :type (cond ((macro-function symbol) :macro)
                          ((typep (symbol-function symbol) 'generic-function) :generic-function)
                          (t :function)))))

(defun load-variable-info (symbol)
  (list (cons :name (string symbol))
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (prin1-to-string (symbol-value symbol))))
        (cons :constant-p (constantp symbol))
        (cons :type :variable)))

(defun find-superclasses (class)
  (labels ((f (classes found)
             (if (and classes
                      (not (eq (car classes) (find-class 'standard-object)))
                      (not (member (car classes) found)))
                 (f (cdr classes)
                    (f (closer-mop:class-direct-superclasses (car classes))
                       (cons (car classes) found)))
                 found)))
    (f (list class) nil)))

(defun safe-class-for-symbol (symbol)
  (handler-case
      (find-class symbol)
    (error nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun load-specialisation-info (class-name)
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
              for v in spec
              for symbol = (specialise->symbol v)
              when (and (not (member symbol ignored))
                        (symbol-external-p symbol (symbol-package (class-name class))))
                collect (list (cons :name symbol)))
            #'string< :key (alexandria:compose #'princ-to-string #'assoc-name)))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (symbol-external-p name)
      symbol)))

(defun load-accessor-info (class slot)
  (flet ((getmethod (readerp method-list)
           (dolist (method method-list)
             (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
               (when (and (eq (type-of method) (if readerp
                                                   'closer-mop:standard-reader-method
                                                   'closer-mop:standard-writer-method))
                          (eq (closer-mop:slot-definition-name (closer-mop:accessor-method-slot-definition method))
                              (closer-mop:slot-definition-name slot)))
                 (return-from getmethod name))))))

    ;; There are several different situations we want to detect:
    ;;   1) Only a reader method: "reader FOO"
    ;;   2) Only a writer method: "writer FOO"
    ;;   3) Only a writer SETF method: "writer (SETF FOO)"
    ;;   4) A reader and a SETF method: "accessor FOO"
    ;;   5) A reader and non-SETF writer: "reader FOO, writer FOO"
    ;;
    ;; The return value from this function is an alist of the following form:
    ;;
    ;;  ((:READER . FOO-READER) (:WRITER . FOO-WRITER) (:ACCESSOR . FOO-ACCESSOR))
    ;;
    ;; Note that if :ACCESSOR is given, then it's guaranteed that neither
    ;; :READER nor :WRITER will be included.
    ;;
    ;; We start by assigning the reader and writer methods to variables
    (let* ((method-list (closer-mop:specializer-direct-methods class))
           (reader (%ensure-external (getmethod t method-list)))
           (writer (%ensure-external (getmethod nil method-list))))
      ;; Now, detect the 5 different cases, but we coalease case 2 and 3.
      (cond ((and reader (null writer))
             `((:reader . ,reader)))
            ((and (null reader) writer)
             `((:writer . ,writer)))
            ((and reader (listp writer) (eq (car writer) 'setf) (eq (cadr writer) reader))
             `((:accessor . ,reader)))
            ((and reader writer)
             `((:reader . ,reader) (:writer . ,writer)))))))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (string (closer-mop:slot-definition-name slot)))
                 (cons :documentation (swank-mop:slot-definition-documentation slot))
                 ;; The LIST call below is because the accessor lookup is wrapped
                 ;; in a FOR statement in the template.
                 (cons :accessors (let ((accessor-list (load-accessor-info class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :slots         (load-slots cl))
          ;; (cons :methods       (load-specialisation-info cl)) TODO: fix
          
          (cons :type :class))))

(defun %annotate-function-info (fn-info classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
    with name = (cdr (assoc :name fn-info))
    for class-info in classes
    do (loop
         for slot-info in (cdr (assoc :slots class-info))
         do (loop
              for accessor in (cdr (assoc :accessors slot-info))
              for accessor-sym = (cdar accessor)
              when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                       (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                do (return-from %annotate-function-info (append fn-info '((:accessorp t))))))
    finally (return fn-info)))

;; Info nodes

(defun make-info-document-for-package (package)
  (let ((doc (make-instance 'lisp-info-document
                            :name (package-name package)
                            :title (package-name package)))
        (top-node (make-instance 'sexp-info-node
                                 :name "Top"))
        (dictionary-node (make-instance 'sexp-info-node
                                        :name "Dictionary"))
        (variable-index-node (make-instance 'sexp-info-node
                                            :name "Variable index"))
        (function-index-node (make-instance 'sexp-info-node
                                            :name "Function index"))
        (class-index-node (make-instance 'sexp-info-node
                                         :name "Class index"))
        )
    (let ((package-info (collect-package-info package)))
      ;; Build top node
      (setf (contents top-node)
            `(:|chapter| ()
               (:|sectiontitle| ()
                 ,(format nil "Package reference: ~a" (package-name package)))
               (:|menu| ()
                 (:|menuentry| ()
                   (:|menunode| ()
                     "Dictionary"))
                 ;; ,(loop for infoitem in info
                 ;;        collect
                 ;;        `(:|menuentry| ()
                 ;;           (:|menunode| ()
                 ;;             ,(aget infoitem :name))
                 ;;           (:|menudescription| ()
                 ;;             "")))

                 )))
      ;; Build dictionary node
      (setf (node-up dictionary-node) "Top")
      (setf (contents dictionary-node)
            `(:|chapter| ()
               (:|sectionname| ()
                 "Dictionary")
               ,(loop for info in package-info
                      collect (lispinfo->sexp info))))

      (push top-node (nodes doc))
      (push dictionary-node (nodes top-node))
      doc)))

(defun lispinfo->sexp (info)
  (ecase (aget info :type)
    (:variable
     `(:|defvr| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "vr")
            ,(aget info :name))
          (:|defcategory| () "Variable")
          (:|defvariable| () ,(aget info :name)))
        (:|definitionitem| ()
          (:|para| () ,(or (aget info :documentation) "")))
        (:|vindex| ()
          (:|indexterm| ()
            ,(aget info :name)))))
    (:function
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Function")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          (:|para| () ,(or (aget info :documentation) "")))
        (:|findex| ()
          (:|indexterm| ()
            ,(aget info :name)))))
    (:generic-function
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Generic function")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          (:|para| () ,(or (aget info :documentation) "")))
        (:|findex| ()
          (:|indexterm| ()
            ,(aget info :name)))))
    (:class
     `(:|deftp| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "tp")
            ,(aget info :name))
          (:|defcategory| () "Class")
          (:|defdatatype| () ,(aget info :name)))
        (:|definitionitem| ()
          (:|para| () ,(or (aget info :documentation) "")))
        (:|tindex| ()
          (:|indexterm| ()
            ,(aget info :name)))))
    (:macro
     `(:|deffn| ()
        (:|definitionterm| ()
          (:|indexterm| (:index "fn")
            ,(aget info :name))
          (:|defcategory| () "Macro")
          (:|deffunction| () ,(aget info :name))
          (:|defparam| () ,(aget info :args)))
        (:|definitionitem| ()
          (:|para| () ,(or (aget info :documentation) "")))
        (:|findex| ()
          (:|indexterm| ()
            ,(aget info :name)))))))

(defclass lispdoc-info-repository (dir-info-repository)
  ())

(defmethod initialize-instance :after ((repo lispdoc-info-repository) &rest initargs)
  (declare (ignore initargs))
  (setf (dir repo)
        (mapcar 'make-info-document-for-package
                (remove "COMMON-LISP" (list-all-packages)
                        :test 'equalp
                        :key 'package-name))))

(defun start-lispdoc-demo ()
  (webinfo:start-webinfo
     :port 9090
     :info-repository
     (make-instance 'lispdoc-info-repository)
     :app-settings (list (cons :theme (make-instance 'nav-theme)))))
