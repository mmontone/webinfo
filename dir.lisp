(in-package :webinfo)

(defclass file-system-info-repository (dir-info-repository)
  ((working-directory :initarg :working-directory
                      :accessor working-directory
                      :type pathname
                      :documentation "The file system directory with the compiled  WebInfo documents."))
  (:documentation "Info repository implemented from a file system directory.

The WORKING-DIRECTORY contains the compiled WebInfo documents in files.

There's also a special 'dir' file with 'direntry' descriptions for the documents.

So, the processing of this type of repository consists of:
* Maintain a 'dir' file with top-level dir node information.
* Maintain a persistent disk index of all the documents.
* Maintain a persistent disk montezuma index for fulltext search of all the documents.
"))

(defclass source-file-system-info-repository (file-system-info-repository)
  ((source-directory :initarg :source-directory
                     :accessor source-directory
                     :type pathname
                     :documentation "The file system directory with the source files."))
  (:documentation "The source directory contains the Info documents, either in .texi source format, or xml. The documents are compiled to a document format that WebInfo uses.

It is similar to FILE-SYSTEM-DIRECTORY-REPOSITORY, but processes files inside the source directory and create documents that can be read by WebInfo from them (winfo files), and copies them to the repository directory. This can involve transforming to xml using texi2any and also applying Pandoc tool, depending of the type of file."))

(defgeneric initialize-info-repository (info-repository))

(defmethod initialize-info-repository ((info-repository source-file-system-info-repository))
  (convert-non-texi-files info-repository)
  (compile-texi-files info-repository)
  (copy-direntries-file info-repository)
  (create-index info-repository)
  (create-fulltext-info info-repository))

(defun read-direntries-file (direntries-file)
  (with-open-file (f direntries-file :direction :input
                                     :external-format :utf-8)
    (read f)))

(defmethod direntries ((info-repository file-system-info-repository))
  (read-direntries-file (merge-pathnames "dir" (working-directory info-repository))))

(defun get-temporary-file-pathname (&key directory (type "tmp" typep) prefix (suffix (when typep "-tmp")))
  "The temporary file's pathname will be based on concatenating
PREFIX (or \"tmp\" if it's NIL), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute."
  (let* ((prefix-pn (uiop/stream::ensure-absolute-pathname
                     (or prefix "tmp")
                     (or (uiop/stream::ensure-pathname
                          directory
                          :namestring :native
                          :ensure-directory t
                          :ensure-physical t)
                         #'uiop/stream:temporary-directory)))
         (prefix-nns (uiop/stream::native-namestring prefix-pn))
         (counter (random (expt 36 #-gcl 8 #+gcl 5))))
    (uiop/stream::parse-native-namestring
     (format nil "~A~36R~@[~A~]~@[.~A~]"
             prefix-nns counter suffix (unless (eq type :unspecific) type)))))

;;(get-temporary-file-pathname)

(defun compile-texinfo-file (file output-pathname)
  (let ((xmlfile (get-temporary-file-pathname)))
    (uiop/run-program:run-program (list "texi2any" "--no-validate" "--xml" (princ-to-string file)
                                        "-o" (princ-to-string xmlfile)))
    (webinfo-user:make-webinfo xmlfile output-pathname)))

(defun compile-texi-files (info-repository)
  (dolist (texi-file (uiop/filesystem:directory-files
                      (source-directory info-repository)
                      "*.texi*"))
    (let ((target-pathname (merge-pathnames (format nil "~a.winfo" (pathname-name texi-file))
                                            (working-directory info-repository))))
      (format t "Compiling Texinfo file: ~a to ~a~%" texi-file target-pathname)
      (compile-texinfo-file texi-file target-pathname))))

(defmethod dir ((info-repository file-system-info-repository))
  (mapcar (lambda (file)
	    (make-instance 'winfo-info-document
			   :filepath file
			   :name (pathname-name file)
			   :title (pathname-name file)))
	  (uiop/filesystem:directory-files (working-directory info-repository)
					   "*.winfo")))

(defparameter *lisp-manuals-repository*
  (make-instance 'source-file-system-info-repository
                 :source-directory (ensure-directories-exist #p"/home/marian/src/lisp/lisp-manuals/")
                 :working-directory (ensure-directories-exist #p"/home/marian/.local/share/webinfo/repositories/lisp-manuals/")))

(defun start-lisp-manuals-demo (&rest args)
  (webinfo:start-webinfo
   :port 9090
   :info-repository
   *lisp-manuals-repository*
   :app-settings (list (cons :theme (make-instance 'nav-theme)))))

;;(compile-texi-files *lisp-manuals-repository*)
;;(dir *lisp-manuals-repository*)
;;(mapcar 'indexes (dir *lisp-manuals-repository*))

(defun create-index-file (info-repository)
  (tokyo-cabinet:with-database
      (db (namestring (merge-pathnames "index.db" (working-directory info-repository)))
	  'tokyo-cabinet:tc-bdb
	  :write :create)
    (dolist (info-document (dir info-repository))
      (dolist (type-indexes (indexes info-document))
	(destructuring-bind (index-type . indexes) type-indexes
	  (dolist (index indexes)
	    (destructuring-bind (term . node) index
	      (let ((key (format nil "~a/~a" index-type term))
		    (value (prin1-to-string (list :document (document-name info-document)
						  :node (node-name node)
						  :type index-type))))
		(tokyo-cabinet:dbm-put db key value :mode :concat)))))))))

;; (create-index-file *lisp-manuals-repository*)


(defun read-index-values (input)
  (with-input-from-string (s input)
    (loop for val := (read s nil nil)
	  while val
	  collect val)))

(defun read-index (name index-type info-repository)
  (tokyo-cabinet:with-database
      (db (namestring (merge-pathnames "index.db" (working-directory info-repository)))
	  'tokyo-cabinet:tc-bdb
	  :read)
    (let ((value (tokyo-cabinet:dbm-get db (format nil "~a/~a" index-type name))))
      (when value
	(read-index-values value)))))

;;(read-index "djula:compile-template*" :fn *lisp-manuals-repository*)
