(in-package :webinfo)

(defclass filesystem-info-repository (dir-info-repository)
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

(defclass source-filesystem-info-repository (filesystem-info-repository)
  ((source-directory :initarg :source-directory
                     :accessor source-directory
                     :type pathname
                     :documentation "The file system directory with the source files."))
  (:documentation "The source directory contains the Info documents, either in .texi source format, or xml. The documents are compiled to a document format that WebInfo uses.

It is similar to FILESYSTEM-DIRECTORY-REPOSITORY, but processes files inside the source directory and create documents that can be read by WebInfo from them (winfo files), and copies them to the repository directory. This can involve transforming to xml using texi2any and also applying Pandoc tool, depending of the type of file."))

(defgeneric initialize-info-repository (info-repository))

(defmethod initialize-info-repository ((info-repository source-filesystem-info-repository))
  (convert-non-texi-files info-repository)
  (compile-texi-files info-repository)
  (copy-direntries-file info-repository)
  (create-index info-repository)
  (create-fulltext-info info-repository))

(defun read-direntries-file (direntries-file)
  (with-open-file (f direntries-file :direction :input
                                     :external-format :utf-8)
    (read f)))

(defmethod direntries ((info-repository filesystem-info-repository))
  (read-direntries-file (merge-pathnames "dir" (working-directory info-repository))))

(defun compile-texi-files (info-repository)
  (dolist (texi-file (uiop/filesystem:directory-files
                      (source-directory info-repository)
                      "*.texi*"))
    (let ((target-pathname (merge-pathnames (format nil "~a.winfo" (pathname-name texi-file))
                                            (working-directory info-repository))))
      (format t "Compiling Texinfo file: ~a to ~a~%" texi-file target-pathname)
      (webinfo/user:compile-texinfo-file texi-file target-pathname))))

(defmethod dir ((info-repository filesystem-info-repository))
  (mapcar (lambda (file)
	    (make-instance 'winfo-info-document
			   :filepath file
			   :name (pathname-name file)
			   :title (pathname-name file)))
	  (uiop/filesystem:directory-files (working-directory info-repository)
					   "*.winfo")))

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
