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

(defparameter *lisp-manuals-repository*
  (make-instance 'source-file-system-info-repository
		 :source-directory (ensure-directories-exist #p"~/src/lisp/lisp-manuals/")
		 :working-directory (ensure-directories-exist #p"~/.local/share/webinfo/repositories/lisp-manuals/")))
