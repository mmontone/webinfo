(in-package :webinfo)

(defclass file-system-directory-repository (dir-info-repository)
  ((directory :initarg :directory
              :accessor directory
              :type pathname
              :documentation "The file system directory with the compiled  WebInfo documents."))
  (:documentation "Info repository implemented from a file system directory.

The DIRECTORY contains the compiled WebInfo documents in files.

There's also a special 'dir' file with 'direntry' descriptions for the documents.

So, the processing of this type of repository consists of:
* Maintain a 'dir' file with top-level dir node information.
* Maintain a persistent disk index of all the documents.
* Maintain a persistent disk montezuma index for fulltext search of all the documents.
"))

(defclass source-file-system-directory-repository (file-system-repository-directory)
  ((source-directory :initarg :source-directory
                     :accessor source-directory
                     :type pathname
                     :documentation "The file system directory with the source files."))
  (:documentation "The source directory contains the Info documents, either in .texi source format, or xml. The documents are compiled to a document format that WebInfo uses.

It is similar to FILE-SYSTEM-DIRECTORY-REPOSITORY, but processes files inside the source directory and create documents that can be read by WebInfo from them (winfo files), and copies them to the repository directory. This can involve transforming to xml using texi2any and also applying Pandoc tool, depending of the type of file."))
