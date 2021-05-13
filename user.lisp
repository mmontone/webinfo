(defpackage :webinfo-user
  (:use :cl)
  (:export :make-webinfo))

(in-package :webinfo-user)

(defun read-xml-info-document (filepath &rest args)
  "Read an info-document in xml format from FILEPATH."
  (check-type filepath pathname)
  (apply #'make-instance 'webinfo::xml-info-document
	 :name (princ-to-string filepath)
	 :title (princ-to-string filepath)
	 :filepath filepath
	 args))

(defun make-webinfo (filepath output-path &rest args)
  "Compiles a Texinfo XML file to a Webinfo file.
OUTPUT-PATH can be either a filename path or a directory path.

See: texi2any --xml"
  (let ((output-filepath
	  (if (uiop/pathname:directory-pathname-p output-path)
	      (merge-pathnames (format nil "~a.winfo" (pathname-name filepath))  output-path)
	      output-path)))
    (let ((doc (apply #'make-instance 'webinfo::xml-info-document
		      :name (pathname-name output-filepath)
		      :title (pathname-name output-filepath)
                      :filepath filepath args)))
      (webinfo::write-info-document-to-file doc output-filepath))))
