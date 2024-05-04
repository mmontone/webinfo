(defpackage :webinfo/user
  (:use :cl)
  (:export
   :compile-texinfo-file
   :comipile-texinfo-xml-file
   :open-texinfo-file))

(in-package :webinfo/user)

(defun read-xml-info-document (filepath &rest args)
  "Read an info-document in xml format from FILEPATH."
  (check-type filepath pathname)
  (apply #'make-instance 'webinfo::xml-info-document
         :name (princ-to-string filepath)
         :title (princ-to-string filepath)
         :filepath filepath
         args))

(defun compile-texinfo-xml-file (filepath output-path &rest args)
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

(defun compile-texinfo-file (file output-file)
  "Compiles Texinfo FILE to a winfo file OUTPUT-FILE."
  (let ((xmlfile (webinfo/utils:get-temporary-file-pathname)))
    (uiop/run-program:run-program (list "texi2any" "--no-validate" "--xml" (princ-to-string file)
                                        "-o" (princ-to-string xmlfile)))
    (compile-texinfo-xml-file xmlfile output-file)))

(defun open-texinfo-file (filepath &rest args)
  
  (let* ((temporary-file (webinfo/utils:get-temporary-file-pathname))
	 (document (make-instance 'webinfo:winfo-info-document
				  :filepath (compile-texinfo-file filepath temporary-file)))
         (acceptor
           (apply #'webinfo:start-webinfo
	          :info-repository (make-instance 'webinfo:file-info-repository
					          :file document)
	          :app-settings (list (cons :theme (make-instance 'webinfo:nav-theme)))
	          args)))
    (trivial-open-browser:open-browser (format nil "http://localhost:~a"
                                               (hunchentoot:acceptor-port acceptor)))
    acceptor))
