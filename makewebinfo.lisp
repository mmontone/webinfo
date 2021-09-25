(require :webinfo)
(require :adopt)

(defpackage :makewebinfo
  (:use :cl))

(in-package :makewebinfo)

(adopt:define-string *help-text*
  "Translate Texinfo source documentation to Webinfo format.")

(defparameter *option-version*
  (adopt:make-option 'version
                     :long "version"
                     :help "display version information and exit"
                     :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
                     :long "help"
                     :short #\h
                     :help "display help information and exit"
                     :reduce (constantly t)))

(defparameter *option-output*
  (adopt:make-option 'output
                     :long "output"
                     :parameter "FILE"
                     :short #\o
                     :help "output file"
                     :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
   :name "makewebinfo"
   :summary "Translate Texinfo source documentation to Webinfo format."
   :usage "[OPTIONS] TEXINFO-FILE"
   :contents (list *option-version* *option-help* *option-output*)
   :help *help-text*))

(defun run (texinfo-file &key output)
  (let (texinfo-pathname output-pathname)

    (setf texinfo-pathname (pathname texinfo-file))
    (when (not (UIOP/PATHNAME:ABSOLUTE-PATHNAME-P texinfo-pathname))
      (setf texinfo-pathname (merge-pathnames texinfo-pathname (uiop/os:getcwd))))

    (when output
      (setf output-pathname (pathname output))
      (when (not (UIOP/PATHNAME:ABSOLUTE-PATHNAME-P output-pathname))
        (setf output-pathname (merge-pathnames output-pathname (uiop/os:getcwd)))))
    (when (not output-pathname)
      (setf output-pathname (merge-pathnames (format nil "~a.winfo"
                                                     (pathname-name texinfo-file))
                                             (uiop/os:getcwd))))
    
    (webinfo/user:compile-texinfo-file texinfo-pathname output-pathname)))      

(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (format t "~a~%"
		  (asdf:component-version (asdf:find-system "webinfo")))
          (adopt:exit))
        (when (zerop (length arguments))
          (adopt:print-help-and-exit *ui*))
        (unless (= 1 (length arguments))
          (format t "Invalid syntax.~%")
          (adopt:exit))
        (destructuring-bind (input-file) arguments
          (run input-file
	       :output (gethash 'output options))))
    (error (c)
      (adopt:print-error-and-exit c))))

(defun build ()
  (sb-ext:save-lisp-and-die "makewebinfo"
                            :save-runtime-options t
                            :executable t
			    :compression t
                            :toplevel #'toplevel))

(build)
