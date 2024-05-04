(ql:quickload :webinfo)
(ql:quickload :adopt)

(defpackage :webinfo/cli
  (:use :cl))

(in-package :webinfo/cli)

(adopt:define-string *help-text*
  "Open TEXINFO-FILE with WebInfo reader.")

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

(defparameter *option-debug*
  (adopt:make-option 'debug
                     :long "debug"
                     :help "show errors"
                     :reduce (constantly t)))

(defparameter *option-lenient*
  (adopt:make-option 'lenient
                     :long "lenient"
                     :help "ignore unrecognized Texinfo commands"
                     :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "webinfo"
   :summary "WebInfo - A Texinfo documents reader for desktop and Web."
   :usage "[OPTIONS] TEXINFO-FILE"
   :contents (list *option-version*
                   *option-help*
                   *option-debug*
                   *option-lenient*)
   :help *help-text*))

(defun run (texinfo-file &rest args)
  (let (texinfo-pathname)

    (setf texinfo-pathname (pathname texinfo-file))
    (when (not (UIOP/PATHNAME:ABSOLUTE-PATHNAME-P texinfo-pathname))
      (setf texinfo-pathname (merge-pathnames texinfo-pathname (uiop/os:getcwd))))
    (webinfo/user:open-texinfo-file texinfo-pathname)))

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
        (when (gethash 'debug options)
          (setf hunchentoot:*show-lisp-backtraces-p* t)
          (setf hunchentoot:*show-lisp-errors-p* t))
        (when (gethash 'lenient options)
          (setf webinfo::*lenient-mode* t))
        (destructuring-bind (input-file) arguments
          (run input-file)
          (read)))
    (error (c)
      (adopt:print-error-and-exit c))))

(defun build ()
  (sb-ext:save-lisp-and-die "webinfo"
                            :save-runtime-options t
                            :executable t
                            :compression (member :sb-core-compression *features*)
                            :toplevel #'toplevel))

(build)
