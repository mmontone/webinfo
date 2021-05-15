(defpackage webinfo/examples
  (:use :cl :webinfo)
  (:export
   :start-doc-demo
   :start-dir-demo))

(in-package :webinfo/examples)

(defun start-doc-demo (&rest args)
  (let ((djula-manual (make-instance 'webinfo:xml-info-document
                                     :name "djula"
                                     :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                     :title "Djula manual")))

    (apply #'webinfo:start-webinfo
     :info-repository
     (make-instance 'webinfo:file-info-repository
                    :file djula-manual
		    :fulltext-search t))
    :app-settings (list (cons :theme (make-instance 'nav-theme)))
    args))

(defun start-dir-demo (&rest args)
  (let* ((djula-manual (make-instance 'webinfo:xml-info-document
                                      :name "djula"
                                      :filepath (asdf:system-relative-pathname :webinfo "test/djula.xml")
                                      :title "Djula manual"))
         (djula-ref (make-instance 'webinfo:xml-info-document
                                   :name "djula-ref"
                                   :filepath (asdf:system-relative-pathname :webinfo "test/djula-ref.xml")
                                   :title "Djula reference"))
         (info-repository (make-instance 'dir-info-repository
                                         :dir (list djula-manual djula-ref)
                                         :search-index (make-memory-search-index))))

    (apply #'webinfo:start-webinfo
	   :info-repository info-repository
	   :app-settings (list (cons :theme (make-instance 'nav-theme)))
	   args)))

(defun start-winfo-demo (&rest args)
  (let ((djula-manual
	  (make-instance 'winfo-info-document
			 :filepath #p"/home/marian/src/webinfo/test/djula.winfo"
			 :name "Djula"
			 :title "Djula")))

    (apply #'webinfo:start-webinfo
           :info-repository
           (make-instance 'file-info-repository
                          :file djula-manual
			  :search-index (make-memory-search-index))
           :app-settings (list (cons :theme (make-instance 'nav-theme)))
           args)))
