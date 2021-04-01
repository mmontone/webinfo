;; An info file format that preserves markup (unlike .info files).
;; Implemented using s-expressions.
;; All nodes are flattened and serialized, then an index table at the end for fast and memory efficient access.
;; Expressions have the form (tag (&rest args) body)
;; Example:
;; (:setfilename () "djula.sinfo")
;; (:node (:name "Top")
;;        (:section ()
;;                  (:sectiontitle () "Top")
;;                  (:menu ()
;;                         (:menuitem (:name "Introduction")
;;                                    "Introduction to Djula"))))
;; (:node (:name "Introuction")
;;        (:section ()
;;                  (:sectiontitle () "Introduction")))
;; (:tag-table ()
;;             (:node (:name "Top" :at 187))
;;             (:ref (:name "#introduction" :at 554)))

(in-package :webinfo)

(defclass sinfo-info-document (info-document)
  ((file :initarg :file :accessor file))
  (:documentation "An info document that works with a serialized form in a file"))
