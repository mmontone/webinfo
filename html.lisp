(in-package :webinfo)

(defclass html-info-document (info-document)
  ()
  (:documentation "An info document that renders the content of an HTML file.
Also makes indexes from HTML headings"))
