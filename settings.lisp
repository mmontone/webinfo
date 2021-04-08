(in-package :webinfo)

(defclass settings-info-node (info-node)
  ())

(defmethod render-node ((node settings-info-node) theme stream &rest args)
  (who:with-html-output (stream)
    (:div :class "node"
          (:h1 (who:str "Settings")))))
