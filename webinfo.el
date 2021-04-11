(require 'browse-url)

(defvar webinfo-url "http://localhost:9090")

(defun webinfo-apropos-symbol (symbol)
  (interactive "sSymbol:")
  (browse-url (format "%s/_s?q=%s" webinfo-url symbol)))
