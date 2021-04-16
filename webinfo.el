(require 'browse-url)
(require 'eww)

(defgroup webinfo ()
  "WebInfo customizations"
  :prefix "webinfo-")

(defcustom webinfo-url "http://localhost:9090"
  "The url of the WebInfo service"
  :type 'string
  :group 'webinfo)

(defcustom webinfo-browser 'eww
  "WebInfo browsing method"
  :type 'symbol
  :group 'webinfo)

(defun webinfo-apropos-symbol (symbol)
  (interactive "sSymbol:")
  (webinfo-browse-url (format "%s/_s?q=%s" webinfo-url symbol)))

(defun webinfo-lookup-symbol (symbol)
  (interactive "sSymbol:")
  (webinfo-browse-url (format "%s/_s?q=%s" webinfo-url symbol)))

;; TODO: look at eww-browse-with-external-browser

(defun webinfo-browse-url (url)
  (case webinfo-browser
    ('eww (eww-browse-url (concat url "&_c=eww")))
    ('browse-url (browse-url (concat url "&_c=browse-url")))
    (t (eww-browse-url url))))

(defvar webinfo-keymap
  (let ((map (make-sparse-keymap "webinfo")))
    (define-key map "t" 'webinfo-navigate-top)
    (define-key map "u" 'webinfo-navigate-up)
    (define-key map "n" 'webinfo-navigate-next)
    (define-key map "p" 'webinfo-navigate-previous)
    (define-key map "h" 'webinfo-help)
    (define-key map "s" 'webinfo-search)
    (define-key map "l" 'webinfo-navigate-back)
    map))

(define-minor-mode webinfo-mode
  "WebInfo minor mode."
  :init-value nil
  :lighter " WebInfo"
  :keymap webinfo-keymap
  :group 'webinfo)
