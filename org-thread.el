;;; org-thread.el --- Search and read web in org-mode -*- coding: utf-8; lexical-binding: t; -*-

;; Version: 0.0.5
;; Author: sergeyqz <sergeyqz@yandex.com>
;; Keywords: outlines, org-mode, web
;; URL: https://github.com/sergeyqz/org-thread
;; Package-Requires: ((emacs "24.1") (unicode-escape "1.1"))

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'org-thread-json)
(require 'org-thread-network)

(defconst org-thread-version "0.0.5" "Version of the `org-thread' package.")

(defgroup org-thread nil
  "Org-thread customization group"
  :group 'applications)
(defcustom org-thread-default-buffer "*org-thread*"
  "Default buffer."
  :type 'string)
(defcustom org-thread-default-search-engine "ddg"
  "Default search engine"
  :type 'string)

(cl-defstruct (org-thread--site (:constructor org-thread--site-create)
                                (:copier nil))
  slug search load-comments)

(defvar org-thread--sites (make-hash-table :test 'equal))
(defun org-thread--register-site (site)
  (puthash (org-thread--site-slug site) site org-thread--sites))
(defun org-thread--get-site (slug)
  (gethash slug org-thread--sites))

(defvar org-thread--search-engines (make-hash-table :test 'equal))
(defun org-thread--register-search-engine (slug search-fn)
  (puthash slug search-fn org-thread--search-engines))
(defun org-thread--get-search-engine (slug)
  (gethash slug org-thread--search-engines))

(defun org-thread--insert-children-as-org (dom)
  (mapc 'org-thread--insert-as-org (dom-children dom)))

(defun org-thread--parse-dom (html)
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun org-thread--insert-html-as-org (html)
  (let* ((dom (org-thread--parse-dom html))
         (body (dom-by-tag dom 'body)))
    (org-thread--insert-children-as-org body)))

;; https://emacs.stackexchange.com/a/16793/15786
(defun org-thread--current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

;; TODO Is there a proper way to unescape a string?
(defun org-thread--unescape (text)
  (unicode-unescape (org-thread--unescape-newline text)))

(defun org-thread--unescape-newline (text)
  (replace-regexp-in-string "\\\\n" "\n" text))

(defun org-thread--insert-as-org (dom)
  (if (stringp dom) (insert (org-thread--unescape (replace-regexp-in-string "^*" "-" dom)))
    (cl-ecase (dom-tag dom)
      ((p) (progn
             (newline (cond ((org-thread--current-line-empty-p) 0)
                            ((org-at-heading-p) 1)
                            (t 2)))
             (org-thread--insert-children-as-org dom)))
      ((i) (progn
             (insert "/")
             (org-thread--insert-children-as-org dom)
             (insert "/")))
      ((b) (progn
             (insert "*")
             (org-thread--insert-children-as-org dom)
             (insert "*")))
      ;; ((a) (insert (org-make-link-string (dom-attr dom 'href) (dom-text dom))))
      ((a) (progn
             (insert "[[")
             (insert (dom-attr dom 'href))
             (insert "]")
             (insert "[")
             (org-thread--insert-children-as-org dom)
             (insert "]]")
             ))
      ((span) (progn
                (org-thread--insert-children-as-org dom)))
      ((pre) (progn
               (unless (org-thread--current-line-empty-p) (newline))
               (insert "#+BEGIN_ASCII\n" (dom-text (dom-by-tag dom 'code)))
               (unless (org-thread--current-line-empty-p) (newline))
               (insert "#+END_ASCII\n"))))))

(defun org-thread--insert-heading (title &optional properties)
  ""
  (org-insert-heading-respect-content)
  (insert title)
  (cl-loop for (p . v) in properties
        if (and p v)
        do (org-set-property (upcase p) (format "%s" v))))

(defun org-thread--insert-subheading (title &optional properties)
  ""
  (org-insert-subheading '(4))
  (insert title)
  (cl-loop for (p . v) in properties
        if (and p v)
        do (org-set-property (upcase p) (format "%s" v))))

(defun org-thread--parse-heading (heading)
  (let* ((sep " ")
         (sep-index (or (string-match sep heading) -1))
         (site-name (if (and (string-prefix-p "@" heading) (> sep-index 0))
                        (substring heading 1 sep-index)
                      org-thread-default-search-engine))
         (query (substring heading (1+ sep-index))))
    `((site-name . ,site-name)
      (query . ,query))))

(defun org-thread--load-comments ()
  (interactive)
  (let* ((slug (org-entry-get (point) "SITE"))
         (site (org-thread--get-site slug))
         (loader (org-thread--site-load-comments site)))
    (if (null loader)
        (message "%s doesn't support comments" slug)
      (funcall loader))))

(defun org-thread--search ()
  (interactive)
  (let* ((heading (string-trim (cl-fifth (org-heading-components))))
         (parsed (org-thread--parse-heading heading)))

    (let-alist parsed
      (funcall (org-thread--get-search-engine .site-name) .query))))

(defvar org-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") 'org-thread--search)
    (define-key map (kbd "C-c c") 'org-thread--load-comments)
    map)
  "Keymap for `org-thread-mode'")
(setf org-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s") 'org-thread--search)
    (define-key map (kbd "C-c c") 'org-thread--load-comments)
    map)
  )

;;;###autoload
(define-derived-mode org-thread-mode org-mode "Org-thread"
  "Search and read web in org-mode.")

;;;###autoload
(defun org-thread ()
  (interactive)
  (switch-to-buffer org-thread-default-buffer)
  (org-thread-mode))

;;;###autoload
(defun org-thread-search (query)
  (interactive
   (list (read-string "Query: ")))
  (switch-to-buffer org-thread-default-buffer)
  (org-thread-mode)
  (org-thread--insert-heading query)
  (org-thread--search))

(provide 'org-thread)

;;; org-thread.el ends here
