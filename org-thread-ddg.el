;;; org-thread-ddg.el -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org-thread)

(defconst org-thread--ddg/slug "ddg")
(defconst org-thread--ddg/name "DuckDuckGo")
(defconst org-thread--ddg/base-url "https://html.duckduckgo.com/")
(defconst org-thread--ddg/search-url (concat org-thread--ddg/base-url "html?q="))
(defconst org-thread--ddg/first-page 0)

(defun org-thread--ddg/parse-results (dom)
  (mapcar
   'org-thread--ddg/parse-result
   (dom-by-class dom "links_main links_deep result__body")))

(defun org-thread--ddg/insert-result (result)
  (let-alist result
    (org-insert-subheading '(4))
    (org-thread--insert-as-org .title)
    (org-set-property "SITE" org-thread--ddg/slug)
    (org-end-of-subtree)
    (insert "\n" .href "\n\n")
    (org-thread--insert-children-as-org .snippet))
  (outline-up-heading 1))

(defun org-thread--ddg/insert-results (results)
  (mapcar 'org-thread--ddg/insert-result results))

(defun org-thread--ddg/parse-result (dom)
  (let* ((title-a (nth 0 (dom-by-class dom "result__a")))
         (snippet-a (nth 0 (dom-by-class dom "result__snippet")))
         ;; (url-a (nth 0 (dom-by-class dom "result__url")))
         (href (alist-get 'href (cl-second title-a))))
    ;; (setf (alist-get 'href (cl-second title-a)) href)
    `((title . ,title-a)
      (href . ,href)
      (snippet . ,snippet-a))))

(defun org-thread--ddg/search (query)
  (let* ((tree (org-thread--get-html (concat org-thread--ddg/search-url query)))
         (results (org-thread--ddg/parse-results tree)))
    (org-thread--ddg/insert-results results)))

(org-thread--register-search-engine org-thread--ddg/slug 'org-thread--ddg/search)

(defvar org-thread--ddg
  (org-thread--site-create
   :slug org-thread--ddg/slug
   :search 'org-thread--ddg/search
   :load-comments nil))

(org-thread--register-site org-thread--ddg)

(provide 'org-thread-ddg)
