;;; org-thread-hn.el -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO Handle changing titles

(require 'org-thread)

;;;###autoload
(defconst org-thread--hn/slug "hn")
(defconst org-thread--hn/name "Hacker News")
(defconst org-thread--hn/base-url "https://news.ycombinator.com")
(defconst org-thread--hn/api-url "https://hn.algolia.com/api/v1/")
(defconst org-thread--hn/search-url (concat org-thread--hn/api-url "search?"))
(defconst org-thread--hn/item-url-json (concat org-thread--hn/api-url "items/"))
(defconst org-thread--hn/user-url-json (concat org-thread--hn/api-url "users/"))
(defconst org-thread--hn/first-page 0)

(cl-defun org-thread--hn/get-homepage (&optional (page org-thread--hn/first-page))
  (org-thread--get-json (concat org-thread--hn/search-url
                           (url-build-query-string `((tags "front_page")
                                                     (page ,page))))))

(defun org-thread--hn/render-items (items)
  (save-excursion
    (mapc 'org-thread--hn/insert-item items))
  ;; TODO Find a more semantic way to hide properties.
  (org-cycle)
  (org-cycle))

(defun org-thread--hn/make-item-url-json (id)
  (concat org-thread--hn/item-url-json id))

(defun org-thread--hn/make-item-url (id)
  (format "%s/item?id=%s" org-thread--hn/base-url id))

(defun org-thread--hn/make-user-url-json (username)
  (concat org-thread--hn/user-url-json username))

(defun org-thread--hn/make-user-url (username)
  (format "%s/user?id=%s" org-thread--hn/base-url username))

(defun org-thread--hn/make-item-link (id title)
  (org-link-make-string (org-thread--hn/make-item-url id) title))

(defun org-thread--hn/make-user-link (username)
  (org-link-make-string (org-thread--hn/make-user-url username) username))

(defun org-thread--hn/get-item-url (item)
  (let-alist item
    (if (member .url '("" nil)) (org-thread--hn/make-item-url (or .objectID .id)) .url)))

(defun org-thread--hn/insert-item (item)
  (let-alist item
    (org-thread--insert-subheading
     .title
     `(("ID" . ,(or .objectID .id))
       ("URL" . ,(org-thread--hn/get-item-url item))
       ("POINTS" . ,.points)
       ("AUTHOR" . ,.author)
       ("COMMENTS" . ,(number-to-string (or .num_comments 0)))
       ("DATE" . ,.created_at)))
    (when .story_text
      (org-end-of-subtree)
      (newline)
      (org-thread--insert-html-as-org .story_text)))
  (outline-up-heading 1))

(defun org-thread--hn/get-query-params ()
  `((query ,(org-entry-get (point) "QUERY"))
    (page ,(org-entry-get (point) "PAGE"))
    (tags ,(org-entry-get (point) "HNTAGS"))
    (numericFilters ,(org-entry-get (point) "NUMERIC_FILTERS"))))

;; TODO By default search stories with no filters.
;;      If prefixed or smth, allow to set all params.
;;;###autoload
(cl-defun org-thread--hn/search (&optional (query "")
                                           (page org-thread--hn/first-page)
                                           (tags "story")
                                           ;; TODO Use s-exps for numeric-filters?
                                           (numeric-filters "num_comments>0"))
  (when (= page org-thread--hn/first-page)
    (org-set-property "QUERY" query)
    (org-set-property "PAGE" (number-to-string (1- page)))
    (org-set-property "HNTAGS" tags)
    (org-set-property "NUMERIC_FILTERS" numeric-filters))
  (org-thread--hn/load-next-page))

  ;; (let* ((params (org-thread--hn/get-query-params))
         ;; (response (org-thread--get-json org-thread--hn/search-url params))
         ;; (hits (alist-get 'hits response)))
    ;; (org-thread--hn/render-items hits)))

(defun org-thread--hn/load-next-page ()
  (while (null (org-entry-get (point) "QUERY"))
    (outline-up-heading 1))
  (let* ((params (org-thread--hn/get-query-params))
         (page (string-to-number (car (alist-get 'page params))))
         (next-page (number-to-string (1+ page)))
         ;; TODO Dirty?
         (p (save-excursion (org-end-of-subtree))))
    (setf (alist-get 'page params) (list next-page))
    (let* ((response (org-thread--get-json org-thread--hn/search-url params))
           (hits (alist-get 'hits response)))
      (org-set-property "PAGE" next-page)
      (org-thread--hn/render-items hits)
      (goto-char p)
      (unless (bolp) (forward-char)))))

;;;###autoload
(org-thread--register-search-engine org-thread--hn/slug 'org-thread--hn/search)

(provide 'org-thread-hn)
