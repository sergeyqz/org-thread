;;; org-thread-hn.el -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Search HN using https://hn.algolia.com/api/.
;; TODO Load top, new, and comment theads from https://news.ycombinator.com/, then parse HTML.
;; Algolia API returns comments in some arbitrary order that does not follow the HN order.
;; TODO Submit stories, comments right from org-mode?
;; TODO Handle changing titles.
;; TODO Search the same title/url.

;;; Code:

(require 'org-thread)

;;;###autoload
(defconst org-thread--hn/slug "hn")
(defconst org-thread--hn/name "Hacker News")
(defconst org-thread--hn/base-url "https://news.ycombinator.com")
(defconst org-thread--hn/api-url "https://hn.algolia.com/api/v1")
(defconst org-thread--hn/search-url (concat org-thread--hn/api-url "/search"))
(defconst org-thread--hn/item-url-json (concat org-thread--hn/api-url "/items"))
(defconst org-thread--hn/user-url-json (concat org-thread--hn/api-url "/users"))
(defconst org-thread--hn/first-page 0)

(cl-defun org-thread--hn/get-homepage (&optional (page org-thread--hn/first-page))
  (org-thread--get-json org-thread--hn/search-url `((tags "front_page") (page ,page))))

(defun org-thread--hn/render-items (items)
  (save-excursion
    (mapc 'org-thread--hn/insert-item items))
  ;; TODO Find a more semantic way to hide properties.
  (org-cycle)
  (org-cycle))

(defun org-thread--hn/make-item-url-json (id)
  (concat org-thread--hn/item-url-json "/" id))

(defun org-thread--hn/make-item-url (id)
  (format "%s/item?id=%s" org-thread--hn/base-url id))

(defun org-thread--hn/make-user-url-json (username)
  (concat org-thread--hn/user-url-json "/" username))

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

(defun org-thread--hn/get-item (id)
  (org-thread--get-json
   (org-thread--hn/make-item-url-json
    (if (numberp id) (number-to-string id) id))))

(defun org-thread--hn/latest-timestamp (comments)
  (cl-loop for c across comments
           maximize (let-alist c
                      (max .created_at_i
                           (or (org-thread--hn/latest-timestamp .children) 0)))))

(defun org-thread--hn/count-comments (comments)
  (cl-loop for c across comments
           summing (if (not (null (alist-get 'text c)))
                       (1+ (org-thread--hn/count-comments (alist-get 'children c)))
                     0)))

(defun org-thread--hn/org-todo-ancestors (from to)
  "Update all ancestors' keywords while their keywords are equal to <from>."
  (save-excursion
    (while (string= (org-get-todo-state) from)
      (org-todo to)
      (outline-up-heading 1))))

(cl-defun org-thread--hn/make-snippet (text &key (limit 100))
  "Get words from the first `limit' symbols of the first line of the `text'."
  (with-temp-buffer
    (insert (substring text 0 (string-match "\n" text)))
    (while (> (point) limit)
      (condition-case nil
          (backward-sexp) ;; To skip links: `backward-word' moves into the middle of links.
        (scan-error (backward-word))))
    ;; Don't call `kill-line' at the eob, as it signals an error in that case.
    (unless (eobp) (kill-line))
    (buffer-string)))

(cl-defun org-thread--hn/insert-comments (comments &key latest-timestamp update-ancestors)
  "Insert comments and optionally mark DONE ancestors as UPDATED.

   Don't render new comments under deleted ancestors."
  (org-show-subtree)
  (cl-loop for c across comments
           with new-comments-count = 0
           if (alist-get 'text c)
           do (let-alist c
                ;; TODO Algolia API doesn't provide updated_at field, so comment updates are not rendered.
                (when (or (null latest-timestamp) (< latest-timestamp .created_at_i))
                  (let ((p (org-find-property "ID" (number-to-string .parent_id)))
                        (text (with-temp-buffer
                                (org-thread--insert-html-as-org .text)
                                (buffer-string))))
                    ;; User can delete any comment and we don't want to reintroduce it.
                    ;; TODO Customize it?
                    (when p
                      (incf new-comments-count)
                      (goto-char p)
                      (when update-ancestors
                        (org-thread--hn/org-todo-ancestors "DONE" "UPDATED"))
                      (org-insert-subheading '(4))
                      (insert "TODO " (org-thread--hn/make-item-link .id "#") " "
                              (org-thread--hn/make-user-link .author) ": "
                              (org-thread--hn/make-snippet text))
                      (insert "\n" text)
                      (org-set-property "ID" (number-to-string .id))
                      (org-set-property "TS" (number-to-string .created_at_i))
                      (org-set-property "AUTHOR" .author)
                      ;; TODO Optimize? Just store calculated counts in the nodes.
                      (let ((n (org-thread--hn/count-comments .children)))
                        (when (> n 0) (org-set-property "COMMENTS" (number-to-string n))))
                      (outline-up-heading 1))))
                (incf new-comments-count
                      (org-thread--hn/insert-comments .children
                                                      :latest-timestamp latest-timestamp)))
           finally return new-comments-count))

(defun org-thread--hn/load-comments ()
  (interactive)
  (org-save-outline-visibility 'use-markers
    (while (null (org-entry-get (point) "POINTS"))
      (outline-up-heading 1))
    (let-alist (org-thread--hn/get-item (org-entry-get (point) "ID"))
      (let* ((previous-latest-timestamp (let ((ts (org-entry-get (point) "LATEST")))
                                          (when ts (string-to-number ts))))
             (latest-timestamp (org-thread--hn/latest-timestamp .children))
             (comments-count (org-thread--hn/count-comments .children))
             (new-comments-count 0))
        (when latest-timestamp
          (org-set-property "LATEST" (number-to-string latest-timestamp)))
        (org-set-property "COMMENTS" (number-to-string comments-count))
        (org-set-property "POINTS" (number-to-string .points))
        (save-excursion
          (setf new-comments-count
                (org-thread--hn/insert-comments .children
                                                :latest-timestamp previous-latest-timestamp)))
        (message "Inserted %d new comments" new-comments-count)))))

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
