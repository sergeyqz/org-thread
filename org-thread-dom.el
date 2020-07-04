;;; org-thread-dom.el -*- lexical-binding: t; coding: utf-8 -*-

(defun org-thread--dom-children-to-org (dom)
  (replace-regexp-in-string
   "^*" "  -"
   (string-join (mapcar 'dom-to-org (dom-children dom)))))

(defun org-thread--parse-dom (html)
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))
