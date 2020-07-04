;;; org-thread-json.el -*- lexical-binding: t; coding: utf-8 -*-

(defun org-thread--get-json (url &optional params)
  (with-temp-buffer
    (if (null params)
        (url-insert-file-contents url)
      (url-insert-file-contents (concat url
                                        (unless (string-suffix-p "?" url) "?")
                                        (url-build-query-string params))))
    (goto-char (point-min))
    (json-read)))

(defun org-thread--read-json (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (json-read)))

(provide 'org-thread-json)
