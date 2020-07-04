;;; org-thread-network.el -*- lexical-binding: t; coding: utf-8 -*-

(require 'url)

(defcustom org-thread--user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:77.0) Gecko/20100101 Firefox/77.0"
  "User-Agent"
  :type 'string
  :group 'org-thread)

(defun org-thread--get-html (url)
  (with-temp-buffer
    (let ((url-user-agent org-thread--user-agent))
      (url-insert-file-contents url))
    (libxml-parse-html-region (point-min) (point-max))))

(defun ombro--get-host (url)
  (url-host (url-generic-parse-url url)))

(provide 'org-thread-network)
