;; -*- lexical-binding: t; -*-
(require 'dom)
(require 'shr)

(defgroup html2org nil
  "Save http(s) page as org file")

(defcustom html2org-store-dir "/home/lujun9972/github/emacs-document/raw/"
  "The directory to store org files"
  :type 'directory)

(defcustom html2org-timeout 30
  "Timeout seconds"
  :type 'number)

(defun html2org-get-dom (&optional url)
  "Retrive `URL' and return the dom"
  (let* ((url (or url (read-string "url: ")))
         (buf (with-timeout
                  (html2org-timeout
                   (error "fetch %s failed in %d seconds" url html2org-timeout))
                (url-retrieve-synchronously url))))
    (prog1 (ignore-errors (with-current-buffer buf
                            (goto-char url-http-end-of-headers)
                            (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun html2org-tag-a (dom)
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
        (start (point)))
    (when (and shr-target-id
               (equal (dom-attr dom 'name) shr-target-id))
      ;; We have a zero-length <a name="foo"> element, so just
      ;; insert...  something.
      (when (= start (point))
        (shr-ensure-newline)
        (insert " "))
      (put-text-property start (1+ start) 'shr-target-id shr-target-id))
    (if title
        (insert (format "[[%s][%s]]" url title))
      (insert (format "[[%s]]" url)))))

(defun html2org-transform-dom (dom)
  "Transform `DOM' into org file content"
  (let ((shr-external-rendering-functions '((a . html2org-tag-a))))
    (with-temp-buffer
      (shr-insert-document dom)
      (buffer-string))))

;;;###autoload
(defun html2org (&optional url org-file)
  "Retrive `URL' and write the content into `ORG-FILE' using org format"
  (interactive)
  (let* ((url (or url (read-string "url: ")))
         (dom (html2org-get-dom url))
         (title (dom-text (dom-by-tag dom 'title)))
         (org-file (or org-file (expand-file-name (concat title ".org") html2org-store-dir)))
         (content (html2org-transform-dom dom)))
    (with-temp-file org-file
      (insert "#+TITLE: " title "\n")
      (insert "#+URL: " url "\n")
      (insert "#+AUTHOR: \n")
      (insert "#+TAGS: \n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time)) "\n")
      (insert "#+LANGUAGE:  \n")
      (insert "#+OPTIONS:  H:6 num:nil toc:t \\n:nil ::t |:t ^:nil -:nil f:t *:t <:nil")
      (insert content))))

(provide 'html2org)
