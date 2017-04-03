(defun html2org-get-dom (&optional url)
  (let* ((url (or url (read-string "输入url: ")))
         (buf (url-retrieve-synchronously url)))
    (prog1 (ignore-errors (with-current-buffer buf
                            (goto-char url-http-end-of-headers)
                            (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun html2org-transform-dom (dom &optional start-tag-handler data-handler end-tag-handler ignore-tags)
  (let* ((ignore-tags (or ignore-tags '(script style comment)))
         (tag (car dom))
         (attrs (cadr dom))
         (subdoms (cddr dom))
         (start-tag-handler (or start-tag-handler
                                #'html2org-default-start-tag-handler))
         (data-handler (or data-handler
                           #'html2org-default-data-handler))
         (end-tag-handler (or end-tag-handler
                              #'html2org-default-end-tag-handler)))
    (unless (member tag ignore-tags)
      (concat (funcall start-tag-handler tag attrs)
              (mapconcat (lambda (dom)
                           (if (stringp dom)
                               (funcall data-handler tag attrs dom)
                             (html2org-transform-dom dom
                                                     start-tag-handler
                                                     data-handler
                                                     end-tag-handler
                                                     ignore-tags)))
                         subdoms "")
              (funcall end-tag-handler tag attrs)))))


(defun html2org--extract-attr (attr-alist attr)
  (cdr (assoc-string attr attr-alist)))

(defun html2org-default-start-tag-handler (tag attrs)
  (cl-case tag
    (b "\n")
    (p "\n\n")
    (a (format "[[%s][" (html2org--extract-attr attrs 'href)))
    (t "")))

(defun html2org-default-end-tag-handler (tag attrs)
  (cl-case tag
    ;; (b "\n")
    (p "\n\n")
    (a "]]")
    (t  "")))


(defun html2org-default-data-handler (tag attrs text)
  (cl-case tag
    (title (format "#+TITLE: %s\n" (string-trim text)))
    (img (format "[[%s]]" (html2org--extract-attr attrs 'src)))
    (input (unless (equal (cdr (assoc-string 'type attrs))
                          "hidden")
             (html2org--extract-attr attrs 'value)))
    (li (format "\n+ %s" text))
    ((h1 h2 h3 h4 h5) (string-trim text))
    (t (replace-regexp-in-string "[\r\n]+" "\n" text))))



;; (setq dom (html2org-get-dom "http://www.baidu.com"))

;; (setq dom (html2org-get-dom "http://nullprogram.com/blog/2014/10/21/"))

;; (with-current-buffer (get-buffer-create "*test*")
;;   (erase-buffer)
;;   (insert (html2org-transform-dom dom))
;;   (org-mode))

(defun html2org (&optional url)
  (let ((dom (html2org-get-dom url)))
    (html2org-transform-dom dom)))

(provide 'html2org)
