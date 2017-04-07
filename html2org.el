(defun html2org-get-dom (&optional url)
  (let* ((url (or url (read-string "输入url: ")))
         (buf (url-retrieve-synchronously url)))
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
  (let ((shr-external-rendering-functions '((a . html2org-tag-a))))
    (with-temp-buffer
      (shr-insert-document dom)
      (buffer-string))))

;; (let ((buf (get-buffer-create "*test*")))
;;   (with-current-buffer buf
;;     (insert (html2org-transform-dom dom))
;;     (org-mode)))
