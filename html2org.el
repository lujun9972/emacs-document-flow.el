(defun html2org-get-dom (&optional url)
  (let* ((url (or url (read-string "输入url")))
         (buf (url-retrieve-synchronously url)))
    (prog1 (ignore-errors (with-current-buffer buf
                            (goto-char url-http-end-of-headers)
                            (libxml-parse-html-region (point) (point-max) nil t)))
      (kill-buffer buf))))

(defun html2org-transform-dom (dom &optional transformers-alist ignore-tags)
  (if (stringp dom)
      dom
    (let* ((ignore-tags (or ignore-tags '(script style comment)))
           (tag (car dom))
           (attrs (cadr dom))
           (subdoms (remove-if #'stringp  (cddr dom)))
           (text (string-trim (string-join (remove-if-not #'stringp (cddr dom)) " ")))
           (tag-transformers (assoc-string tag transformers-alist))
           (tag-start-transformer (or (second tag-transformers)
                                      (intern (format "html2org--%s-start-transformer" tag))))
           (tag-end-transformer (or (third tag-transformers)
                                    (intern (format "html2org--%s-end-transformer" tag)))))
      (unless (member tag ignore-tags)
        (concat (if (functionp tag-start-transformer)
                    (funcall tag-start-transformer attrs text)
                  "")
                (mapconcat #'html2org-transform-dom subdoms "")
                (if (functionp tag-end-transformer)
                    (funcall tag-end-transformer attrs text)
                  ""))))))

(defun html2org--title-start-transformer (attrs text)
  (format "#+URL: %s\n" text))


(defun html2org--b-start-transformer (attrs text)
  (format  "%s\n" text))

(defun html2org--p-start-transformer (attrs text)
  (format "%s\n\n" text))

(defun html2org--img-start-transformer (attrs text)
  (format "[[%s]]" (cdr (assoc-string 'src attrs))))

(defun html2org--a-start-transformer (attrs text)
  (format "[[%s][%s]]" (cdr (assoc-string 'href attrs)) text))

(defun html2org--input-start-transformer (attrs text)
  (unless (equal (cdr (assoc-string 'type attrs))
                 "hidden")
    (cdr (assoc-string 'value attrs))))

(defun html2org--span-start-transformer (attrs text)
  text)

(defun html2org--div-start-transformer (attrs text)
  text)

(defun html2org--li-start-transformer (attrs text)
  (format "\n+ " text))

;; (setq dom (html2org-get-dom "http://www.baidu.com"))

(setq dom (html2org-get-dom "http://nullprogram.com/blog/2014/10/21/"))

(with-current-buffer (get-buffer-create "*test*")
  (erase-buffer)
  (insert (html2org-transform-dom dom)))

