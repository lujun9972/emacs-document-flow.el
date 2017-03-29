(defun html2org-get-dom (&optional url)
  (let* ((url (or url (read-string "输入url")))
         (buf (url-retrieve-synchronously url)))
    (prog1 (ignore-errors (with-current-buffer buf
                            (goto-char url-http-end-of-headers)
                            (libxml-parse-html-region (point) (point-max) url t)))
      (kill-buffer buf))))

(defun html2org-funcall (fn &rest args)
  (if (functionp fn)
      (apply fn args)
    ""))

(defun html2org-transform-dom (dom &optional transformers-alist ignore-tags)
  (let* ((ignore-tags (or ignore-tags '(script style comment)))
         (tag (car dom))
         (attrs (cadr dom))
         ;; (subdoms (remove-if #'stringp  (cddr dom)))
         ;; (text (string-trim (string-join (remove-if-not #'stringp (cddr dom)) " ")))
         (subdoms (cddr dom))
         (tag-transformers (assoc-string tag transformers-alist))
         (tag-start-transformer (or (second tag-transformers)
                              (intern (format "html2org--%s-start-transformer" tag))))
         (tag-data-transformer (or (third tag-transformers)
                              (intern (format "html2org--%s-data-transformer" tag))))
         (tag-end-transformer (or (fourth tag-transformers)
                              (intern (format "html2org--%s-end-transformer" tag)))))
    (unless (member tag ignore-tags)
      (concat (html2org-funcall tag-start-transformer attrs)
              (mapconcat (lambda (dom)
                           (if (stringp dom)
                               (if (functionp tag-data-transformer)
                                   (funcall tag-data-transformer attrs dom)
                                 (replace-regexp-in-string "^[\r\n]+$" "\n" dom))
                             (html2org-transform-dom dom))) subdoms "")
              (html2org-funcall tag-end-transformer attrs)))))

(defun html2org--title-data-transformer (attrs text)
  (format "#+URL: %s\n" (string-trim text)))


(defun html2org--b-before-transformer (attrs)
  "\n")
(defun html2org--b-data-transformer (attrs text)
  text)
(defun html2org--b-end-transformer (attrs)
  "\n")

(defun html2org--p-before-transformer (attrs)
  "\n\n")
(defun html2org--p-data-transformer (attrs text)
  text)
(defun html2org--p-end-transformer (attrs)
  "\n\n")

(defun html2org--img-data-transformer (attrs text)
  (format "[[%s]]" (cdr (assoc-string 'src attrs))))

(defun html2org--a-start-transformer (attrs)
  (format "[[%s][" (cdr (assoc-string 'href attrs))))
(defun html2org--a-data-transformer (attrs text)
  text)
(defun html2org--a-end-transformer (attrs)
  (format "]]" (cdr (assoc-string 'href attrs))))

(defun html2org--input-data-transformer (attrs text)
  (unless (equal (cdr (assoc-string 'type attrs))
                 "hidden")
    (cdr (assoc-string 'value attrs))))

(defun html2org--div-data-transformer (attrs text)
  text)

(defun html2org--li-data-transformer (attrs text)
  (format "\n+ %s" text))

;; (defun html2org--code-data-transformer (attrs text)
;;   text)

(defun html2org--h1-data-transformer (attrs text)
  (format "* %s\n" (string-trim text)))

(defun html2org--h2-data-transformer (attrs text)
  (format "** %s\n" (string-trim text)))

(defun html2org--h3-data-transformer (attrs text)
  (format "*** %s\n" (string-trim text)))

;; (setq dom (html2org-get-dom "http://www.baidu.com"))

;; (setq dom (html2org-get-dom "http://nullprogram.com/blog/2014/10/21/"))

;; (with-current-buffer (get-buffer-create "*test*")
;;   (erase-buffer)
;;   (insert (html2org-transform-dom dom))
;;   (org-mode))

(provide 'html2org)
