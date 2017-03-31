(load (expand-file-name "html2org.el" (file-name-directory (or buffer-file-name
                                                 load-file-name))))

(defgroup emacs-document nil
  "Group for emacs-document.")

(defcustom emacs-document-directory "~/github/emacs-document"
  "The root directory of emacs-document."
  :type 'directory
  :group 'emacs-document)

(defun emacs-document-raw-directory ()
  (file-name-as-directory (expand-file-name "raw" emacs-document-directory)))

(defun emacs-document-processing-directory ()
    (file-name-as-directory (expand-file-name "processing" emacs-document-directory)))

(defun emacs-document-add-new ()
  "新增一篇待翻译的文章

输入url,然后转成$title.org放到raw目录下"
  (interactive)
  (let* ((default-directory emacs-document-directory)
         (dom (html2org-get-dom))
         (org-content (html2org-transform-dom dom))
         (org-title (progn (string-match "#\\+URL: \\([^\n]+\\)" org-content)
                           (match-string-no-properties 1 org-content)))
         (org-file (expand-file-name (concat org-title ".org") (emacs-document-raw-directory))))
    (with-temp-file org-file
      (insert org-content))
    (vc-git-register (list org-file))
    (vc-git-checkin (list org-file) (format "add raw post %s" org-title))
    (vc-git-push nil)))

(defun emacs-document-new-translation ()
  "开始新的翻译

从raw目录中挑选一篇文章放到processing目录下,然后打开这篇文章开始翻译
若raw目录中也没有待翻译的文章,调用`emacs-document-new' 新增一篇待翻译的文章,然后再调用`emacs-document-new-translation' 开始翻译"
  (interactive)
  (let* ((default-directory emacs-document-directory)
         (raw-directory (emacs-document-raw-directory))
         (raw-file (read-file-name "请选择要翻译的文章: " raw-directory nil t))
         (filename (file-name-nondirectory raw-file))
         (process-file (expand-file-name filename (emacs-document-processing-directory))))
    (if (vc-git-registered raw-file)
        (vc-git-rename-file raw-file process-file)
      (rename-file raw-file process-file)
      (vc-git-register (list process-file)))
    (vc-git-checkin (list raw-file process-file) (format "processing post %s" filename))
    (vc-git-push nil)
    (find-file process-file)))

(vc-registered "~/github/emacs-document/raw/literate-devops.org")

(defun emacs-document-start-translation ()
  "开始翻译

从processing目录中选择一篇文章开始翻译
若processing目录中没有要翻译的文章,调用`emacs-document-new-translation' 开始新的翻译 "
  (interactive)
  (let* ((processing-dir (emacs-document-processing-directory))
         (processing-files (remove-if-not #'file-regular-p (directory-files processing-dir t))))
    (if processing-files
        (find-file (read-file-name "请选择要翻译的文章: " processing-dir nil t))
      (emacs-document-new-translation))))

(defun emacs-document-finish-translation ()
  "翻译结束

将当前文件从raw目录移动到其他目录中"
  (interactive)
  (let ((default-directory emacs-document-directory)
        (current-file (buffer-file-name))
        (new-file (read-file-name "Moved to: " emacs-document-directory)))
    (vc-git-rename-file current-file new-file)
    (vc-git-checkin (list current-file new-file) "change category")
    (vc-git-push)
    (kill-buffer)))
