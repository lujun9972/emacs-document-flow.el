(require 'cl-lib)
(require 'url2orgfile)

(defgroup emacs-document nil
  "Group for emacs-document.")

(defcustom emacs-document-directory "~/github/emacs-document"
  "The root directory of emacs-document."
  :type 'directory
  :group 'emacs-document)

(defcustom emacs-document-auto-update-readme t
  "Non nil means update README.org automatically"
  :type 'boolean
  :group 'emacs-document)

(defcustom emacs-document-auto-push t
  "Non nil means push to remote repo automatically"
  :type 'boolean
  :group 'emacs-document)

(defun emacs-document-raw-directory ()
  (file-name-as-directory (expand-file-name "raw" emacs-document-directory)))

(defun emacs-document-processing-directory ()
    (file-name-as-directory (expand-file-name "processing" emacs-document-directory)))

(defun emacs-document-update-org-option (option value)
  "更新当前buffer中org文件的metadata,将OPTION对应的值更像为VALUE.

该函数会返回OPTION原始的值"
  (save-excursion
    (goto-char (point-min))
    (let* ((regexp (org-make-options-regexp (list option)))
           (origin-value (progn (re-search-forward regexp)
                                (match-string 2))))
      (replace-match value t t nil 2)
      origin-value)))

(defun emacs-document-update-readme ()
  (interactive)
  (let ((default-directory emacs-document-directory)
        (readme (expand-file-name  "README.org" emacs-document-directory)))
    (with-temp-file readme
      (insert (shell-command-to-string  "./generate_index.sh")))
    (vc-git-checkin (list readme) "update README")))

(defun emacs-document-add-raw (&optional url)
  "新增一篇待翻译的文章

输入url,然后转成$title.org放到raw目录下"
  (interactive)
  (let* ((url (or url (read-string "输入url: " (cond ((eq major-mode 'eww-mode)
                                                      (eww-current-url))
                                                     ((eq major-mode 'w3m-mode)
                                                      w3m-current-url)
                                                     (t "")))))
         (default-directory emacs-document-directory)
         (url2orgfile-store-dir (emacs-document-raw-directory))
         (org-file (url2orgfile url nil
                             :URL url
                             :AUTHOR user-login-name
                             :DATE (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
                             :TAGS "raw"
                             :LANGUAGE "zh-CN"
                             :OPTIONS "H:6 num:nil toc:t \\n:nil ::t |:t ^:nil -:nil f:t *:t <:nil")))
    (vc-git-register (list org-file))
    (vc-git-checkin (list org-file) (format "add raw post %s" url))
    (when emacs-document-auto-update-readme
      (emacs-document-update-readme))
    (when emacs-document-auto-push
      (vc-git-push nil))))

(defun emacs-document-new-translation ()
  "开始新的翻译

从raw目录中挑选一篇文章放到processing目录下,然后打开这篇文章开始翻译."
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
    (find-file process-file)
    (emacs-document-update-org-option "TAGS" "processing")
    (save-buffer)
    (vc-git-checkin (list raw-file process-file) (format "processing post %s" filename))
    (when emacs-document-auto-update-readme
      (emacs-document-update-readme))
    (when emacs-document-auto-push
      (vc-git-push nil))))

(defun emacs-document-continue-translation ()
  "开始翻译

从processing目录中选择一篇文章开始翻译."
  (interactive)
  (let* ((processing-dir (emacs-document-processing-directory))
         (processing-files (cl-remove-if-not #'file-regular-p (directory-files processing-dir t))))
    (if processing-files
        (find-file (read-file-name "请选择要翻译的文章: " processing-dir nil t))
      (emacs-document-new-translation))))

(defun emacs-document-finish-translation ()
  "翻译结束

将当前文件从raw目录移动到其他目录中"
  (interactive)
  (let* ((default-directory emacs-document-directory)
         (current-file (buffer-file-name))
         (new-file (read-file-name "Moved to: " (file-name-as-directory emacs-document-directory)))
         (new-category (file-relative-name (file-name-directory new-file)))
         (new-name (file-name-base new-file)))
    (emacs-document-update-org-option "TITLE" new-name)
    (emacs-document-update-org-option "TAGS" new-category)
    (emacs-document-update-org-option "DATE" (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time)))
    (save-buffer)
    (vc-git-rename-file current-file new-file)
    (vc-git-checkin (list current-file new-file) "change category")
    (when emacs-document-auto-update-readme
      (emacs-document-update-readme))
    (when emacs-document-auto-push
      (vc-git-push nil))
    (kill-buffer)))
