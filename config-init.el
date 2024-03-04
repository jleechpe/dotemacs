;; * Config Loading Setup
;; ** Variables
(defgroup config-loading
  nil ""
  :group 'config-init)
(defgroup config-dirs
  nil ""
  :group 'config-init)
(defgroup config-ui
  nil ""
  :group 'config-init)

;; *** Custom File
;; Set custom file immediately.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; *** Directories
(defvar user-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "Directory to cache files generated by emacs.")
(defvar user-config-dir (expand-file-name "config" user-emacs-directory)
  "Directory for all configurations to go under.")
(defvar user-lisp-dir (expand-file-name "single-lisp" user-emacs-directory)
  "Directory for standalone lisp files that are manually installed.")
(defcustom user-repo-dir (expand-file-name "sources" (getenv "HOME"))
  "User directory that repos are stored in."
  :type 'directory
  :group 'config-dirs)
(defcustom user-org-dir (expand-file-name "org" (getenv "HOME"))
  "User's default directory for org files"
  :type 'directory
  :group 'config-dirs)

;; Add them to load path for use
(add-to-list 'load-path user-config-dir)
(add-to-list 'load-path user-lisp-dir)

;; ** Macros
;; Require config library with prefix and condition to allow
;; configurability
(cl-defmacro config-require (package &key (condition 'var)
                                     &key (prefix "config")
                                     &key (silent 'nil)
                                     &key (fail-silently 'nil))
  ""
  (let* ((n (cond ((string= package "system-name")
                   (setq condition t)
                   (system-name))
                  ((string= package "os")
                   (setq condition t)
                   (cond ((memq system-type '(darwin))
                          "macos")
                         ((memq system-type '(windows-nt ms-dos cygwin))
                          "windows")
                         ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
                          "linux")
                         "other"))
                  ((symbol-name package))))
         (v (intern (format "config-enable-%s" n)))
         (c (cond
             ((eq condition 'var)
              (symbol-value v))
             (condition
              (eval condition))))
         (p (intern
             (mapconcat 'identity
                        (cl-remove-if 'nil
                                      (list prefix n))
                        "-")))
         (r (cond
             ((eq condition 'var)
              v)
             (condition
              condition))))
    (if c
        `(prog1
             ,@(cl-remove-if 'nil
                             (list (unless (or silent config-load-silently)
                                     (message "Config loaded: %s" n))
                                   `(require ',p nil ,fail-silently))))
      (unless (or config-load-silently silent fail-silently)
        `(message "Did not load %s due to %s not being 't." ',package ',r)))))

;; Bulk load packages via list
(defmacro config-packages (packages)
  ""
  `(mapc (lambda (x)
           (eval `(config-require ,x)))
         ,packages))

(defmacro define-config-vars (vars)
  ""
  `(mapcar (lambda (x)
             (let ((k (intern (format "config-enable-%s" (car x))))
                   (v (cdr x)))
               (eval `(defcustom ,k ,v ,(format "If 't enable %s related functionality." k)
                        :type 'boolean
                        :group 'config-loading))))
           ,vars))

;; *** Silence messages
(defvar config-load-silently 'nil
  "If 't do not echo messages on package load")

;; *** Known config files
;; Define known config files
(define-config-vars '((buffer . t)
                      (dependencies . t)
                      (completion . t)
                      (lisp . t)
                      (lsp . t)
                      (mail . nil)
                      (mgmt . t)
                      (org . t)
                      (programming . t)
                      (server . t)
                      (system . t)
                      (textediting . t)
                      (ui . t)
                      (vc . t)
                      (packagemanagement . t)
                      (irc . nil)
                      (shell . t)
                      (os . t)))

;; * Provide
(provide 'config-init)
