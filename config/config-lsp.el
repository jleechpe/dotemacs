;; -*- lexical-binding: t; -*-

(use-package eglot
  :init
  (defun eglot-next-backend ()
    "Switch between chosen backends for MAJOR-MODE.

Use variable `eglot-MODE-backend' (MODE just first word to allow
for TS/non-ts) to define alternatives to cycle between.  Cycling
values become buffer local since they overwrite
`eglot-server-programs' to leverage eglot lifecycle.
"
    (interactive)
    (let* ((name (car (split-string (symbol-name major-mode) "-")))
           (var (intern (format "eglot-%s-backend" name))))
      (message "%s" var)
      (if (boundp var)
          (let* ((val (symbol-value var))
                 (contact (eglot--guess-contact))
                 (item (nth 3 contact))
                 (e (-elem-index item val))
                 (l (-last-item val))
                 (list-l (if (listp l)
                             l
                           (list l)))
                 (new-eglot (if (-same-items? item list-l)
                                (nth 0 val)
                              (nth (+ 1 e) val))))
            (setq-local eglot-server-programs
                        (list (cons major-mode (if (listp new-eglot)
                                                   new-eglot
                                                 (list new-eglot)))))))
      (eglot-shutdown (eglot-current-server))
      (eglot-ensure)))
  :hook
  (((dockerfile-ts-mode
     terraform-mode
     python-base-mode) . eglot-ensure)))

(use-package eglot-tempel
  :ensure (:host github :repo "jleechpe/eglot-tempel")
  :after (eglot)
  :init
  (eglot-tempel-mode 1))

(use-package eldoc-box
  :init
  (eldoc-box-hover-mode 1)
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)))

;; (use-package lsp-mode
;;   :defer t
;;   :commands lsp
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   :init
;;   (setq lsp-prefer-flymake nil
;;         lsp-session-file (expand-file-name ".lsp-session-v1" user-cache-dir)
;;         lsp-semantic-tokens-enable t)
;;   (defun corfu-lsp-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   :config
;;   (setq lsp-completion-provider :none)
;;   (require 'lsp-modeline)
;;   (require 'lsp-headerline)
;;   :hook (((powershell-mode
;;            yaml-mode
;;            dockerfile-mode
;;            csharp-mode
;;            terraform-mode
;;            vue-mode) . lsp-deferred)
;;          (lsp-mode . yas-minor-mode)
;;          (lsp-completion-mode . corfu-lsp-completion)))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-symbol t
;;         lsp-ui-doc-enable nil
;;         lsp-modeline-code-actions-enable t
;;         lsp-modeline-diagnostics-enable nil)
;;   :general
;;   (lsp-ui-mode-map
;;    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
;;    [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (lsp-command-map
;;    "s-n" #'lsp-ui-imenu))

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :config
;;   (lsp-treemacs-sync-mode 1)
;;   :after (treemacs))

;; (use-package dap-mode
;;   :defer t
;;   :after lsp-mode
;;   :config
;;   (dap-auto-configure-mode)
;;   :after (lsp-treemacs))

;; * Provide
(provide 'config-lsp)

