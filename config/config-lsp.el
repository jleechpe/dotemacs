;; -*- lexical-binding: t; -*-

(use-package flycheck
  :defer t
  :hook ((yaml-mode powershell-mode text-mode) . flycheck-mode)
  )

(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil
        lsp-session-file (expand-file-name ".lsp-session-v1" user-cache-dir)
        lsp-semantic-tokens-enable t
        )
  (defun corfu-lsp-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (setq lsp-completion-provider :none)
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  :hook (((powershell-mode
           yaml-mode
           dockerfile-mode
           csharp-mode
           terraform-mode
           vue-mode) . lsp-deferred)
         (lsp-mode . yas-minor-mode)
         (lsp-completion-mode . corfu-lsp-completion)))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-symbol t
        lsp-ui-doc-enable nil
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable nil)
  :general
  (lsp-ui-mode-map
   [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
   [remap xref-find-references] #'lsp-ui-peek-find-references)
  (lsp-command-map
   "s-n" #'lsp-ui-imenu))

(use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package dap-mode
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; * Provide
(provide 'config-lsp)

