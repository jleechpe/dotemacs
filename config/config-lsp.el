;; -*- lexical-binding: t; -*-

(elpaca-use-package flycheck
  :defer t
  ;; :hook ((yaml-mode powershell-mode) . flycheck-mode)
  )

(elpaca-use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil
        lsp-session-file (expand-file-name ".lsp-session-v1" user-cache-dir))
  (defun corfu-lsp-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (setq lsp-completion-provider :none)
  :hook (((powershell-mode
           yaml-mode
           dockerfile-mode
           csharp-mode
           terraform-mode
           vue-mode) . lsp-deferred)
         (lsp-mode . yas-minor-mode)
         (lsp-completion-mode . corfu-lsp-completion)))

(elpaca-use-package lsp-ui
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

(elpaca-use-package lsp-treemacs
  :after lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))


;; * Provide
(provide 'config-lsp)

