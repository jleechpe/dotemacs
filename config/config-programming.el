;; -*- lexical-binding: t; -*-

;; * Time Tracking
(use-package wakatime-mode
  :init
  (global-wakatime-mode 1))

;; * Programming related

;; ** Ensure editor configuration
(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package envrc
  :config (envrc-global-mode))

;; ** Programming language modes
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'"
  :config
  (setq lsp-terraform-ls-enable-show-reference t)
  )

(use-package lua-mode
  :defer t)

(use-package powershell
  :defer t)

(use-package fish-mode
  :defer t)

(use-package puppet-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

;; *** DotNet
(use-package csharp-mode
  :defer t)

(use-package fsharp-mode
  :defer t)

;; *** Java
(use-package lsp-java
  :hook (java-mode . lsp))

;; *** Javascript

;; Indent to 2 since I always set it manually
(setq js-indent-level 2)

;; *** Lisp

(use-package emacs-lisp
  :elpaca nil
  :init
  (defun ignore-scratch ()
    (unless (string= (buffer-name) "*scratch*")
      (outshine-mode 1)))
  :hook (emacs-lisp-mode . ignore-scratch))

;; *** Python
(use-package python
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython"
            python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))
  ;; Let Vale work with Python (doesn't inherit from base yet)
  (flycheck-add-mode 'vale 'python-mode)
  (flycheck-add-mode 'vale 'python-ts-mode)
  ;; Match fill column to black settings
  :hook ((python-base-mode . (lambda () (set-fill-column 88)))))

(use-package inferior-python-mode
  :elpaca nil
  :hook (inferior-python-mode . hide-mode-line-mode))

(use-package lsp-pyright
  :defer t
  :init
  (setq lsp-pyright-multi-root nil)
  :config
  (setq lsp-pyright-disable-language-services nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t)
  ;; Vale and Flake8 should run automagically
  (flycheck-add-next-checker 'lsp '(warning . python-flake8))
  (flycheck-add-next-checker 'lsp 'vale)
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright) (lsp-deferred)))))

;; ** Data
(use-package yaml-mode
  :defer t)

(use-package json-mode
  :defer t)

;; ** OS Packages
(use-package systemd
  :defer t)

;; * Provide
(provide 'config-programming)
