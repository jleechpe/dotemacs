;; -*- lexical-binding: t; -*-

;; * Programming related

;; ** Programming language modes
(elpaca-use-package terraform-mode
  :hook ((terraform-mode . terraform-format-on-save-mode)))

(elpaca-use-package powershell
  :defer t)

(elpaca-use-package puppet-mode
  :defer t)

(elpaca-use-package dockerfile-mode
  :defer t)

;; *** DotNet
(elpaca-use-package csharp-mode
  :defer t)

(elpaca-use-package fsharp-mode
  :defer t)

;; *** Java
(elpaca-use-package lsp-java
  :hook (java-mode . lsp))

;; *** Lisp

(elpaca nil
  (use-package emacs-lisp
    :init
    (defun ignore-scratch ()
      (unless (string= (buffer-name) "*scratch*")
        (outshine-mode 1)))
    :hook (emacs-lisp-mode . ignore-scratch)))

;; *** Python
(elpaca-use-package python
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
    (setq python-shell-interpreter "python"))))

(elpaca nil (use-package inferior-python-mode
  :hook (inferior-python-mode . hide-mode-line-mode)))

(elpaca-use-package lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-disable-language-services nil
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-auto-import-completions t
        lsp-pyright-use-library-code-for-types t)
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright) (lsp-deferred)))))

(elpaca-use-package blacken
  :defer t
  :after python
  :hook (python-mode . blacken-mode))

;; ** Data
(elpaca-use-package yaml-mode
  :defer t)

(elpaca-use-package json-mode
  :defer t)

;; ** OS Packages
(elpaca-use-package systemd
  :defer t)

;; * Provide
(provide 'config-programming)
