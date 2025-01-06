;; -*- lexical-binding: t; -*-

;; * Time Tracking
(use-package wakatime-mode
  :init
  (global-wakatime-mode 1))

;; * Programming related

;; ** Ensure editor configuration
;; (use-package editorconfig
;;   :disabled t
;;   :config (editorconfig-mode 1))

(use-package envrc
  :config (envrc-global-mode))

;; ** Programming language modes
;; *** Dockerfile
(use-package dockerfile-mode
  :disabled t
  :defer t)

(defun my/ts-fontification (n custom)
  ""
  (let* ((original (nth n treesit-font-lock-feature-list)))
    (mapc (lambda (x) (add-to-list 'original x 't)) custom)
    (setf (nth n treesit-font-lock-feature-list) original)
    (treesit-font-lock-recompute-features)))

(defun my/dockerfile-ts-fontification ()
  (my/ts-fontification 3 '(definition)))

(use-package dockerfile-ts-mode
  :ensure nil
  :config
  (setq my/dockerfile-ts-mode--font-lock-settings
        (treesit-font-lock-rules
         :language 'dockerfile
         :feature 'definition
         '((arg_instruction (unquoted_string) @font-lock-variable-name-face)
           (env_pair name: (unquoted_string) @font-lock-variable-name-face)
           (env_pair value: (unquoted_string) @font-lock-constant-face)
           (workdir_instruction (path) @font-lock-constant-face)
           (label_pair key: (unquoted_string) @font-lock-variable-name-face)
           (label_pair value: (unquoted_string) @font-lock-constant-face))
         :language 'dockerfile
         :feature 'string
         '((json_string) @font-lock-string-face))
        dockerfile-ts-mode--font-lock-settings
        (append dockerfile-ts-mode--font-lock-settings
                my/dockerfile-ts-mode--font-lock-settings))

  :hook ((dockerfile-ts-mode . my/dockerfile-ts-fontification)))

;; *** DotNet
(use-package csharp-mode
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist
               '(csharp-mode . csharp-ts-mode))
  :defer t)

(use-package fsharp-mode
  :defer t)

;; *** Fish shell
(use-package fish-mode
  :defer t)

;; *** Java

;; (use-package lsp-java
;;   :disabled t
;;   :hook (java-mode . lsp)
;;   :after (treemacs))

;; *** Javascript

;; Indent to 2 since I always set it manually
(setq js-indent-level 2)

;; *** Just
(use-package just-mode
  :defer t)

(use-package justl
  :general
  (:keymaps 'project-prefix-map
            "j" #'justl))

;; *** Lisp

(use-package emacs-lisp
  :ensure nil
  :init
  (defun ignore-scratch ()
    (unless (string= (buffer-name) "*scratch*")
      (outshine-mode 1)))
  :hook (emacs-lisp-mode . ignore-scratch))

;; *** Lua
(use-package lua-mode
  :defer t)

;; *** Powershell
(use-package powershell
  :defer t)

;; *** Puppet
(use-package puppet-mode
  :defer t)

;; *** Python
(use-package python
  :defer t
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))
  (setq eglot-python-backend
        `((,(executable-find "pyright-langserver") "--stdio") "ruff-lsp"))
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-flymake-command '("flake8" "--max-line-length=88" "-"))
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
  ;; Match fill column to black settings
  :hook ((python-base-mode . (lambda () (set-fill-column 88)))))

(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

(use-package python-pytest
  :ensure t
  :defer t
  :general
  (:keymaps 'python-base-mode-map
            "C-c t" #'python-pytest-dispatch))

(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load)
  :config
  (defun update-ruff-codes ()
    (interactive)
    (let* ((codes (car (remove flymake-ruff--curr-codes
                               flymake-ruff--codes)))
           (args `("check" "--quiet"
                   "--preview" ; enables beta checks
                   ,@(flatten-list (mapcar (lambda (code) (list "--select" code))
                                           codes)) ;codes to select
                   "--output-format" "concise"
                   "--exit-zero" "-")))
      (setq flymake-ruff--curr-codes codes
            flymake-ruff-program-args args)))
  (setq flymake-ruff--codes '(("ALL")("E" "W" "F"))
        flymake-ruff--curr-codes '("ALL"))
  (update-ruff-codes))

;; *** Salt
(use-package salt-mode
  :defer t)

;; *** Terraform
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'"
  :init
  (add-to-list 'eglot-server-programs
               '(terraform-mode "terraform-ls" "serve"))
  :after eglot)




;; ** Data
(use-package sxhkdrc-mode
  :defer t)

(use-package dhall-mode
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package json-ts-mode
  :ensure nil
  :defer nil ; Do not defer since json-mode is from a different file
  :init
  (add-to-list 'major-mode-remap-alist
               '(json-mode . json-ts-mode)))

;; ** OS Packages
(use-package systemd
  :defer t)

;; * Provide
(provide 'config-programming)
