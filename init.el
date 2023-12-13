;; -*- lexical-binding: t; -*-

;; * 2022 Startup (switch to elpaca)

(require 'config-init (expand-file-name "config-init.el" user-emacs-directory))

;; Always require package management, os and host specific settings,
;; failing silently when os/host are not present
(config-require packagemanagement)
(config-require os :fail-silently t)
(config-require system-name :fail-silently t)

;; 
(config-packages '(system
                   server
                   dependencies
                   buffer
                   shell
                   org
                   lisp
                   completion
                   lsp
                   mgmt
                   programming
                   textediting
                   ui
                   vc
                   mail))
