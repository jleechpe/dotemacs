;; -*- lexical-binding: t; -*-

;; * EShell

(setq eshell-directory-name (expand-file-name "eshell/" user-cache-dir))

;; * Eat
(use-package eat
  :general
  (:keymaps 'project-prefix-map
            "E" #'eat-project))

;; * Provide
(provide 'config-shell)
