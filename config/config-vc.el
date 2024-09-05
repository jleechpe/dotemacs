;; -*- lexical-binding: t; -*-

;; * Version Control

;; ** Diffing
(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (diff-hl-dired-mode 1))

;; ** Git
(use-package magit
  :commands magit-status
  :defer t
  :general
  ("<f12>" #'magit-status)
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package magit-extras
  :ensure nil
  :defer t
  :after magit)

(use-package transient
  :init
  (setq transient-levels-file (expand-file-name "transients/levels.el" user-cache-dir)
        transient-values-file (expand-file-name "transients/values.el" user-cache-dir)
        transient-history-file (expand-file-name "transients/history.el" user-cache-dir)))

(use-package treemacs-magit
  :after (treemacs magit))


(use-package forge
  :init
  (setq forge-database-file (expand-file-name "forge-database.sqlite" user-cache-dir))
  :defer t)

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode))

(use-package magit-todos
  :after magit
  :init (magit-todos-mode 1))

(use-package magit-delta
  :disabled t
  :after (magit)
  :hook (magit-mode . magit-delta-mode))

;; ** Fossil
(elpaca vc-fossil)

;; * Provide
(provide 'config-vc)
