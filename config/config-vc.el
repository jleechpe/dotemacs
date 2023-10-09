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
  :general
  ("<f12>" #'magit-status)
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package transient
  :init
  (setq transient-levels-file (expand-file-name "transients/levels.el" user-cache-dir)
        transient-values-file (expand-file-name "transients/values.el" user-cache-dir)
        transient-history-file (expand-file-name "transients/history.el" user-cache-dir)))

(elpaca nil
  (use-package treemacs-magit
    :after (treemacs magit)))

(use-package forge
  :init
  (setq forge-database-file (expand-file-name "forge-database.sqlite" user-cache-dir))
  :defer t)

;; ** Fossil
(elpaca vc-fossil)

;; * Provide
(provide 'config-vc)
