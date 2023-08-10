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
  ("<f12>" #'magit-status))

(use-package transient
  :init
  (setq transient-levels-file (expand-file-name "transients/levels.el" user-cache-dir)
        transient-values-file (expand-file-name "transients/values.el" user-cache-dir)
        transient-history-file (expand-file-name "transients/history.el" user-cache-dir)))

(elpaca nil
  (use-package treemacs-magit
    :after (treemacs magit)))

(use-package forge
  :defer t)

;; ** Fossil
(elpaca vc-fossil)

;; * Provide
(provide 'config-vc)
