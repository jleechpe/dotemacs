;; -*- lexical-binding: t; -*-

;; * General Emacs related configurations

;; This should only have OotB packages to avoid need for
;; elpaca/use-package configuration

;; ** Enable disabled features
(put 'downcase-region 'disabled nil)

;; ** Backups
(setq backup-by-copying t
      backup-directory-alist
      `(("," . , (expand-file-name "backups" user-cache-dir)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; ** Persistence
(setq savehist-file (expand-file-name "history" user-cache-dir))
(savehist-mode 1)

(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(setq recentf-save-file (expand-file-name "recentf" user-cache-dir))
(recentf-mode 1)

;; ** CUA
;; Disable CUA editing but keep rectangle
(setq cua-enable-cua-keys 'nil)

;; * Provide
(provide 'config-system)
