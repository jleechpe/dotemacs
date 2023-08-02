;; -*- lexical-binding: t; -*-

;; * General Emacs related configurations

;; This should only have OotB packages to avoid need for
;; elpaca/use-package configuration

;; ** Startup options
(setq inhibit-startup-screen t)

;; ** Enable disabled features
(put 'downcase-region 'disabled nil)

;; ** Backups
(setq backup-by-copying t
      backup-directory-alist
      `(("." . , (expand-file-name "backups" user-cache-dir)))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; ** Persistence
(setq savehist-file (expand-file-name "history" user-cache-dir))
(savehist-mode 1)

(setq save-place-forget-unreadable-files t
      save-place-file (expand-file-name "places" user-cache-dir))
(save-place-mode 1)

(setq recentf-save-file (expand-file-name "recentf" user-cache-dir))
(recentf-mode 1)
(add-hook 'find-file-hook #'recentf-save-list)

;; ** Abbrevs
;; Keep abbrevs with config (chemacs)
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

;; ** CUA
;; Disable CUA editing but keep rectangle
(setq cua-enable-cua-keys 'nil)

;; ** Fill Column
(setq-default fill-column 80)

;; * Provide
(provide 'config-system)
