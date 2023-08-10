;; -*- lexical-binding: t; -*-

;; * Customization variables
(defcustom emacs-font "Fira Code Nerd Font" "Font to use"
  :type 'string
  :group 'config-ui)
(defcustom emoji-font "Noto Color Emoji" "Font for emojis/icons"
  :type 'string
  :group 'config-ui)
(defcustom emacs-theme 'doom-spacegrey "Theme to use"
  :type 'string
  :group 'config-ui)

;; * UI
;; ** Defaults
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode 1)
(column-number-mode 1)

(use-package paren
  :elpaca nil
  :init
  (setq show-paren-context-when-offscreen 'overlay)
  (show-paren-mode 1))

(use-package whitespace
  :elpaca nil
  :init
  (setq whitespace-line-column nil ; Leave at nil to follow `fill-column'
        whitespace-global-modes '(not circe-mode)
        whitespace-style '(tabs newline tab-mark space-mark
                                newline-mark face lines-tail)
        whitespace-display-mappings '(
                                      (space-mark nil)
                                      (newline-mark 10 [172 10])
                                      (tab-mark 9 [183 9] [92 9])))
  :config (global-whitespace-mode 1))

;; ** Font

(set-fontset-font t nil emoji-font)
(set-face-attribute 'default nil
                    :family emacs-font
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; ** Theme
;; Rainbow mode colors text when a color is recognized
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Solaire makes non-file buffers slightly different background to
;; catch attention
(elpaca solaire-mode
  (solaire-global-mode 1))

(use-package doom-themes
  :config
  (defun theme-color (color)
    (nth 2 (assoc color doom-themes--colors)))
  (load-theme emacs-theme t))

;; ** Modeline
(use-package minions
  :config (minions-mode 1))

(use-package doom-modeline
  :config (doom-modeline-mode 1)
  :init
  (setq doom-modeline-minor-modes t
        doom-modeline-checker-simple-format nil
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-enable-word-count t
        doom-modeline-hud t
        doom-modeline-icon t
        doom-modeline-continuous-word-count-modes '(markdown-mode org-mode)
        doom-modeline-indent-info nil
        doom-modeline-buffer-encoding nil
        doom-modeline-env-version t
        doom-modeline-env-load-string "?env?")
  )

;; ** Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; * UX
;; ** Defaults
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

;; ** Keybindings
(use-package which-key
  :config (which-key-mode 1))
(elpaca nil
  (general-def
    "C-M-i" #'delete-indentation
    "C-M-=" #'align-regexp))

;; ** Diffs
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;; ** Search info
(use-package anzu
  :config (global-anzu-mode 1)
  :general
  ([remap query-replace] #'anzu-query-replace)
  ([remap query-replace-regexp] #'anzu-query-replace-regexp))

;; ** Undo
(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name "undo-tree" user-cache-dir))))
  :config
  (global-undo-tree-mode 1))

;; ** SmartParens
(use-package smartparens
  :config
  (defun configure-smartparens ()
    (require 'smartparens-config))
  :hook ((smartparens-mode . configure-smartparens)
         (prog-mode . smartparens-mode)))

;; ** Dired extras
(use-package sunrise-commander
  :disabled t
  :commands sunrise)

;; ** Multiple Cursors
(use-package multiple-cursors
  :general
  ("C-<" #'mc/mark-previous-like-this)
  ("C->" #'mc/mark-next-like-this)
  ("C-c C-<" #'mc/mark-all-like-this-dwim))

;; ** Projects
(use-package project)
(use-package project-mode-line-tag
  :config (project-mode-line-tag-mode))

(use-package project-treemacs
  :after (treemacs project)
  :config
  (project-treemacs-mode))

;; * Provide
(provide 'config-ui)
