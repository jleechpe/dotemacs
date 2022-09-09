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
(show-paren-mode 1)

;; ** Font

(set-fontset-font t nil emoji-font)
(set-face-attribute 'default nil
                    :family emacs-font
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; ** Theme
;; Rainbow mode colors text when a color is recognized
(elpaca-use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Solaire makes non-file buffers slightly different background to
;; catch attention
(elpaca solaire-mode
  (solaire-global-mode 1))

(elpaca-use-package doom-themes
  :config
  (defun theme-color (color)
    (nth 2 (assoc color doom-themes--colors)))
  (load-theme emacs-theme t))

;; ** Modeline
(elpaca minions (minions-mode 1))

(elpaca-use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t
        doom-modeline-checker-simple-format t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-enable-word-count t
        doom-modeline-hud t
        doom-modeline-icon t
        doom-modeline-continuous-word-count-modes '(markdown-mode org-mode)
        doom-modeline-indent-info t
        doom-modeline-env-version t
        doom-modeline-env-load-string "?env?")
  )

;; ** Delimiters
(elpaca-use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ** File Treeview
;; Treemacs allows management of projects/workspaces and filtering by
;; perspective to show desired folders rather than purely follow the
;; current file.

(elpaca-use-package treemacs
  :config
  (treemacs-fringe-indicator-mode 'only-when-focused)
  (defun my/treemacs-setup-title ()
    (let* ((bg (face-attribute 'default :background))
           (bg2 (doom-lighten bg 0.2))
           (fg (face-attribute 'default :foreground)))
      (face-remap-add-relative
       'header-line
       :background bg :foreground fg
       :box `(:line-width ,(/ (line-pixel-height) 4) :color ,bg2))))
  :hook (treemacs-mode . my/treemacs-setup-title)
  :commands (treemacs-select-window)
  :general
  ("M-0" #'treemacs-select-window)
  (:prefix "s-t"
           "1"   #'delete-other-window
           "t"   #'treemacs
           "B"   #'treemacs-bookmark
           "C-t" #'treemacs-find-file
           "M-t" #'treemacs-find-tag))

;; (elpaca-use-package all-the-icons-completion
;;   :init (all-the-icons-completion-mode)
;;   :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup))

;; * UX
;; ** Defaults
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

;; ** Keybindings
(elpaca which-key (which-key-mode 1))
(elpaca nil
  (general-def
  "C-M-i" #'delete-indentation
  "C-M-=" #'align-regexp))

;; ** Diffs
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;; ** Search info
(elpaca-use-package anzu
  :init (global-anzu-mode 1)
  :general
  ([remap query-replace] #'anzu-query-replace)
  ([remap query-replace-regexp] #'anzu-query-replace-regexp))

;; ** Undo
(elpaca-use-package undo-tree
  :diminish
  :init
  (setq undo-tree-history-directory-alist
        `((".*" . ,(expand-file-name "undo-tree" user-cache-dir))))
  (global-undo-tree-mode 1))

;; ** SmartParens
(elpaca-use-package smartparens
  :config
  (defun configure-smartparens ()
    (require 'smartparens-config))
  :hook ((smartparens-mode . configure-smartparens)
         (prog-mode . smartparens-mode)))

;; ** Dired extras
(elpaca-use-package (sunrise-commander
                     :host github
                     :repo "sunrise-commander/sunrise-commander")
  :commands sunrise)

;; ** Multiple Cursors
(elpaca-use-package multiple-cursors
  :general
  ("C-<" #'mc/mark-previous-like-this)
  ("C->" #'mc/mark-next-like-this)
  ("C-c C-<" #'mc/mark-all-like-this-dwim))

;; ** Projects
(elpaca-use-package projectile
  :config
  (setq projectile-mode-line-prefix " Prj"
        projectile-completion-system 'auto
        projectile-globally-ignored-files '(".tfstate" ".gitignore"))
  (add-to-list 'projectile-globally-ignored-directories ".terraform")
  :init (projectile-mode 1)
  :general ("C-c p" #'projectile-command-map))

;; ** Window Management
(elpaca-use-package ace-window
  :general ("M-o" #'ace-window)
  :init (ace-window-display-mode 1))

;; * Provide
(provide 'config-ui)
