;; -*- lexical-binding: t; -*-

;; * Buffer/Window Management

;; Extract Buffer and Window Management related configuration into a single
;; location.  Currently there should be very little but it can expand
;; ** Ibuffer / Buffer list
(use-package ibuffer
  :demand t
  :ensure nil
  :general
  ([remap list-buffers] #'ibuffer))

(defun my/ibuffer-project-run ()
  (setq ibuffer-filter-groups
        (ibuffer-project-generate-filter-groups))
  (unless (eq ibuffer-sorting-mode 'project-file-relative)
    (ibuffer-do-sort-by-project-file-relative)))

(use-package ibuffer-project
  :demand t
  :hook (ibuffer . my/ibuffer-project-run))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :hook (ibuffer-sidebar-mode . my/ibuffer-project-run))

;; ** Popup windows
(use-package popper
  :defer t
  :init
  ;; Window Definitions
  (setq popper-group-function #'popper-group-by-project
        popper-reference-buffers
        '("\\*eldoc for.*\\*$"
          "\\*Messages\\*"
          help-mode
          flymake-diagnostics-buffer-mode
          compilation-mode))
  ;; Must come before enabling
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :general
  ("C-`" #'popper-toggle)
  ("M-`" #'popper-cycle)
  ("C-M-`" #'popper-toggle-type))

;; ** Window management
(use-package ace-window
  :general ("M-o" #'ace-window)
  :config
  (ace-window-display-mode 1)
  (ace-window-posframe-mode 1)
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground (doom-color 'red)
                      :background (doom-color 'base7)
                      :height 6.0))

;; ** File Treeview
;; Treemacs allows management of projects/workspaces and filtering by
;; perspective to show desired folders rather than purely follow the
;; current file.

(use-package treemacs
  :init
  (setq treemacs-map (make-sparse-keymap "Treemacs"))
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
  (:prefix "C-c"
           "t" treemacs-map)
  (:keymaps 'treemacs-map
            "1"   #'delete-other-window
            "t"   #'treemacs
            "B"   #'treemacs-bookmark
            "C-t" #'treemacs-find-file
            "M-t" #'treemacs-find-tag))

(use-package all-the-icons
  :config
  (require 'all-the-icons))

(use-package all-the-icons-completion
  :init (all-the-icons-completion-mode)
  :hook (marginalia-mode . #'all-the-icons-completion-marginalia-setup))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

;; * Provides
(provide 'config-buffer)
