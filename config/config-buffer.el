;; -*- lexical-binding: t; -*-

;; * Buffer/Window Management

;; Extract Buffer and Window Management related configuration into a single
;; location.  Currently there should be very little but it can expand
;; ** Ibuffer / Buffer list
(use-package ibuffer
  :ensure t
  :elpaca nil
  :general
  ([remap list-buffers] #'ibuffer))

(defun my/ibuffer-project-run ()
  (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative)))

(use-package ibuffer-project
  :ensure t
  :hook (ibuffer . my/ibuffer-project-run))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :hook (ibuffer-sidebar-mode . my/ibuffer-project-run))

;; ** Popup windows
(use-package popper
  :defer t
  :init
  ;; Window Definitions
  (setq popper-group-function #'popper-group-by-project)
  ;; Must come before enabling
  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  :general
  ("C-`" #'popper-toggle-latest)
  ("M-`" #'popper-cycle)
  ("C-M-`" #'popper-toggle-type))

;; ** Window management
(use-package ace-window
  :general ("M-o" #'ace-window)
  :config (ace-window-display-mode 1))

;; * Provides
(provide 'config-buffer)
