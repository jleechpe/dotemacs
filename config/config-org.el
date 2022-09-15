;; -*- lexical-binding: t; -*-
(defcustom user-org-dir "~/org" "Default directory for org files"
  :type 'directory
  :group 'config-dirs)
(defcustom user-roam-dir (expand-file-name "roam" user-org-dir)
  "Directory for OrgRoam"
  :type 'directory
  :group 'config-dirs)

(defun org-daily-agenda (arg)
  (interactive) "P"
  (org-agenda arg "a"))

(elpaca-use-package org
  :defer t
  :hook
  ((org-mode . auto-fill-mode)
   (org-mode . flyspell-mode)
   (org-after-todo-statistics . my/org-statistics-update)
   (org-checkbox-statistics . my/org-checkbox-statistics-update))
  :config
  (add-to-list 'org-modules 'org-habit)

  (setq
   ;; Overall Org
   org-startup-indented t
   org-agenda-compact-blocks t
   org-log-into-drawer t
   org-clock-into-drawer t
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-refile-targets '((nil :maxlevel . 3))
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t

   ;; Publishing
   org-publish-project-alist
   '(("vitae"
      :base-directory (expand-file-name "vitae" user-repo-dir)
      :publishing-directory (expand-file-name "vitae/export" user-repo-dir)
      :publishing-function org-latex-publish-to-pdf))

   ;; Todo Configs
   org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "PEND(n@)" "|" "DONE(d@)")
     (sequence "TODO(t)" "PROG(p)" "BLOCK(b)" "|" "DONE(d@)")
     (type "RISK(r@)" "ISSUE(i@)" "|" "AVRT(a@)")
     (sequence "|" "CANC(c@)"))
   org-todo-keyword-faces
        `(("DEPR" . (:foreground  ,(theme-color 'orange) :weight bold))
          ("PROG" . (:foreground  ,(theme-color 'dark-blue) :weight bold))
          ("PEND" . (:foreground  ,(theme-color 'magenta) :weight bold))
          ("BLOCK" . (:foreground  ,(theme-color 'orange) :inverse-video t))
          ("RISK" . (:foreground  ,(theme-color 'red) :weight bold))
          ("ISSUE" . (:foreground  ,(theme-color 'red) :inverse-video t))
          ("AVRT" . (:foreground  ,(theme-color 'cyan) :weight bold))))

  ;; Update statistics
  (defun my/org-statistics-update (n-done n-not-done)
    "Switch to appropriate DONE (Completed) or TODO (Starting)
    state when statistics are updated."
    (let ((org-inhibit-logging 'note)) ; turn off logging
      (org-todo (if (= n-not-done 0) 'done 1))))
  (defun my/org-checkbox-statistics-update ()
    (let ((todo-state (org-get-todo-state))
          (cookie-re "\\[[0-9/%]+\\]")
          (org-inhibit-logging 'note)
          beg end value done)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (when (re-search-forward cookie-re end t)
            (goto-char (match-beginning 0))
            (setq value
                  (substring (org-element-statistics-cookie-interpreter
                              (org-element-statistics-cookie-parser) t)
                             1 -1))
            (message "Value" value )
            (cond
             ((equal value "100%")
              (setq done 'done))
             ((string-match "%" value)
              (setq done 1))
             ((apply #'equal (split-string value "/"))
              (setq done 'done))
             (t
              (setq done 1)))
            (org-todo done))))))
  :general
  ("<f5>" #'org-daily-agenda)
  ("S-<f5>" #'org-agenda))

;; ** Roam
(elpaca-use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
        org-roam-directory user-roam-dir
        org-roam-db-location (expand-file-name "db/org-roam.db" user-org-dir))
  (defun my/org-roam-update-links-on-save ()
    (add-hook 'before-save-hook 'org-roam-link-replace-all nil 't))
  (defun my/org-completion-completers ()
    (mapc (lambda (x)
            (add-to-list 'completion-at-point-functions x))
          org-roam-completion-functions)
    (add-to-list 'completion-at-point-functions #'cape-abbrev))
  :config (org-roam-db-autosync-mode 1)
  :hook ((org-mode . my/org-completion-completers)
         (org-mode . my/org-roam-update-links-on-save)))

(elpaca-use-package org-roam-ui
  :defer t)

;; ** Outlining
(elpaca-use-package outshine
  :commands outshine-mode
  :config
  :diminish "Outl")

(elpaca-use-package outorg
  :after org
  :defer t
  :commands (outorg-edit-as-org))

(elpaca nil
  (use-package outline
    :diminish outline-minor-mode))

;; * Provide
(provide 'config-org)
