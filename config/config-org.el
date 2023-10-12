;; -*- lexical-binding: t; -*-
(defcustom user-org-dir "~/org" "Default directory for org files"
  :type 'directory
  :group 'config-dirs)

(defcustom user-roam-dir (expand-file-name "roam" user-org-dir)
  "Directory for OrgRoam"
  :type 'directory
  :group 'config-dirs)

(defgroup config-org nil ""
  :group 'config-init)

(defcustom default-org-agenda-files `(,user-org-dir)
  ""
  :type 'list
  :group 'config-org)

(defconst org-roam-states-not-todo '("LOG" "FUP" "TBR" "RDN"))

(defun org-daily-agenda (arg)
  (interactive "P")
  (org-agenda arg "a"))

(use-package org
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
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t

   ;; Capturing
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-refile-targets '((nil :maxlevel . 3)
                        ("~/jlptech/internal/timetracking.org" :tag . "#work")
                        (org-agenda-files :maxlevel . 3))
   org-capture-templates
   '(("i" "Innovacare Task"
      entry
      (file+olp "~/jlptech/internal/timetracking.org" "Innovacare" "Tasks")
      "* TODO %:subject - %:fromname

%a

%i"
      :prepend t))
   ;; Publishing
   org-publish-project-alist
   '(("vitae"
      :base-directory (expand-file-name "vitae" user-repo-dir)
      :publishing-directory (expand-file-name "vitae/export" user-repo-dir)
      :publishing-function org-latex-publish-to-pdf))

   ;; Tags
   org-use-fast-tag-selection t
   org-tag-persistent-alist
   '(
     ;; Org Roam Tags
     (:startgrouptag)
     ("#" . "#")
     (:grouptags)
     ("#work" . ?W)
     ("#homelab" . ?H)
     ("#FTC". ?F)
     (:endgrouptag))
   ;; Todo Configs
   org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "PEND(n@)" "|" "DONE(d@)")
     (sequence "TODO(t)" "PROG(p)" "BLOCK(b)" "|" "DONE(d@)")
     (type "RISK(r@)" "ISSUE(i@)" "|" "AVRT(a@)")
     (sequence "|" "CANC(c@)")
     ;; Org Roam Sequences
     (sequence "LOG(l)" "FUP(f)" "|" "DONE(d@)")
     (sequence "TBR(T)" "RDN(R)" "|" "READ(D@)"))
   org-modern-todo-faces
   `(("TODO" . (:foreground ,(theme-color 'base1) :background ,(theme-color 'green)
                            :weight bold))
     ("PROG" . (:foreground ,(theme-color 'base1) :background ,(theme-color 'dark-cyan)
                            :weight bold))
     ("PEND" . (:foreground ,(theme-color 'magenta)
                            :weight bold
                            :inverse-video t))
     ("BLOCK" . (:foreground ,(theme-color 'orange) :inverse-video t))
     ("RISK" . (:foreground ,(theme-color 'base2) :background ,(theme-color 'red)
                            :weight bold))
     ("ISSUE" . (:foreground ,(theme-color 'base8) :background ,(theme-color 'red)
                             :weight bold))
     ("AVRT" . (:foreground ,(theme-color 'cyan)
                            :weight bold
                            :inverse-video t))
     ("DONE" . (:foreground ,(theme-color 'base5) :background ,(theme-color 'base0)
                            :weight bold))
     ("CANC" . (:foreground ,(theme-color 'base4) :background ,(theme-color 'base0)
                            :weight bold))
     ;; Org Roam faces
     ("LOG" . (:foreground ,(theme-color 'violet) :background ,(theme-color 'dark-blue)
                           :weight bold))
     ("FUP" . (:foreground ,(theme-color 'magenta) :background ,(theme-color 'dark-blue)
                           :weight bold))
     ("TBR" . (:foreground ,(theme-color 'green) :background ,(theme-color 'base4)
                           :weight :bold))
     ("RDN" . (:foreground ,(theme-color 'yellow) :background ,(theme-color 'base4)
                           :weight bold))
     ("READ" . (:foreground ,(theme-color 'dark-cyan) :background ,(theme-color 'base0)
                            :weight bold)))
   org-todo-keyword-faces org-modern-todo-faces
   ;; `(("TODO" . (:foreground ,(theme-color 'base1) :background ,(theme-color 'green)))
   ;;   ("DEPR" . (:foreground  ,(theme-color 'orange) :weight bold))
   ;;   ("PROG" . (:foreground  ,(theme-color 'dark-blue) :weight bold))
   ;;   ("PEND" . (:foreground  ,(theme-color 'magenta) :weight bold))
   ;;   ("BLOCK" . (:foreground  ,(theme-color 'orange) :inverse-video t))
   ;;   ("RISK" . (:foreground  ,(theme-color 'red) :weight bold))
   ;;   ("ISSUE" . (:foreground  ,(theme-color 'red) :inverse-video t))
   ;;   ("AVRT" . (:foreground  ,(theme-color 'cyan) :weight bold))
   ;;   ("DONE" . (:foreground ,(theme-color 'base5) :background ,(theme-color 'base0)))
   ;;   ;; Org Roam faces
   ;;   ("LOG" . (:foreground ,(theme-color 'violet) :weight bold))
   ;;   ("FUP" . (:foreground ,(theme-color 'magenta) :weight bold)))
   )

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

(use-package org-modern
  :after org
  :config
  (set-face-attribute 'org-modern-date-active nil
                      :foreground (theme-color 'yellow)
                      :background (theme-color 'base2))
  (set-face-attribute 'org-modern-date-inactive nil
                      :foreground (theme-color 'orange)
                      :background (theme-color 'base2))
  (set-face-attribute 'org-modern-time-active nil
                      :foreground (theme-color 'yellow)
                      :background (theme-color 'base2)
                      :inverse-video t)
  (set-face-attribute 'org-modern-time-inactive nil
                      :foreground (theme-color 'orange)
                    :background (theme-color 'base2)
                    :inverse-video t))

;; ** Roam

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t
        org-roam-directory user-roam-dir
        org-roam-db-location (expand-file-name "db/org-roam.db" user-org-dir)
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target
           (file+head+olp "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>
#+filetags: :review:

"
                          ("Log")))))

  :config
  (defun my/org-roam-agenda-all-files (&optional arg keys restriction)
    (interactive)
    (advice-remove 'org-agenda #'my/org-roam-update-org-agenda-files)
    (setq org-agenda-files
          (flatten-tree (list default-org-agenda-files
                              user-roam-dir
                              (expand-file-name "daily" user-roam-dir))))
    (org-agenda arg keys restriction)
    (advice-add 'org-agenda :before #'my/org-roam-update-org-agenda-files))

  (defun my/org-roam--find-todo-files ()
    (let ((nodes (seq-filter (lambda (e)
                               (assoc "HAS_TODO" (org-roam-node-properties e)))
                             (org-roam-node-list))))
      (seq-uniq (seq-map #'org-roam-node-file nodes))))

  (defun my/org-roam--todo-p ()
    (org-element-map
        (org-element-parse-buffer 'headline)
        'headline
      (lambda (h)
        (and (eq (org-element-property :todo-type h)
                 'todo)
             (not (member (org-element-property :todo-keyword h)
                          org-roam-states-not-todo))))
      nil 'first-match))

  (defun my/org-roam-update-org-agenda-files (&optional arg keys restriction)
    (interactive)
    (setq org-agenda-files
          (flatten-tree (list default-org-agenda-files
                              (my/org-roam--find-todo-files)))))

  (defun my/org-roam-update-todo-property ()
    (interactive)
    (when (and (not (active-minibuffer-window))
               (org-roam-file-p))
      (org-with-point-at 1
        (let ((is-todo (my/org-roam--todo-p)))
          (if is-todo
              (org-roam-property-add "HAS_TODO" "t")
            (org-roam-property-remove "HAS_TODO" "t"))))))

  (defun my/org-roam-update-todo-on-save ()
    (add-hook 'before-save-hook #'my/org-roam-update-todo-property nil 't))

  (defun my/org-roam-update-links-on-save ()
    (add-hook 'before-save-hook 'org-roam-link-replace-all nil 't))

  (defun my/org-completion-completers ()
    (mapc (lambda (x)
            (add-to-list 'completion-at-point-functions x))
          org-roam-completion-functions)
    (add-to-list 'completion-at-point-functions #'cape-abbrev))

  (org-roam-db-autosync-mode 1)
  (advice-add 'org-agenda :before #'my/org-roam-update-org-agenda-files)
  :general ("C-<f5>" #'my/org-roam-agenda-all-files)
  :hook ((org-mode . my/org-completion-completers)
         (org-mode . my/org-roam-update-links-on-save)
         (org-mode . my/org-roam-update-todo-on-save)
         (org-mode . my/org-roam-update-todo-property)))

(use-package org-roam-ui
  :defer t)

(use-package consult-org-roam
  :after org-roam
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (consult-notes-org-roam-mode 1)
  (consult-notes-org-headings-mode 1))

;; ** Outlining
(use-package outshine
  :commands outshine-mode
  :config
  :diminish "Outl")

(use-package outorg
  :after org
  :defer t
  :commands (outorg-edit-as-org))

(use-package outline
  :elpaca nil
  :diminish outline-minor-mode)

;; * Provide
(provide 'config-org)
