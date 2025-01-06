;; -*- lexical-binding: t; -*-

;; *** Custom Vars
(defcustom user-org-dir "~/org" "Default directory for org files"
  :type 'directory
  :group 'config-dirs)

(defcustom user-roam-dir (expand-file-name "roam" user-org-dir)
  "Directory for OrgRoam"
  :type 'directory
  :group 'config-dirs)

(defcustom user-roam-daily-dir (expand-file-name "daily" user-roam-dir)
  "Directory for Daily files in OrgRoam"
  :type 'directory
  :group 'config-dirs)

(defgroup config-org nil ""
  :group 'config-init)

(defcustom default-org-agenda-files `(,user-org-dir ,user-roam-dir ,user-roam-daily-dir)
  ""
  :type 'list
  :group 'config-org)

(defconst org-roam-states-not-todo '("LOG" "FUP" "TBR" "RDN"))

;; *** Custom Functions
(defun org-daily-agenda (arg)
  (interactive "P")
  (org-agenda arg "a"))

;; *** Org-Ql
(defun my/setup-ql-views ()
  (mapc (lambda (view)
          (add-to-list 'org-ql-views view))
        org-ql--my-views))

(use-package org-ql
  :after (org)
  :ensure t
  :defer t
  :init
  ;; Setup org-ql filtering variables so views and agendas can use them
  (setq
   ;; Filters
   org-ql-todo--roam-log '(todo "LOG" "FUP")
   org-ql-todo--reading-list '(todo "TBR" "RDN")
   org-ql-todo--excl-testing '(not (category "testing"))

   ;; Queries
   org-ql-query--roam
   `(and ,org-ql-todo--roam-log
         (tags "review"))
   org-ql-query--habit-stats
   '(and (habit)
         ;; (not (scheduled :to today))
         )
   org-ql-query--habits
   '(and (habit)
         (scheduled :to today))
   org-ql-query--reading-list
   `(and ,org-ql-todo--reading-list
         ,org-ql-todo--excl-testing)
   org-ql-query--todos
   `(and (todo)
         (not ,org-ql-todo--roam-log)
         ,org-ql-todo--excl-testing
         (not (habit))
         (not ,org-ql-todo--reading-list))
   ;; Views
   org-ql--my-views `(("Roam: Review"
                       :buffers-files org-agenda-files
                       :query ,org-ql-query--roam
                       :sort (date priority)
                       :title "Roam Review"
                       :super-groups ((:take (5 (:tag "review")))))
                      ("Habit: status"
                       :buffers-files org-agenda-files
                       :query ,org-ql-query--habit-stats
                       :sort (date priority)
                       :title "Habit status"
                       :super-groups ((:name "Overdue" :scheduled past)
                                      (:name "Current" :scheduled today)
                                      (:name "Up-to-date" :scheduled future)))
                      ("Reading List"
                       :buffers-files org-agenda-files
                       :query ,org-ql-query--reading-list
                       :title "Reading List"
                       :super-groups org-super-agenda-groups))))

;; *** Org-Mode Setup
(use-package doct
  :defer t
  :ensure t)
(use-package org
  :defer t
  :hook
  ((org-mode . auto-fill-mode)
   ;; (org-mode . flyspell-mode)
   (org-after-todo-statistics . my/org-statistics-update)
   (org-checkbox-statistics . my/org-checkbox-statistics-update))
  :config
  (push 'org-self-insert-command completion-preview-commands)
  (add-to-list 'org-modules 'org-habit)

  (setq org-agenda-custom-commands
        `(("r" "Roam"
           ((org-ql-block (quote ,org-ql-query--roam)
                          ((org-agenda-max-entries 5)))))
          ("H" "Habit status"
           ((org-ql-block (quote ,org-ql-query--habit-stats))))
          ("h" "Habits"
           ((org-ql-block (quote ,org-ql-query--habits))))
          ("t" "Todo test"
           ((org-ql-block (quote ,org-ql-query--todos))))
          ("A" "Test blocks"
           ((org-ql-block (quote ,org-ql-query--todos)
                          (;; (org-agenda-max-entries 5)
                           (org-ql-block-header "TODOs")))
            (org-ql-block (quote ,org-ql-query--roam)
                          (;; (org-agenda-max-entries 5)
                           (org-ql-block-header "Roam Review")))
            ;; (org-ql-block (quote ,org-ql-query--habits)
            ;;               ((org-ql-block-header "Habits")))
            (org-ql-block (quote ,org-ql-query--habit-stats)
                          ((org-ql-block-header "Habits")))
            (org-ql-block (quote ,org-ql-query--reading-list)
                          ((org-ql-block-header "Reading List")))))))

  (setq
   ;; Overall Org
   org-startup-indented t
   org-agenda-compact-blocks t
   org-log-into-drawer t
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies t
   org-list-allow-alphabetical t
   org-agenda-include-diary nil
   org-agenda-span 'day

   ;; Clocks and timestamps
   org-clock-into-drawer t
   org-clock-rounding-minutes 15

   ;; Habits
   org-habit-show-habits-only-for-today nil

   ;; Capturing
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-refile-targets '((nil :maxlevel . 3)
                        ("~/jlptech/internal/timetracking.org" :tag . "#work")
                        (org-agenda-files :maxlevel . 3))
   org-capture-templates--email
   `((:group "Mail Task"
             :template "* TODO %:subject - %:fromname\n\n%a\n\n%i"
             :contexts ((:in-mode "mu4e-view-mode")
                        (:in-mode "mu4e-headers-mode"))
             :children (("Innovacare"
                         :keys "i"
                         :file "~/jlptech/internal/timetracking.org"
                         :type entry
                         :olp ("Innovacare" "Tasks")
                         :prepend t))))
   org-capture-templates--health
   `(("Health"
      :keys "H"
      :file "~/org/roam/20240610085623-health.org"
      :type table-line
      :immediate-finish t
      :children (("Weight"
                  :keys "w"
                  :olp ("Weight")
                  :template "| %^u | %^{Weight} |"
                  )
                 ("Exercise"
                  :keys "e"
                  :olp ("Exercise")
                  :template "| %^u | %^{Type} | %^{Amount}"))))
   org-capture-templates
   '(("P" "3d Printing related")
     ("PQ" "Qidi X Max 3")
     ("PQs" "New Spool"
      entry
      (file+olp "~/org/printing.org" "Qidi XMax 3" "Filaments")
      "* %^{Name}
#+name: %\\1
| | Print | Weight| Cost |
|-+-------+-------|-|
| |  | | |
|-+-------+-------|-|
|#| Total |       | |
|^|       | tot | |
|$| start=%^{Weight} | | |
|$| color=%^{Color} | | |
|$| type=%^{Type} | | |
|$| cost=%^{Cost} | | |
#+TBLFM: $tot=vsum(@I..@II)::@2$4..@3$4=($cost/$start)*$-1;%.2f
"
      :prepare-finalize (lambda () (org-store-link 'nil 't))
      :after-finalize (lambda () (org-capture 'nil "PQS"))
      :immediate-finish t)
     ("PQS" "Summary Spool"
      table-line
      (file+olp "~/org/printing.org" "Qidi XMax 3" "Filament Amount")
      "| %(substring-no-properties (cadar org-stored-links)) | | | | | |"
      :immediate-finish t))
   org-capture-templates-contexts
   '(("i" ((in-mode . "mu4e-view-mode")
           (in-mode . "mu4e-headers-mode"))))
   org-capture-templates (doct-add-to org-capture-templates
                                      org-capture-templates--email)
   org-capture-templates (doct-add-to org-capture-templates
                                      org-capture-templates--health)
   ;; Publishing
   org-publish-project-alist
   `(("vitae"
      :base-directory ,(expand-file-name "vitae" user-repo-dir)
      :publishing-directory ,(expand-file-name "vitae/export" user-repo-dir)
      :publishing-function org-latex-publish-to-pdf))

   ;; Tags
   org-use-fast-tag-selection t
   org-tag-persistent-alist
   '(
     ;; Org Roam Tags
     (:startgroup)
     ("Context" . "Context")
     (:grouptags)
     ("#work" . ?W)
     ("#homelab" . ?H)
     ("#FTC". ?F)
     ("#aurelius" . ?A)
     ("#config" . ?C)
     ("#dev" . ?D)
     (:endgroup)
     (:startgroup)
     ("ConfigType" . "Config Type")
     (:grouptags)
     ("%theme" . ?t)
     ("%workflow" . ?w)
     ("%binding" . ?b)
     (:endgroup)
     )
   ;; Todo Configs
   org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "PEND(n@)" "|" "DONE(d@)")
     (sequence "TODO(t)" "PROG(p)" "BLOCK(b)" "|" "DONE(d@)")
     (type "RISK(r@)" "ISSUE(i@)" "|" "AVRT(a@)")
     (sequence "|" "CANC(c@)")
     (sequence "PRCH(P)" "|" "BGHT(B)")
     ;; Org Roam Sequences
     (sequence "LOG(l)" "FUP(f)" "|" "LOGD(L@)")
     (sequence "TBR(T)" "RDN(R)" "|" "READ(D@)"))
   org-modern-todo-faces
   `(("TODO" . (:foreground ,(doom-color 'base1) :background ,(doom-color 'green)
                            :weight bold))
     ("PROG" . (:foreground ,(doom-color 'base1) :background ,(doom-color 'dark-cyan)
                            :weight bold))
     ("PEND" . (:foreground ,(doom-color 'magenta)
                            :weight bold
                            :inverse-video t))
     ("BLOCK" . (:foreground ,(doom-color 'orange) :inverse-video t))
     ("RISK" . (:foreground ,(doom-color 'base2) :background ,(doom-color 'red)
                            :weight bold))
     ("ISSUE" . (:foreground ,(doom-color 'base8) :background ,(doom-color 'red)
                             :weight bold))
     ("AVRT" . (:foreground ,(doom-color 'cyan)
                            :weight bold
                            :inverse-video t))
     ("DONE" . (:foreground ,(doom-color 'base5) :background ,(doom-color 'base0)
                            :weight bold))
     ("CANC" . (:foreground ,(doom-color 'base4) :background ,(doom-color 'base0)
                            :weight bold))
     ;; Org Roam faces
     ("LOG" . (:foreground ,(doom-color 'violet) :background ,(doom-color 'dark-blue)
                           :weight bold))
     ("FUP" . (:foreground ,(doom-color 'magenta) :background ,(doom-color 'dark-blue)
                           :weight bold))
     ("TBR" . (:foreground ,(doom-color 'green) :background ,(doom-color 'base4)
                           :weight :bold))
     ("RDN" . (:foreground ,(doom-color 'yellow) :background ,(doom-color 'base4)
                           :weight bold))
     ("READ" . (:foreground ,(doom-color 'dark-cyan) :background ,(doom-color 'base0)
                            :weight bold)))
   org-todo-keyword-faces org-modern-todo-faces
   ;; `(("TODO" . (:foreground ,(doom-color 'base1) :background ,(doom-color 'green)))
   ;;   ("DEPR" . (:foreground  ,(doom-color 'orange) :weight bold))
   ;;   ("PROG" . (:foreground  ,(doom-color 'dark-blue) :weight bold))
   ;;   ("PEND" . (:foreground  ,(doom-color 'magenta) :weight bold))
   ;;   ("BLOCK" . (:foreground  ,(doom-color 'orange) :inverse-video t))
   ;;   ("RISK" . (:foreground  ,(doom-color 'red) :weight bold))
   ;;   ("ISSUE" . (:foreground  ,(doom-color 'red) :inverse-video t))
   ;;   ("AVRT" . (:foreground  ,(doom-color 'cyan) :weight bold))
   ;;   ("DONE" . (:foreground ,(doom-color 'base5) :background ,(doom-color 'base0)))
   ;;   ;; Org Roam faces
   ;;   ("LOG" . (:foreground ,(doom-color 'violet) :weight bold))
   ;;   ("FUP" . (:foreground ,(doom-color 'magenta) :weight bold)))
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
  ("S-<f5>" #'org-agenda)
  ("<f6>" #'org-capture))

;; *** Org-Agenda-Files-track-ql
(use-package org-agenda-files-track-ql
  :ensure (:host github :repo "jleechpe/org-agenda-files-track")
  :defer t
  :after (org-ql org-roam)
  :init
  ;; Setup org agenda files to account for sync changes
  (if (file-directory-p "~/jlptech/internal")
      (add-to-list 'default-org-agenda-files "~/jlptech/internal"))
  (defun my/org-agenda-files-track-init ()
    "(Re)initialize dynamic agenda files.

This can take a long time, so it is recommended to run this only
on installation and when first tasks are added to many files via
methods the save hook cannot detect, like file synchronization."
    (interactive)
    (require 'org-agenda-files-track-ql)
    (setq org-agenda-files default-org-agenda-files)
    (org-agenda-files-track-ql-cleanup-files 'full)
    (message "Initialized org agenda files"))
  ;; :config
  (org-agenda-files-track-ql-mode t)
  ;; Run agenda files hook on startup since syncthing might make out of band
  ;; updates (until I share custom as part of repo)
  ;; :hook (elpaca-after-init . my/org-agenda-files-track-init)
  )

;; *** Org-Modern
(use-package org-modern
  :after org
  :defer t
  :config
  (set-face-attribute 'org-modern-date-active nil
                      :foreground (doom-color 'yellow)
                      :background (doom-color 'base2))
  (set-face-attribute 'org-modern-date-inactive nil
                      :foreground (doom-color 'orange)
                      :background (doom-color 'base2))
  (set-face-attribute 'org-modern-time-active nil
                      :foreground (doom-color 'yellow)
                      :background (doom-color 'base2)
                      :inverse-video t)
  (set-face-attribute 'org-modern-time-inactive nil
                      :foreground (doom-color 'orange)
                      :background (doom-color 'base2)
                      :inverse-video t))

;; *** Org-Super-Agenda
(use-package org-super-agenda
  :after (org)
  :defer t
  :init
  (add-to-list 'warning-suppress-types '(org-element))
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        `((:name ""
                 :and (:todo ("FUP")
                             :tag "review"))
          (:name "Logs"
                 :and (:todo "LOG"
                             :tag "review"))
          (:name ""
                 :todo "RDN")
          (:name "To read"
                 :todo "TBR")
          (:name "Overdue Habits"
                 :and (:habit t
                              :scheduled past))
          (:name "Due Habits"
                 :and (:habit t
                              :scheduled today))
          (:name "Up-to-date Habits"
                 :and (:habit t
                              :scheduled future))

          ;; Keep this last so timegrid doesn't steal
          (:time-grid t
                      :order -1))))

;; ** Roam
;; *** Org-Roam
(use-package org-roam
  :ensure t
  :defer t
  :after org
  :init
  (setq org-roam-org-mode-map (make-sparse-keymap "Org-Roam"))
  (defun my/org-roam-node-annotate (node)
    (let* ((id (org-roam-node-id node))
           (title (org-roam-node-title node))
           (aliases (org-roam-node-aliases node))
           (tags (org-roam-node-tags node))
           (blinks (length (org-roam-db-query
                            [:select (funcall count source)
                                     :from links
                                     :where (= dest $s1)
                                     :and (= type "id")]
                            id)))
           (flinks (length (org-roam-db-query
                            [:select dest
                                     :from links
                                     :where (= source $s1)
                                     :and (= type "id")
                                     :group-by dest]
                            id)))
           (file (org-roam-node-file node))
           (dir (file-name-nondirectory (directory-file-name
                                         (file-name-directory file)))))
      (concat (propertize " " 'display `(space :align-to center))
              title " " (format "%S" aliases) " "
              (format "B:%s" blinks)
              " "
              (format "F:%s" flinks)
              " "
              (s-truncate 8 (format "%s" dir) "..."))))


  (setq org-roam-v2-ack t
        org-roam-directory user-roam-dir
        org-roam-db-location (expand-file-name "db/org-roam.db" user-org-dir)
        org-roam-node-annotation-function #'my/org-roam-node-annotate
        org-roam-completion-everywhere t
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %c"
           :target
           (file+head+olp "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>
#+filetags: :log:review:\n\n* Log"
                          ("Log"))
           :empty-lines 3)))

  (defun my/org-completion-completers ()
    (add-to-list 'completion-at-point-functions #'cape-abbrev))
  :config

  (org-roam-db-autosync-mode 1)

  (defun my/org-roam-capture-daily (date)
    (interactive "P")
    (message "%S" date)
    (cond
     ((equal '(16) date)
      (org-roam-dailies-goto-date))
     ((equal '(4) date)
      (org-roam-dailies-goto-yesterday 1))
     ((eq '- date)
      (org-roam-dailies-goto-tomorrow 1))
     (date
      (org-roam-dailies-goto-yesterday date))
     ((org-roam-dailies-goto-today))))
  :general
  ("C-<f5>" #'my/org-roam-agenda-all-files
   "C-<f6>" #'my/org-roam-capture-daily)
  (:keymaps 'org-mode-map
            "C-c r" org-roam-org-mode-map)
  (:keymaps 'org-roam-org-mode-map
            "t" #'org-roam-buffer-toggle
            "d" #'org-roam-buffer-display-dedicated)
  :hook ((org-mode . my/org-completion-completers)))

;; *** Org-Roam-UI
(use-package org-roam-ui
  :defer t
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil)
  :general
  (:keymaps 'org-roam-org-mode-map
            "u" #'org-roam-ui-open))

;; *** Consult-Org-Roam
(use-package consult-org-roam
  :after (org-roam)
  :defer t
  :init
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :general
  (:keymaps 'org-roam-org-mode-map
            "b" #'consult-org-roam-backlinks
            "B" #'consult-org-roam-backlinks-recursive
            "f" #'consult-org-roam-forward-links
            "g" #'consult-org-roam-search ; mirror `C-x p g' from project
            ))

;; ** Consult-Notes
(use-package consult-notes
  :defer t)

(use-package consult-notes-org-roam
  :disabled t)

;; ** Outlining
(use-package outshine
  :commands outshine-mode
  :config
  :defer t
  :diminish "Outl")

(use-package outorg
  :after org
  :defer t
  :commands (outorg-edit-as-org))

(use-package outline
  :ensure nil
  :defer t
  :diminish outline-minor-mode)

;; ** Calendar sync
(use-package khalel
  :after org
  :defer t
  :disabled t
  :autoload khalel-import-events
  :init
  (setq khalel-capture-key "e"
        khalel-import-org-file (expand-file-name "khal.org" org-directory)
        khalel-import-start-date "-5d"
        khalel-import-end-date "+30d"
        khalel-import-org-file-confirm-overwrite nil)
  (run-at-time 0 300 #'khalel-import-events)
  :config
  (khalel-add-capture-template))

;; ** Postman requests
(use-package verb
  :after org
  :defer t
  :general
  (:keymaps 'org-mode-map
            "C-c C-r" verb-command-map))

;; * Provide
(provide 'config-org)
