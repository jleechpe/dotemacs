;; -*- lexical-binding: t; -*-

;; * General Completion

;; Enhance default completion framework for minibuffers and any other
;; expansion systems.
(elpaca (vertico :files(:defaults "extensions/*"))
  (use-package vertico)
  (use-package vertico-directory
    :after vertico
    :elpaca nil
    :general
    (:keymaps 'vertico-map
              "\r" #'vertico-directory-enter
              "\d" #'vertico-directory-delete-char
              "M-\d" #'vertico-directory-delete-word))
  (vertico-mode))

(elpaca orderless
  (setq completion-styles '(orderless)))

(use-package marginalia
  :config (marginalia-mode 1)
  :general
  (minibuffer-mode-map "s-a" #'marginalia-cycle))

(use-package consult
  :config
  (setq consult-narrow-key "<"
        consult-preview-key '(:debounce 0.2 any))
  :general
  ([remap switch-to-buffer] #'consult-buffer)
  ([remap project-switch-to-buffer] #'consult-project-buffer)
  ([remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  ([remap repeat-complex-command] #'consult-complex-command)
  ([remap goto-line] #'consult-goto-line)
  ("M-g M-g" #'consult-line)
  ([remap apropos-command] #'consult-apropos)
  ([remap yank-pop] #'consult-yank-pop)
  ([remap pop-global-mark] #'consult-global-mark)
  ("C-c k" #'consult-macro)
  ("M-g o" #'consult-outline)
  ("M-g i" #'consult-project-imenu)
  ("M-g M-i" #'consult-imenu)
  ("s-s" #'consult-isearch-history)
  (:keymaps 'project-prefix-map
            [remap project-find-regexp] #'consult-ripgrep))

(use-package embark
  :defer t
  :general ("s-e" #'embark-act))

(use-package embark-consult
  :after (consult embark))

(elpaca (corfu :files (:defaults "extensions/*"))
  (use-package corfu
    :elpaca nil
    :general
    ("M-<tab>" #'complete-symbol)
    ("M-/" #'completion-at-point)
    :init
    (global-corfu-mode)
    (setq corfu-auto t
          corfu-auto-delay 0.1
          corfu-preselect-first t
          corfu-preview-current t
          corfu-cycle t
          corfu-quit-at-boundary nil
          corfu-quit-no-match t
          corfu-scroll-margin 2))
  (global-corfu-mode))

;; (use-package cape
;;   :disabled t
;;   :config
;;   (defun my/tabnine-capf ()
;;     (interactive)
;;     (let ((completion-at-point-functions
;;            (list (cape-company-to-capf #'company-tabnine))))
;;       (completion-at-point)))
;;   :demand t
;;   :commands cape-abbrev
;;   :general
;;   ("M-<tab>" #'my/tabnine-capf)
;;   )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; *** Tabnine support

;; ;; Company-tabnine, requires company
;; (elpaca company)
;; (use-package company-tabnine
;;   :defer t
;;   :after (company corfu))

(elpaca (tabnine :repo "https://github.com/shuxiao9058/tabnine"
                 :refs nil
                 :files (:defaults))
  (use-package tabnine
    :elpaca nil
    :commands (tabnine-start-process)
    :hook (prog-mode . tabnine-mode)
    :diminish "⌬"
    :custom
    (tabnine-wait 1)
    (tabnine-idle-delay 0.5)
    (tabnine-minimum-prefix-length 3)
    :hook ((on-first-input . tabnine-start-process)
           (kill-emacs . tabnine-kill-process))
    :config
    (add-to-list 'completion-at-point-functions
                 #'tabnine-completion-at-point t)
    (add-to-list 'kind-icon-mapping '(tabnine "ai" :icon "cloud" :face shadow) t)
    :general
    (:keymaps 'tabnine-completion-map
              "<tab>" #'tabnine-accept-completion
              "M-<tab>" #'tabnine-accept-completion
              "M-f" #'tabnine-accept-completion-by-word
              "M-<return>" #'tabnine-accept-completion-by-line
              "C-g" #'tabnine-clear-overlay
              "M-[" #'tabnine-previous-completion
              "M-]" #'tabnine-next-completion)))

;; ** Snippets
(use-package yasnippet
  :init
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-dropdown-prompt
                               yas-no-prompt)
        yas-snippet-dirs (list
                          (expand-file-name "snippets" user-cache-dir)))
  :hook (lsp-mode . yas-minor-mode))

;; * Provide
(provide 'config-completion)
