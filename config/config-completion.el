;; -*- lexical-binding: t; -*-

;; * General Completion

;; Enhance default completion framework for minibuffers and any other
;; expansion systems.  
(elpaca-use-package (vertico :files (:defaults "extensions/*"))
  :init
  (vertico-mode 1))
(elpaca nil (use-package vertico-directory
              :after vertico
              :general
              (:keymaps 'vertico-map
                        "\r" #'vertico-directory-enter
                        "\d" #'vertico-directory-delete-char
                        "M-\d" #'vertico-directory-delete-word)))


(elpaca orderless
  (setq completion-styles '(orderless)))

(elpaca-use-package marginalia
  :config (marginalia-mode 1)
  :general
  (minibuffer-mode-map "s-a" #'marginalia-cycle))

(elpaca-use-package consult
  :config
  (setq consult-project-root-function #'projectile-project-root
        consult-narrow-key "<"
        consult-preview-key '(:debounce 0.2 any))
  :general
  ([remap switch-to-buffer] #'consult-buffer)
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
  ("s-s" #'consult-isearch-history))

(elpaca-use-package consult-projectile
  :commands consult-projectile
  :general
  ("C-x f" #'consult-projectile))

(elpaca-use-package embark
  :defer t
  :general ("s-e" #'embark-act))

(elpaca-use-package embark-consult
  :after (consult embark))

(elpaca-use-package corfu
  :init (global-corfu-mode)
  :general
  ("M-<tab>" #'complete-symbol)
  ("M-/" #'completion-at-point)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-preselect-first t
        corfu-preview-current t
        corfu-cycle t
        corfu-quit-at-boundary nil
        corfu-quit-no-match t
        corfu-scroll-margin 2))

(elpaca-use-package cape
  :config
  (defun my/tabnine-capf ()
    (interactive)
    (let ((completion-at-point-functions
           (list (cape-company-to-capf #'company-tabnine))))
      (completion-at-point)))
  
  :general
  ("M-<tab>" #'my/tabnine-capf)
  )

(elpaca-use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; *** Tabnine support
(elpaca company)
(elpaca-use-package company-tabnine
  :defer t
  :after (company corfu))

;; ** Snippets
(elpaca-use-package yasnippet
  :init
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-dropdown-prompt
                               yas-no-prompt)
        yas-snippet-dirs (list
                          (expand-file-name "snippets" user-cache-dir)))
  :hook (lsp-mode . yas-minor-mode))

;; * Provide
(provide 'config-completion)
