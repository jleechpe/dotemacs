;; -*- lexical-binding: t; -*-

;; * Text Editing
;; ** Formatting
;; This also includes auto-format for non-text modes since the
;; formatting is done to the text
(elpaca-use-package apheleia
  :config
  (apheleia-global-mode 1))

;; ** Spell Check
(elpaca-use-package flyspell-correct
  :general
  ([remap flyspell-auto-correct-word] #'flyspell-correct-wrapper))

(elpaca-use-package (flycheck-vale
                     :host github
                     :repo "jleechpe/flycheck-vale")
  :config
  (flycheck-vale-setup))

;; ** Markdown
(elpaca-use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . flycheck-mode)))

;; * Provides
(provide 'config-textediting)
