;; -*- lexical-binding: t; -*-

;; * Text Editing
;; ** Formatting
;; This also includes auto-format for non-text modes since the
;; formatting is done to the text
(use-package apheleia
  :config
  (setf (alist-get 'isort apheleia-formatters)
      '("isort" "--profile" "black" "--stdout" "-"))
  (setf (alist-get 'python-base-mode apheleia-mode-alist)
        '(isort black))
  (apheleia-global-mode 1))

(use-package ws-butler
  :config (ws-butler-global-mode 1))

;; ** Spell Check
(use-package flyspell-correct
  :general
  ([remap flyspell-auto-correct-word] #'flyspell-correct-wrapper))

(elpaca (flymake-vale :repo "https://github.com/tpeacock19/flymake-vale"
                      :refs nil
                      :files (:defaults))
  (use-package flymake-vale
    :elpaca nil
    :config
    (add-to-list 'flymake-vale-modes 'python-ts-mode)
    (add-to-list 'flymake-vale-modes 'python-mode)
    :hook ((find-file . flymake-vale-maybe-load)
           (eglot-managed-mode . flymake-vale-maybe-load))
  ))

;; ** Markdown
(use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . flymake-mode)))

;; * Provides
(provide 'config-textediting)
