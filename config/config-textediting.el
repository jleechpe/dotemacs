;; -*- lexical-binding: t; -*-

(elpaca-use-package flyspell-correct
  :general
  ([remap flyspell-auto-correct-word] #'flyspell-correct-wrapper))

(elpaca-use-package markdown-mode
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . flycheck-mode)))

;; * Provides
(provide 'config-textediting)
