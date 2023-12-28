;; -*- lexical-binding: t; -*-

;; * Package dependencies

;; Get these installed early since apparently plenty of stuff wants them and
;; gets confused if they aren't present

(elpaca-queue
 (use-package dash)
 (use-package yaml)
 (use-package emacsql)
 (use-package compat
   :demand t)
 (use-package closql
   :defer t)
 (use-package seq)
 (use-package pfuture)
 (use-package hl-todo))

;; * Provide
(provide 'config-dependencies)
