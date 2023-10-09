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
   :defer t))

;; * Provide
(provide 'config-dependencies)
