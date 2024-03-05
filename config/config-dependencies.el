;; -*- lexical-binding: t; -*-

;; * Package dependencies

;; Get these installed early since apparently plenty of stuff wants them and
;; gets confused if they aren't present
;; Fix seq version (https://github.com/progfolio/elpaca/issues/216)
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package))) ;
(use-package seq
  :preface (unload-feature 'seq t)
  :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

;; (elpaca-queue
;;  (use-package dash)
;;  (use-package yaml)
;;  (use-package emacsql)
;;  (use-package compat
;;    :demand t)
;;  (use-package closql
;;    :defer t)
;;  (use-package jsonrpc)
;;  (use-package pfuture)
;;  (use-package hl-todo))

;; * Provide
(provide 'config-dependencies)
