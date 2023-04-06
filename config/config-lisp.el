;; -*- lexical-binding: t; -*-

(use-package sly
                    :defer t
                    :commands (sly sly-connect)
                    :config
                    (setq inferior-lisp-program "sbcl"))
;; * Provide
(provide 'config-lisp)
