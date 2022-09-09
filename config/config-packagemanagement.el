;; -*- lexical-binding: t; -*-
;; * Package configuration management
(elpaca use-package
  (setq use-package-verbose t
        use-package-compute-statistics t)
  (require 'use-package))

(elpaca-use-package general)

;; ** Benchmark Init File
;; Loading use-package and general first are low cost
(elpaca benchmark-init
  (require 'benchmark-init)
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; * Provide
(provide 'config-packagemanagement)
