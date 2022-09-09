;; -*- lexical-binding: t; -*-

;; * Version Control
;; ** Git
(elpaca-use-package magit
  :commands magit-status
  :general
  ("<f12>" #'magit-status))

(elpaca nil
  (use-package treemacs-magit
    :after (treemacs magit)))

(elpaca-queue
 (elpaca-use-package closql
   :defer t)
 (elpaca-use-package forge
   :defer t))

;; ** Fossil
(elpaca vc-fossil)

;; * Provide
(provide 'config-vc)
