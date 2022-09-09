;; -*- lexical-binding: t; -*-

;; * Version Control
;; ** Git
(elpaca-use-package magit
  :commands magit-status
  :general
  ("<f12>" #'magit-status))

(elpaca-use-package transient
  :init
  (setq transient-levels-file (expand-file-name "transients/levels.el" user-cache-dir)
        transient-values-file (expand-file-name "transients/values.el" user-cache-dir)
        transient-history-file (expand-file-name "transients/history.el" user-cache-dir)))

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
