;; -*- lexical-binding: t; -*-

;; ** Server
;; Start server to ensure it's running

(elpaca nil
         (use-package server
           :config
           (unless (server-running-p)
             (server-start))))

;; * Edit Server
;; Allow edit-with-emacs to work
(elpaca-use-package edit-server
                    :commands edit-server-start
                    :init (if after-init-time
                              (edit-server-start)
                            (add-hook 'after-init-hook
                                      #'(lambda() (edit-server-start))))
                    :config (setq edit-server-new-frame-alist
                                  '((name . "Edit with Emacs FRAME")
                                    (top . 200)
                                    (left . 200)
                                    (width . 80)
                                    (height . 25)
                                    (minibuffer . 5)
                                    (menu-bar-lines . t))))
;; * Provide
(provide 'config-server)
