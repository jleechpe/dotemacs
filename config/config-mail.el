;; -*- lexical-binding: t; -*-

(setq my/mu4e-contexts
      '((JLP (make-mu4e-context
        :name "JLP"
        :enter-func (lambda () (mu4e-message "Entering JLP Context"))
        :leave-func (lambda () (mu4e-message "Leaving JLP Context"))
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches
                         msg
                         :to "jonathan@leechpepin.com")))
        :vars '((user-mail-address . "jonathan@leechpepin.com")
                (user-full-name . "Jonathan Leech-Pepin")
                (mu4e-trash-folder . "/jlp/Trash")
                (mu4e-drafs-folder . "/jlp/Drafts")
                (mu4e-sent-folder . "/jlp/Sent")
                (mu4e-refile-folder .
                                    (lambda (msg)
                                      (cond
                                       ;; Fidelity
                                       ((mu4e-message-contact-field-matches
                                         msg
                                         :from ".*@mail.fidelity.com")
                                        "/jlp/Archive/Finances/Stocks")
                                       ;; Webull
                                       ((mu4e-message-contact-field-matches
                                         msg
                                         :from ".*@\\\(email.webull.com\\\|investordelivery.com\\\)")
                                        "/jlp/Archive/Finances/Stocks")
                                       ;; Coinbase
                                       ((mu4e-message-contact-field-matches
                                         msg
                                         :from "no-reply@coinbase.com")
                                        "/jlp/Archive/Finances/Coinbase")
                                       ;; Catchall
                                       (t "/jlp/Archive"))))
                (mu4e-compose-signature .
                                        (concat
                                         "Regards,\n"
                                         "Jon\n")))))
        (Consult (make-mu4e-context
        :name "Consulting"
        :enter-func (lambda () (mu4e-message "Switching to Consulting context"))
        :leave-func (lambda () (mu4e-message "Leaving consulting context"))
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches
                         msg
                         :to "jlp@consultjlp.com")))
        :vars '((user-mail-address . "jlp@consultjlp.com")
                (user-full-name . "Jonathan Leech-Pepin")
                (mu4e-trash-folder . "/consultjlp/Trash")
                (mu4e-drafs-folder . "/consultjlp/Drafts")
                (mu4e-sent-folder . "/consultjlp/Sent")
                (mu4e-compose-signature .
                                        (concat
                                         "Thank you,\n"
                                         "JLP\n")))))))

(elpaca nil
  (use-package mu4e
    :defer t
    :commands (mu4e mu4e-update-index)
    :elpaca nil
    :config
    (defun my/update-mu4e-contexts ()
      ""
      (interactive)
      (setq mu4e-contexts nil)
      (mapc (lambda (context)
              (let ((key (car context))
                    (val (cadr context)))
                (if (or (eq (car context) config-enable-mail)
                        (and (listp config-enable-mail)
                             (member (car context) config-enable-mail))
                        (eq config-enable-mail 'all)
                        (eq config-enable-mail t))
                    (add-to-list 'mu4e-contexts (eval val)))))
            my/mu4e-contexts))
    (my/update-mu4e-contexts)
    (defun my/mu4e-context-dirs (dir)
      (expand-file-name dir
                        (expand-file-name mu4e-context-dir mu4e-maildir)))
    (setq
     mu4e-maildir "~/.mail"
     mu4e-trash-folder "/Trash"
     mu4e-drafts-folder "/Drafts"
     mu4e-sent-folder "/Sent"

     mu4e-use-fancy-chars t
     mu4e-sent-messages-behavior 'sent
     mu4e-change-filenames-when-moving t
     mu4e-completing-read-function 'completing-read

     ;; Sendmail
     sendmail-program "/usr/bin/msmtp"
     message-sendmail-f-is-evil t
     mail-specify-envelope-from t
     mail-envelope-from 'header
     message-sendmail-extra-arguments '("--read-envelope-from")
     send-mail-function 'smtpmail-send-it
     message-send-mail-function 'message-send-mail-with-sendmail
     )

    :hook
    (mu4e-headers-mode . (lambda () (auto-composition-mode 0)))))

(use-package mu4e-alert
  :after mu4e
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (setq doom-modeline-mu4e t))

;; * BBDB
;; (elpaca (bbdb ;:files (:defaults "lisp/*")
;;          :pre-build '(("./autogen.sh")("./configure")("make"))
;;          )
;;   (use-package bbdb
;;   :defer t
;;   :config
;;   (bbdb-initialize 'mu4e 'pgp 'anniv)
;;   ;; Currently does not auto-initialize..
;;   (bbdb-mua-auto-update-init 'mu4e)
;;   (bbdb-insinuate-mu4e)
;;   (setq bbdb-mail-user-agent 'mu4e-user-agent
;;         mu4e-view-mode-hook 'bbdb-mua-auto-update
;;         mu4e~view-buffer-name "*Article*"
;;         bbdb-mua-pop-up t
;;         mu4e-compose-complete-addresses t
;;         mu4e-view-show-addresses t
;;         bbdb-mua-auto-update-p 'query
;;         company-bbdb-modes '(message-mode mu4e-compose-mode))
;;   :hook
;;   (bbdb-notice-record . bbdb-auto-notes)))


;; * Provide
(provide 'config-mail)
