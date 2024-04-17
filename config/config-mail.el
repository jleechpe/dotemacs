;; -*- lexical-binding: t; -*-

;; * Contact Management
(use-package ebdb
  :defer t
  :init
  ;; Set list to nil to avoid undefined variable when trying to load
  (setq ebdb-db-list nil)
  (setq ebdb-sources "~/.syncthing/SecureSync/ebdb")
  (defun my/mu4e-contacts-ebdb-capf ()
    "Combine EBDB with Mu4e contacts"
    (ebdb-records)
    (add-to-list 'completion-at-point-functions
                 (cape-capf-super
                  (cape-capf-properties #'ebdb-mail-dwim-completion-at-point-function
                                        :annotation-function (lambda (_) " EBDB")
                                        :company-kind (lambda (_) 'text)
                                        :exclusive 'no)
                  (cape-capf-properties #'mu4e--compose-complete-contact-field
                                        :annotation-function (lambda (_) " mu4e")
                                        :company-kind (lambda (_) 'event)
                                        :exclusive 'no))))
  :hook
  (mu4e-compose-mode . my/mu4e-contacts-ebdb-capf)
  :requires seq
  :after mu4e)

;; * Mu4e
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
   mu4e-headers-fields '((:maildir . 16)
                         (:human-date . 12)
                         (:flags . 6)
                         (:mailing-list . 5)
                         (:from . 15)
                         (:subject))
   mu4e-maildir "~/.mail"
   mu4e-trash-folder "/Trash"
   mu4e-drafts-folder "/Drafts"
   mu4e-sent-folder "/Sent"

   mu4e-use-fancy-chars t
   mu4e-sent-messages-behavior 'sent
   mu4e-change-filenames-when-moving t
   mu4e-completing-read-function 'completing-read

   mu4e-context-policy 'pick-first

   mu4e-search-hide-predicate
   (lambda (msg)
     (string-match (rx (or "Trash"
                           "Coinbase"))
                   (mu4e-message-field msg :maildir)))

   ;; Sendmail
   sendmail-program (executable-find "msmtp")
   message-sendmail-f-is-evil t
   mail-specify-envelope-from t
   mail-envelope-from 'header
   message-sendmail-extra-arguments '("--read-envelope-from")
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'message-send-mail-with-sendmail)

  :hook
  (mu4e-headers-mode . (lambda () (auto-composition-mode 0))))

(use-package mu4e-alert
  :after mu4e
  :defer t
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)
  (setq doom-modeline-mu4e t))

;; ** Email Contexts
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
                      (mu4e-drafts-folder . "/jlp/Drafts")
                      (mu4e-sent-folder . "/jlp/Sent")
                      (mu4e-refile-folder
                       .
                       (lambda (msg)
                         (let ((subject (mu4e-message-field msg :subject)))
                           (cond
                            ;; FTC related
                            ((or (mu4e-message-contact-field-matches
                                  msg
                                  :from ;; ".*@\\\(servocity.com\\\|revrobotics.com\\\|andymark.com\\\|ftclive.org\\)"
                                  (rx (* nonl) "@"
                                      (or "servocity.com"
                                          "andymark.com"
                                          "ftclive.org"
                                          "revrobotics.com"
                                          "zeffy.com")))
                                 (string-match-p (rx (or "FTC" "Saturn" "9944")) subject))
                             "/jlp/Archive/FTC")
                            ;; Empower
                            ((mu4e-message-contact-field-matches
                              msg
                              :from ".*@sfmc.empowermyretirement.com")
                             "/jlp/Archive/Finances")
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
                            ;; Patreon
                            ((mu4e-message-contact-field-matches
                              msg :from "patreon")
                             "/jlp/Archive/Patreon")
                            ;; Catchall
                            (t "/jlp/Archive")))))
                      (mu4e-compose-signature .
                                              (concat
                                               "Regards,\n"
                                               "Jon\n")))))
        (Consult (make-mu4e-context
                  :name "Consulting"
                  :enter-func
                  (lambda () (mu4e-message "Switching to Consulting context"))
                  :leave-func
                  (lambda () (mu4e-message "Leaving consulting context"))
                  :match-func (lambda (msg)
                                (when msg
                                  (mu4e-message-contact-field-matches
                                   msg
                                   :to "jlp@consultjlp.com")))
                  :vars '((user-mail-address . "jlp@consultjlp.com")
                          (user-full-name . "Jonathan Leech-Pepin")
                          (mu4e-trash-folder . "/consultjlp/Trash")
                          (mu4e-drafts-folder . "/consultjlp/Drafts")
                          (mu4e-sent-folder . "/consultjlp/Sent")
                          (mu4e-compose-signature .
                                                  (concat
                                                   "Thank you,\n"
                                                   "JLP\n"))
                          (mu4e-refile-folder
                           .
                           (lambda (msg)
                             (let ((subject (mu4e-message-field msg :subject)))
                               (cond
                                ;; ICS Timesheet
                                ((and (string-match "Timesheet" subject)
                                      (mu4e-message-contact-field-matches
                                       msg '(:to :from :cc) "innovacare"))
                                 "/consultjlp/Consulting/InnovaCare/Timesheet")
                                ;; ICS
                                ((mu4e-message-contact-field-matches
                                  msg '(:to :from :cc) "innovacare")
                                 "/consultjlp/Consulting/InnovaCare")
                                ;; Catchall
                                ("/consultjlp/Archive"))))))))
        (Aurelius (make-mu4e-context
                   :name "Aurelius"
                   :enter-func
                   (lambda () (mu4e-message "Switching to Aurelius context"))
                   :leave-func
                   (lambda () (mu4e-message "Leaving Aurelius context"))
                   :match-func (lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches
                                    msg
                                    :to "jlp@aureliusmind.ai")))
                   :vars '((user-mail-address . "jlp@aureliusmind.ai")
                           (user-full-name . "Jonathan Leech-Pepin")
                           (mu4e-trash-folder . "/jlpaurelius/[Gmail]/Trash")
                           (mu4e-sent-folter . "jlpaurelius/[Gmail]/Sent Mail")
                           (mu4e-compose-signature .
                                                   (concat
                                                    "Thank you,\n"
                                                    "JLP\n"))
                           (mu4e-refile-folder
                            .
                            (lambda (msg)
                              (let ((subject (mu4e-message-field msg :subject)))
                                (cond
                                 ("/jlpaurelius/[Gmail]/Archive"))))))))))

;; * Provide
(provide 'config-mail)
