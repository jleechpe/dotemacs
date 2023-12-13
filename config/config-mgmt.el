;; -*- lexical-binding: t; -*- }
(use-package chezmoi
  :init
  ;; Variables
  (setq-default chezmoi-template-display-p t)
  ;; Custom functions
  (defun chezmoi-open-other-in-window (file)
    (interactive (list (buffer-file-name)))
    (if (chezmoi-target-file-p file)
        (chezmoi-find-other-window file)
      (display-buffer (thread-first file
                                    chezmoi-target-file
                                    find-file-noselect))))

  (defun chezmoi-find-other-window (file)
    (let ((source-file (chezmoi-source-file file)))
      (when source-file
        (display-buffer (find-file-noselect source-file))
        (let ((target-file (chezmoi-target-file source-file)))
	  (when-let ((mode (thread-first target-file
				         file-name-nondirectory
				         (assoc-default auto-mode-alist 'string-match))))
	    (funcall mode))
	  (message target-file)
	  (unless chezmoi-mode (chezmoi-mode))
	  source-file))))

  (defun chezmoi--load-other ()
    (if (eq this-command 'chezmoi-find)
        (chezmoi-open-other-in-window (buffer-file-name))))

  (defun chezmoi--is-target ()
    (message "%S" this-command)
    (if (and (not (eq this-command 'chezmoi-find))
             (chezmoi-target-file-p (buffer-file-name))
             (y-or-n-p "File is managed by Chezmoi.  Visit chezmoi copy?"))
        (let ((this-command 'nil)
              (curr (buffer-name)))
          (chezmoi-find (buffer-file-name))
          (display-buffer curr))))
  :autoload chezmoi-target-file-p
  chezmoi-target-file
  :general ("C-c C f" #'chezmoi-find
            "C-c C s" #'chezmoi-write)
  :hook ((find-file . chezmoi--is-target)
         (chezmoi-mode . chezmoi--load-other)))


;; * Provide
(provide 'config-mgmt)
