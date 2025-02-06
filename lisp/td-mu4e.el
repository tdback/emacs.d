;;; -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil
  :bind (("C-c m m" . mu4e)
         ("C-c m c" . 'mu4e-compose-new)
         ("C-c m u" . 'mu4e-update-mail-and-index))
  :config
  (setq mu4e-maildir "~/Mail")

  ;; IMAP options.
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-change-filenames-when-moving t)

  ;; SMTP options.
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/etc/profiles/per-user/tdback/bin/msmtp")

  ;; Compose options.
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq message-kill-buffer-on-exit t)

  ;; Display options.
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-modeline-show-global nil)

  (setq mu4e-contexts
        (list
         ;; Personal account.
         (make-mu4e-context
          :name "Personal"
          :vars '((user-mail-address      . "tyler@tdback.net")
                  (user-full-name         . "Tyler Dunneback")
                  (mu4e-compose-signature . "Tyler Dunneback")
                  (mu4e-drafts-folder     . "/tdback/Drafts")
                  (mu4e-sent-folder       . "/tdback/Sent")
                  (mu4e-refile-folder     . "/tdback/Archive")
                  (mu4e-trash-folder      . "/tdback/Trash")))))

  (setq mu4e-maildir-shortcuts
        '(("/tdback/Inbox"    . ?i)
          ("/tdback/Sent"     . ?s)
          ("/tdback/Trash"    . ?t)
          ("/tdback/Drafts"   . ?d)
          ("/tdback/Archive"  . ?a)
          ("/tdback/All Mail" . ?m))))

(use-package org-mime
  :ensure t
  :config
  (setq org-mime-export-options (list :section-numbers nil
                                      :with-author nil
                                      :with-toc nil))
  (add-hook 'org-mime-html-hook (lambda ()
                                  (org-mime-change-element-style
                                   "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                                                 "#E6E1DC" "#232323"))))
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

(provide 'td-mu4e)
