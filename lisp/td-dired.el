;;; td-dired.el --- dired configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook ((dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map (("C-x C-j" . dired-jump)
                              ("b"       . dired-up-directory)))
  :config
  (setq dired-listing-switches "-Agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-detailes-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'td-dired)
;;; td-dired.el ends here
