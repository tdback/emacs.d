;;; td-theme.el --- emacs appearance -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

(defun td/ef-toggle-theme ()
  "Toggles the current ef-theme. If a ef-theme isn't currently
enabled, set the ef-theme to the dark variant."
  (interactive)
  (let ((dark-theme    'ef-dream)
        (light-theme   'ef-arbutus)
        (current-theme (car custom-enabled-themes)))
    ;; Disable all active themes before setting a new one.
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (cond
     ((eql current-theme light-theme) (load-theme dark-theme t))
     ((eql current-theme dark-theme)  (load-theme light-theme t))
     (t (load-theme dark-theme t)))))

(defun td/set-font ()
  (when (display-graphic-p)
    (message "Setting font...")
    (set-face-attribute 'default nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)
    (set-face-attribute 'fixed-pitch nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)
    (set-face-attribute 'variable-pitch nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)))

;;; Packages

(use-package ef-themes
  :ensure t
  :init
  (load-theme 'ef-dream t))

(use-package emacs
  :ensure nil
  :hook ((emacs-startup-hook . (lambda ()
                                 (message "emacs loaded in %s with %d garbage collections."
                                          (format "%.2f seconds"
                                                  (float-time
                                                   (time-subtract after-init-time before-init-time)))
                                          gcs-done))))
  :config
  ;; Fix font issues when opening a client window.
  (if (daemonp)
      (add-hook 'after-make-frame-functions (lambda (frame)
                                              (with-selected-frame frame
                                                (td/set-font))))
    (td/set-font))

  ;; Disable line numbers for some modes.
  (dolist (mode '(eat-mode-hook
                  eshell-mode-hook
                  dired-mode-hook
                  olivetti-mode-hook
                  org-mode-hook
                  shell-mode-hook
                  term-mode-hook
                  vterm-mode-hook))
    (add-hook mode (lambda ()
                     (display-line-numbers-mode 0))))

  ;; Prettify the lambda symbol.
  (dolist (mode '(emacs-lisp-mode-hook
                  lisp-mode-hook
                  scheme-mode-hook))
    (add-hook mode 'prettify-symbols-mode))

  ;; Cleanup the modeline.
  (setq-default mode-line-format '("%e" mode-line-front-space
                                   (:propertize
                                    ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                                   mode-line-frame-identification
                                   mode-line-buffer-identification
                                   "   "
                                   mode-line-position
                                   mode-line-format-right-align
                                   "   "
                                   (vc-mode vc-mode)
                                   "   "
                                   mode-line-modes
                                   mode-line-misc-info
                                   mode-line-end-spaces)))

(provide 'td-theme)
;;; td-theme.el ends here
