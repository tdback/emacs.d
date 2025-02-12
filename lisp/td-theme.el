;;; td-theme.el --- emacs appearance -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

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

(defun td/disable-theme ()
  "Disable all currently enabled themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun td/toggle-theme ()
  "Toggle between light and dark modes."
  (interactive)
  (let ((theme (if (eq (car custom-enabled-themes) 'ef-dark)
                   'ef-day
                 'ef-dark)))
    (td/disable-theme)
    (load-theme theme t)))

(defun td/display-startup-time ()
    (message (if (daemonp)
                 "emacs daemon loaded in %s with %d garbage collections."
               "emacs loaded in %s with %d garbage collections.")
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

;;; Packages

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-modal-icon nil))

(use-package ef-themes
  :ensure t
  :config
  ;; By default start with a light theme.
  (load-theme 'ef-day t))

(use-package emacs
  :ensure nil
  :config
  (add-hook 'emacs-startup-hook #'td/display-startup-time)

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
    (add-hook mode 'prettify-symbols-mode)))

(provide 'td-theme)
;;; td-theme.el ends here
