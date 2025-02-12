;;; td-shell.el --- a shell and a repl! -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

(defun td/meow-insert-state (mode)
  "If meow is installed, start `mode' in INSERT state."
  (when (package-installed-p 'meow)
    (add-to-list 'meow-mode-state-list `(,mode . insert))))

(defun td/eshell-prompt ()
  (concat
   (propertize (abbreviate-file-name (eshell/pwd)) 'face `(:foreground "white"))
   (if (zerop (user-uid))
       (propertize " # " 'face `(:foreground "red"))
     (propertize " λ " 'face `(:foreground "yellow")))))

(defun td/eshell-configure ()
  (require 'xterm-color)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Use xterm-256color when running interactive commands.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Save command history when commands are entered.
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Initialize the shell history.
  (eshell-hist-initialize)

  ;; Truncate buffer for performance.
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-prompt-function           'td/eshell-prompt
        eshell-prompt-regexp             "^.+ λ "
        eshell-history-size              10000
        eshell-buffer-maximum-lines      10000
        eshell-hist-ignoredups           t
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions     nil))

(defun td/eshell-clear ()
  "Allow the user to clear the current eshell buffer while retaining the
current line input."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear t)
    (eshell-emit-prompt)
    (insert input)))

;;; Packages

(use-package eat
  :ensure t
  :bind (("C-x S" . eat))
  :custom
  (eat-enable-autoline-mode t)
  (eat-kill-buffer-on-exit t)
  :config
  (td/meow-insert-state 'eat-mode))

(use-package eshell
  :ensure nil
  :hook (eshell-first-time-mode . td/eshell-configure)
  :bind (("C-l" . td/eshell-clear))
  :config
  (td/meow-insert-state 'eshell-mode))

(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package xterm-color)

(provide 'td-shell)
;;; td-shell.el ends here
