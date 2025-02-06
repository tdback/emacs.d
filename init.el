;;; -*- lexical-binding: t; -*-

;; Setup package.el and `use-package'.
(require 'package)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("elpa"   . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(when (version< emacs-version "27")
  (package-initialize)
  (load (concat user-emacs-directory "early-init.el")))

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ----- Package Imports -----

;; Load custom modules.
(add-to-list 'load-path '"~/.emacs.d/lisp")

(require 'td-common)
(require 'td-dired)
(require 'td-evil)
(require 'td-functions)
(require 'td-mu4e)
(require 'td-org)
(require 'td-present)
(require 'td-programming)
(require 'td-writing)

;;; ----- Sane Defaults -----

(column-number-mode)                                ; Display columns in our modeline.
(global-display-line-numbers-mode t)                ; Display line numbers in the buffer/modeline.
(prefer-coding-system 'utf-8)                       ; Always default to `utf-8'.
(save-place-mode 1)                                 ; Save our place in the file.
(fset 'yes-or-no-p 'y-or-n-p)                       ; Do I even have to explain this one?
(setq-default indent-tabs-mode nil)                 ; Disable tab indentation.
(setq-default tab-width 2)                          ; Set tabs to two spaces.
(setq inhibit-startup-message t)                    ; Don't show a startup message.
(setq echo-keystrokes 0.1)                          ; Don't wait long before showing keystrokes.
(setq ring-bell-function 'ignore)                   ; Don't make a sound when something goes south.
(setq use-dialog-box nil)                           ; Get rid of the dialog box.
(setq native-comp-async-report-warnings-errors nil) ; Silence compiler warnings.
(setq large-file-warning-threshold nil)             ; Don't warn when opening large files.
(setq vc-follow-symlinks t)                         ; Always follow symlinks.
(setq async-shell-command-display-buffer nil)       ; Only display a buffer if the command returns something.
(setq inhibit-x-resources t)                        ; Fix emacsclient issues.
(setq frame-resize-pixelwise t)                     ; Pixel perfect window resize.
(setq make-pointer-invisible t)                     ; This should hide the mouse... doesn't always work.
(setq word-wrap t)                                  ; Don't wrap in the middle of a word.
(setq save-place-forget-unreadable-files nil)       ; Always save our place in the file.
(setq display-line-numbers-type 'relative)          ; Show relative line numbers.

;;; ----- Keybinds -----

;; Make escape quit prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Increment/decrement a number at point, similar to vim's `C-a' and `C-x'.
(global-set-key (kbd "C-x n a") 'td/increment-number-at-point)
(global-set-key (kbd "C-x n x") 'td/decrement-number-at-point)

;;; ----- Hooks -----

;; Display startup stats.
(add-hook 'emacs-startup-hook #'td/display-startup-time)

;; Delete all trailing whitespace.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Fix font issues when running as a daemon.
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (td/set-font))))
  (td/set-font))

;; Disable line numbers for some modes.
(dolist (mode '(eshell-mode-hook
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
