;;; td-common.el --- a step up from vanilla -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

(defun td/quit-if-not-in-macro ()
  "Allow for an unintentional `C-g' when recording macros."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (progn
        (if (region-active-p)
            (deactivate-mark)
          (message "Macro running. Can't quit.")))
    (keyboard-quit)))

(defun td/soft-kill-line ()
  "Kill a line while keeping expressions balanced. When there's no complete
sexp before the line end to delete, delete one sexp forward."
  (interactive)
  (require 'puni)
  (puni-soft-delete-by-move (lambda ()
                              (if (eolp)
                                  (forward-char)
                                (end-of-line)))
                            nil 'within 'kill 'delete-one))

;;; Packages

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode 1))

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.25))

(use-package cape
  :defer 10
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Nice completion to have available everywhere.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package completion-preview
  :ensure nil
  :demand t
  :hook (prog-mode . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("C-i" . completion-preview-insert)
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 2))

(use-package corfu
  :disabled
  :ensure t
  :custom
  (corfu-cycle t)                      ; Cycle through candidates
  (corfu-auto t)                       ; Enable auto completion.
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)      ; Insert previewed candidate.
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("C-n"        . corfu-next)
              ([tab]        . corfu-next)
              ("C-p"        . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :ensure t
  :demand t
  :bind (("M-s g" . consult-ripgrep)
         ("M-s f" . consult-find)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("C-x b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r"   . consult-history)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

(use-package emacs
  :ensure nil
  :demand t
  :bind (("M-c"     . capitalize-dwim)
         ("M-u"     . upcase-dwim)
         ("M-l"     . downcase-dwim)
         ("C-x C-b" . ibuffer)
         ("C-x M-t" . transpose-regions)
         ("C-g"     . td/quit-if-not-in-macro)))

(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Remove mixed indicator to prevent popup from being displayed automatically.
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)

  ;; Use embark to show command prefix help.
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after embark)

(use-package expand-region
  :ensure t
  :bind ([remap mark-paragraph] . er/expand-region))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-M-." . mc/mark-next-like-this)
         ("C-M-," . mc/mark-previous-like-this))
  :custom
  (mc/always-run-for-all t))

(use-package no-littering
  :ensure t
  :demand t
  :config
  ;; Set custom-file to a file that won't be tracked by git.
  (setq custom-file
        (let ((custom-file "custom.el"))
          (if (boundp 'server-socket-dir)
              (expand-file-name custom-file server-socket-dir)
            (no-littering-expand-etc-file-name custom-file))))
  (when (file-exists-p custom-file)
    (load custom-file t))

  ;; Don't litter project folders with backup files.
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/"     . nil)
            ("\\`/dev/shm/" . nil)
            ("."            . ,backup-dir))))

  ;; Tidy up auto-save files.
  (setq auto-save-default nil)
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat temporary-file-directory "\\2") t)
            ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
            ("." ,auto-save-dir t)))))

(use-package orderless
  :ensure t
  :commands (orderless)
  :custom
  (completion-styles '(orderless flex)))

(use-package puni
  :ensure t
  :hook (((eat-mode eshell-mode) . puni-disable-puni-mode)
         (prog-mode . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :bind (:map puni-mode-map
              ([remap forward-sentence]  . puni-beginning-of-sexp)
              ([remap backward-sentence] . puni-end-of-sexp)
              ([remap forward-sexp]      . puni-forward-sexp-or-up-list)
              ([remap backward-sexp]     . puni-backward-sexp-or-up-list)
              ([remap mark-paragraph]    . puni-expand-region)
              ([remap kill-line]         . puni-kill-line)
              ;; Make puni-kill-line less greedy.
              ([remap puni-kill-line]    . td/soft-kill-line)
              ("C-)"   . puni-slurp-forward)
              ("C-("   . puni-slurp-backward)
              ("C-}"   . puni-barf-forward)
              ("C-{"   . puni-barf-backward)
              ("M-("   . puni-wrap-round)
              ("C-M-t" . puni-transpose)
              ("C-M-?" . puni-convolute)
              ("C-M-z" . puni-squeeze)
              ("C-w"   . kill-region))
  :config
  (puni-global-mode t))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)
  :config
  (require 'vertico-directory)
  (vertico-mode))

(use-package which-key
  :ensure t
  :defer 0
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(provide 'td-common)
;;; td-common.el ends here
