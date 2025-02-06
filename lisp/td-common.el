;;; -*- lexical-binding: t; -*-

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode 1))

(use-package beframe
  :ensure t
  :config
  (global-set-key (kbd "C-c b") 'beframe-prefix-map)
  (beframe-mode 1))

(use-package consult
  :ensure t
  :demand t
  :bind (("C-s"     . consult-ripgrep)
         ("C-M-l"   . consult-imenu)
         ("C-M-j"   . consult-buffer)
         ("C-x C-b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r"     . consult-history)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function nil))

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
