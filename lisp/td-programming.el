;;; td-programming.el --- for writing code -*- lexical-binding: t; -*-

;;; Code:

;;; Completion

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

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                     ; Cycle through candidates
  (corfu-auto t)                      ; Enable auto completion.
  (corfu-auto-prefix 2)               ; Display completion options after two characters.
  (corfu-auto-delay 0.0)              ; Don't delay.
  (corfu-quit-at-boundary 'separator) ; Quit if no separator has been inserted at the boundary.
  (corfu-echo-documentation 0.25)     ; Echo docs shortly after options.
  (corfu-preview-current 'insert)     ; Auto-insert the current completion.
  (corfu-preselect-first nil)         ; Don't select a completion right away.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("C-n"        . corfu-next)
              ([tab]        . corfu-next)
              ("C-p"        . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  ;; Use corfu everywhere.
  (global-corfu-mode)
  ;; Save completion history for better sorting.
  (corfu-history-mode))

(use-package eglot
  :ensure t
  :defer t
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(c-mode      . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode    . ("clangd")))
  (add-to-list 'eglot-server-programs '(nix-mode    . ("nixd")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(rust-mode   . ("rust-analyzer")))
  :hook
  ((c-mode      . eglot-ensure)
   (c++-mode    . eglot-ensure)
   (nix-mode    . eglot-ensure)
   (python-mode . eglot-ensure)
   (rust-mode   . eglot-ensure)))

(use-package orderless
  :ensure t
  :commands (orderless)
  :custom
  (completion-styles '(orderless flex)))

;;; Modes

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package nix-mode
  :ensure t
  :defer t)

(use-package python-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t
  :config
  (setq rust-format-on-save nil))

(use-package toml-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

;;; Tooling & Enhancements

(use-package compile
  :bind (("C-x C-m" . compile))
  :custom
  (compilation-scroll-output t)
  ;; By default, use `compile' to search for text in files and display results
  ;; in a separate buffer. I find this workflow to be a bit nicer than
  ;; `consult-ripgrep' when searching outside of the current directory tree.
  (compile-command "rg -nS --no-heading "))

(use-package devdocs
  :ensure t
  :defer t
  :bind (("C-h D d" . devdocs-lookup)
         ("C-h D i" . devdocs-install)
         ("C-h D x" . devdocs-delete)
         ("C-h D u" . devdocs-update-all)))

(use-package direnv
  :ensure t
  :custom
  (direnv-always-show-summary t)
  (direnv-show-paths-in-summary t)
  :config
  (direnv-mode))

(use-package magit
  :ensure t
  :commands magit-status)

(use-package paren-face
  :ensure t
  :hook ((prog-mode
          eshell-mode
          inferior-lisp-mode
          inferior-emacs-lisp-mode
          lisp-interaction-mode
          sly-mrepl-mode
          scheme-mode)
         . paren-face-mode))

(provide 'td-programming)
;;; td-programming.el ends here
