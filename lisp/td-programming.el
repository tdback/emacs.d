;;; td-programming.el --- for writing code -*- lexical-binding: t; -*-

;;; Code:

;;; Modes

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package go-mode
  :ensure t
  :defer t
  :hook (before-save . gofmt-before-save))

(use-package js-mode
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2))

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

(use-package typescript-mode
  :ensure t
  :defer t
  :custom
  (typescript-indent-level 2))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

;;; Tooling & Enhancements

(use-package compile
  :bind (("C-x C-m" . compile))
  :custom
  (compilation-scroll-output t)
  (compile-command ""))

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

(use-package eglot
  :ensure t
  :defer t
  :custom
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(c-mode          . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode        . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode         . ("gopls")))
  (add-to-list 'eglot-server-programs '(js-mode         . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(nix-mode        . ("nixd")))
  (add-to-list 'eglot-server-programs '(python-mode     . ("pylsp")))
  (add-to-list 'eglot-server-programs '(rust-mode       . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(zig-mode        . ("zls")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  :hook
  ((c-mode          . eglot-ensure)
   (c++-mode        . eglot-ensure)
   (go-mode         . eglot-ensure)
   (js-mode         . eglot-ensure)
   (nix-mode        . eglot-ensure)
   (python-mode     . eglot-ensure)
   (rust-mode       . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (zig-mode        . eglot-ensure)))

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
