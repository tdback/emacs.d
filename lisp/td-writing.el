;;; td-writing.el --- writing environment -*- lexical-binding: t; -*-

;;; Code:

(use-package jinx
  :ensure t
  :hook ((org-mode  . jinx-mode)
         (text-mode . jinx-mode))
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  ;; https://github.com/minad/corfu/discussions/457
  (text-mode-ispell-word-completion nil))

(use-package writegood-mode
  :ensure t
  :hook (jinx-mode . writegood-mode))

(use-package olivetti
  :ensure t
  :defer t
  :hook ((markdown-mode . olivetti-mode)
         (org-mode      . olivetti-mode))
  :init
  (setq olivetti-body-width 80))

(use-package markdown-mode
  :ensure t
  :defer t)

(provide 'td-writing)
;;; td-writing.el ends here
