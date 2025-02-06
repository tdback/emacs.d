;;; -*- lexical-binding: t; -*-

(use-package jinx
  :ensure t
  :hook ((org-mode  . jinx-mode)
         (text-mode . jinx-mode))
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package olivetti
  :ensure t
  :defer t
  :hook ((markdown-mode . olivetti-mode)
         (org-mode      . olivetti-mode))
  :init
  (setq olivetti-body-width 80))

(use-package writegood-mode
  :ensure t
  :hook (jinx-mode . writegood-mode))

(provide 'td-writing)
