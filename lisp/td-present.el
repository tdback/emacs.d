;;; td-present.el --- org-powered presentations -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

(defun td/org-present-start ()
  (setq-local face-remapping-alist
              '((default (:height 1.5) variable-pitch)
                (header-line (:height 4.0) variable-pitch)
                (org-document-title (:height 1.75) org-document-title)
                (org-code (:height 1.55) org-code)
                (org-verbatim (:height 1.55) org-verbatim)
                (org-block (:height 1.25) org-block)
                (org-block-begin-line (:height 0.7) org-block))))

(defun td/org-present-end ()
  (setq-local face-remapping-alist '((default variable-pitch default))))

(defun td/org-present-prepare-slide (buffer-name heading)
  (org-overview)
  (org-show-entry)
  (org-show-children))

;;; Packages

(use-package org-present
  :ensure t
  :hook ((org-present-mode                     . td/org-present-start)
         (org-present-mode-quit                . td/org-present-end)
         (org-present-after-navigate-functions . td/org-present-prepare-slide)))

(provide 'td-present)
;;; td-present.el ends here
