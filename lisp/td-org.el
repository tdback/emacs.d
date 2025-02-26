;;; td-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

(defun td/org-font-setup ()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)))
    (set-face-attribute (car face)
                        nil
                        :font "Aporetic Sans Mono"
                        :weight 'regular
                        :height (cdr face)))
  (set-face-attribute 'org-block
                      nil
                      :inherit 'fixed-pitch)
  (set-face-attribute 'org-code
                      nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table
                      nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim
                      nil
                      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword
                      nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line
                      nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox
                      nil
                      :inherit 'fixed-pitch))

(defun td/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

;;; Packages

(use-package org
  :ensure nil
  :hook (org-mode . td/org-mode-setup)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-startup-folded t
        org-hide-leading-stars t
        org-edit-src-content-indentation 2
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-hide-block-startup nil
        org-src-tab-acts-natively t
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-files '("~/Org/tasks.org"
                           "~/Org/ideas.org")
        org-agenda-custom-commands '(("d" "Dashboard"
                                      ((agenda ""
                                               ((org-deadline-warning-days 7)))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "Next Tasks")))))
                                     ("n" "Next Tasks"
                                      ((todo "NEXT"
                                             ((org-agenda-overriding-header "Next Tasks")))))
                                     ("s" "Status"
                                      ((todo "ACTIVE"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files org-agenda-files)))
                                       (todo "BACKLOG"
                                             ((org-agenda-overriding-header "Backlog")
                                              (org-agenda-todo-list-sublevels nil)
                                              (org-agenda-files org-agenda-files)))
                                       (todo "CANCELED"
                                             ((org-agenda-overriding-header "Canceled")
                                              (org-agenda-files org-agenda-files)))
                                       (todo "COMPLETED"
                                             ((org-agenda-overriding-header "Completed")
                                              (org-agenda-files org-agenda-files)))
                                       (todo "PLAN"
                                             ((org-agenda-overriding-header "In Planning")
                                              (org-agenda-todo-list-sublevels nil)
                                              (org-agenda-files org-agenda-files)))
                                       (todo "READY"
                                             ((org-agenda-overriding-header "Ready for Work")
                                              (org-agenda-files org-agenda-files))))))
        org-capture-templates '(("t" "Tasks")
                                ("tt" "New Task" entry (file+olp (car org-agenda-files))
                                 "* TODO %?\n %i" :empty-lines 1)
                                ("i" "Ideas")
                                ("ii" "New Idea" entry (file+olp (cadr org-agenda-files))
                                 "* %^{Idea}\n %U\n %?\n %i" :empty-lines 1))
        org-todo-keywords '((sequence "TODO(t)"
                                      "NEXT(n)"
                                      "|"
                                      "DONE(d!)")
                            (sequence "BACKLOG(b)"
                                      "PLAN(p)"
                                      "READY(r)"
                                      "ACTIVE(a)"
                                      "|"
                                      "COMPLETED(c)"
                                      "CANCELED(k@)"))
        org-refile-targets '(("archive.org" :maxlevel . 1)
                             ("tasks.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (td/org-font-setup))

(use-package org-appear
  :ensure t
  :after org
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
  (org-appear-inside-latex t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

(use-package org-modern
  :ensure t
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-star nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands
  (org-modern-mode org-modern-agenda)
  :init
  (global-org-modern-mode))

(use-package org-timeline
  :ensure t
  :commands org-agenda
  :init
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;; Code block execution and template expansion.
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python     . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sqlite")))

(provide 'td-org)
;;; td-org.el ends here
