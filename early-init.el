;; Raise GC limits for a faster startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 100000000
                                   gc-cons-percentage 0.1)))

;; Disable additional toolbars, scrollbars, etc.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
