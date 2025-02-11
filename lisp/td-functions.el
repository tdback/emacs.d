;;; td-functions --- custom elisp functions -*- lexical-binding: t; -*-

;;; Code:

(defun td/display-startup-time ()
  (message (if (daemonp)
               "emacs daemon loaded in %s with %d garbage collections."
             "emacs loaded in %s with %d garbage collections.")
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(defun td/set-font ()
  (when (display-graphic-p)
    (message "Setting font...")
    (set-face-attribute 'default nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)
    (set-face-attribute 'fixed-pitch nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)
    (set-face-attribute 'variable-pitch nil
                        :font "Aporetic Sans Mono"
                        :weight 'normal
                        :height 200)))

(defun td/quit-if-not-in-macro ()
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (progn
        (if (region-active-p)
            (deactivate-mark)
          (message "Macro running. Can't quit.")))
    (keyboard-quit)))

(defun td/increment-number-at-point (&optional increment)
  "Increment number at point like vim's `C-a'."
  (interactive "p")
  (td/change-number-at-point '+ (or increment 2)))

(defun td/decrement-number-at-point (&optional decrement)
  "Decrement number at point like vim's `C-x'."
  (interactive "p")
  (td/change-number-at-point '- (or decrement 2)))

(defun td/change-number-at-point (change operation)
  "The generic logic for incrementing and decrementing numbers at point."
  (search-forward-regexp (rx digit))
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (forward-word)
      (search-backward (number-to-string number))
      (replace-match (number-to-string (funcall change number operation)))
      (goto-char (- point 1)))))

(defun td/disable-theme ()
  "Disable all themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun td/toggle-theme ()
  "Toggle between light and dark modes."
  (interactive)
  (let ((theme (if (eq (car custom-enabled-themes) 'ef-dark)
                   'ef-day
                 'ef-dark)))
    (td/disable-theme)
    (load-theme theme t)))

(provide 'td-functions)
;;; td-functions.el ends here
