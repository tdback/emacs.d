;;; td-functions --- custom elisp functions -*- lexical-binding: t; -*-

;;; Code:

;;; Functions

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

;;; Packages

(use-package custom-functions
  :ensure nil
  :bind (("C-x n a" . td/increment-number-at-point)
         ("C-x n x" . td/decrement-number-at-point)))

(provide 'td-functions)
;;; td-functions.el ends here
