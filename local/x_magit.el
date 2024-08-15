(defun my-magit-status ()
  "Don't split window."
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))
