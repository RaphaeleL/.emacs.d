(load "~/.emacs.d/_base/ssh.el")

;; Theme
(lr/enable-custom-font-aporetic)

(lr/load-theme 'modus-operandi)

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))

;; Copy & Paste to Clipboard

(defun lr/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark)
  (message "Cutted")
  (sit-for 1))

(defun lr/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark)
  (sit-for 1))

(defun lr/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark)
  (sit-for 1))

(use-package simpleclip
  :ensure t
  :defer t
  :bind (("C-y" . lr/paste)
         ("C-w" . lr/copy)
         ("C-t" . lr/cut)))
