(load "~/.emacs.d/_base/ssh.el")

;; Theme
(lr/enable-custom-font-aporetic)
(lr/load-theme 'gruberdarker)
(global-whitespace-mode 1)

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))
