(load "~/.emacs.d/_base/ssh.el")

;; Theme
;; (lr/legacy)
(lr/enable-custom-font-iosevka)
(lr/load-theme 'modus-operandi)
(global-whitespace-mode 0)

(custom-set-faces
 '(mode-line ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil)))))
