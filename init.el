;; === CONFIGURATION BASED ON THE OS =============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/init.windows.el"))
 ((eq system-type 'darwin)     (load "~/.emacs.d/_templates/init.mac.el"))
 ((eq system-type 'gnu/linux)  (load "~/.emacs.d/_templates/init.linux.el")))

;; === CUSTOM USER SPECIFIC CHANGES ==============
(add-to-list 'default-frame-alist '(width . 88))
(add-to-list 'default-frame-alist '(height . 33))
; (lr/enable-custom-font)
; (lr/load-theme 'nord)
