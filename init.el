;; ===============================================
;; === BASE CONFIGURATION BASED ON THE OS ========
;; ===============================================

(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/init.windows.el"))
 ((eq system-type 'darwin)     (load "~/.emacs.d/_templates/init.mac.el"))
 ((eq system-type 'gnu/linux)  (load "~/.emacs.d/_templates/init.linux.el")))

;; ===============================================
;; === CUSTOM ADJUSTMENTS ========================
;; ===============================================

;; SSH Setup (FCO Windows to DevVM)
(load "~/.emacs.d/_base/ssh.el")

;; Better Moving in Different Kind of Buffers
(add-hook 'minibuffer-setup-hook   'rc/my-compile-minibuffer-setup)
(add-hook 'minibuffer-setup-hook   'rc/my-fido-minibuffer-setup)

