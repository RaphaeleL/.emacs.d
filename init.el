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

(load "~/.emacs.d/_base/ssh.el")

;; - https://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html
;; - https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

