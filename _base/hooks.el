; ==================================================
; ===== HOOKS ======================================
; ==================================================

(add-hook 'after-init-hook (lambda () (advice-add 'yes-or-no-p :override #'y-or-n-p)))

;; On save hook for many modes
(dolist (mode '(c-mode c++-mode go-mode rust-mode python-mode simpc-mode
                      markdown-mode text-mode emacs-lisp-mode jenkinsfile-mode
                      dockerfile-mode makefile-mode rpm-spec-mode))
  (add-hook (intern (format "%s-hook" mode)) #'lr/on_save))

;; Combine minibuffer setup hooks
(add-hook 'minibuffer-setup-hook #'lr/minibuffer-setup-combined)

;; better modes
(add-to-list 'auto-mode-alist '("\\(?:[Mm]akefile\\|\\.mk\\)\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
