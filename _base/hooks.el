; ==================================================
; ===== HOOKS ======================================
; ==================================================

(add-to-list 'exec-path "~/.local/bin")

;; Replace yes-or-no-p with y-or-n-p (safer, loads later)
(add-hook 'after-init-hook
          (lambda ()
            (advice-add 'yes-or-no-p :override #'y-or-n-p)))

;; (with-eval-after-load 'company
;;   (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

;; Global company mode after init
; (add-hook 'after-init-hook 'global-company-mode)

;; Paredit only in Lisp modes
(dolist (mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode))
  (add-hook (intern (format "%s-hook" mode)) #'lr/turn-on-paredit))

;; On save hook for many modes
(dolist (mode '(c-mode c++-mode go-mode rust-mode python-mode simpc-mode
                      markdown-mode text-mode emacs-lisp-mode jenkinsfile-mode
                      dockerfile-mode makefile-mode rpm-spec-mode))
  (add-hook (intern (format "%s-hook" mode)) #'lr/on_save))

;; Combine minibuffer setup hooks
(add-hook 'minibuffer-setup-hook #'lr/minibuffer-setup-combined)

;; well
(add-to-list 'auto-mode-alist '("\\(?:[Mm]akefile\\|\\.mk\\)\\'" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.log\\'" . syslog-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
