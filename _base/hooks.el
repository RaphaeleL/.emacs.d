; ==================================================
; ===== HOOKS ======================================
; ==================================================

(add-to-list 'exec-path "~/.local/bin")

;; Replace yes-or-no-p with y-or-n-p (safer, loads later)
(add-hook 'after-init-hook
          (lambda ()
            (advice-add 'yes-or-no-p :override #'y-or-n-p)))

(with-eval-after-load 'company
  (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

(with-eval-after-load 'eglot
  (setq eglot-server-programs
        '((python-mode . ("pylsp"))
          (c-mode . ("clangd"))
          (c++-mode . ("clangd"))
          (rust-mode . ("rust-analyzer"))
          (go-mode . ("gopls")))))

;; Hook eglot to major modes
(dolist (mode '(python-mode c-mode c++-mode rust-mode go-mode))
  (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

;; Global company mode after init
(add-hook 'after-init-hook 'global-company-mode)

;; Paredit only in Lisp modes
(dolist (mode '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode))
  (add-hook (intern (format "%s-hook" mode)) #'rc/turn-on-paredit))

;; On save hook for many modes
(dolist (mode '(c-mode c++-mode go-mode rust-mode python-mode simpc-mode
                      markdown-mode text-mode emacs-lisp-mode jenkinsfile-mode
                      dockerfile-mode makefile-mode rpm-spec-mode))
  (add-hook (intern (format "%s-hook" mode)) #'rc/on_save))

;; Combine minibuffer setup hooks
(add-hook 'minibuffer-setup-hook #'rc/minibuffer-setup-combined)
