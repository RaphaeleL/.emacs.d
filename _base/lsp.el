;; LSP Configuration for Emacs using Eglot and Mason
(use-package mason :ensure t :config (mason-ensure))
(mason-ensure
 (lambda ()
   (ignore-errors (mason-install "rust-analyzer"))
   (ignore-errors (mason-install "gopls"))
   (ignore-errors (mason-install "clangd"))))

;; Initialize exec-path from shell PATH
; (when (memq system-type '(gnu/linux darwin))
;   (let* ((shell-path (shell-command-to-string "echo $PATH"))
;          (paths (split-string (string-trim shell-path) ":" t)))
;     (setq exec-path (append paths exec-path))
;     (setenv "PATH" (mapconcat 'identity exec-path ":"))))

;; Add extra paths
; (dolist (extra-path '("~/.local/bin" "/usr/local/bin" "/opt/homebrew/bin"))
;   (let ((expanded-path (expand-file-name extra-path)))
;     (when (file-directory-p expanded-path)
;       (add-to-list 'exec-path expanded-path t)
;       (setenv "PATH" (concat expanded-path ":" (getenv "PATH"))))))

;; Configure Eglot
(when (require 'eglot nil 'noerror)
  ;; Configure LSP servers
  (let ((server-programs nil))
    (when (executable-find "pylsp") (push '(python-mode . ("pylsp")) server-programs))
    (when (executable-find "gopls") (push '(go-mode . ("gopls")) server-programs))
    (when (executable-find "clangd") (push '(simpc-mode . ("clangd")) server-programs))
    (when (executable-find "rust-analyzer") (push '(rust-mode . ("rust-analyzer")) server-programs))
    (setq eglot-server-programs (append server-programs eglot-server-programs)))

  ;; Enable Eglot only for modes with configured servers
  (dolist (mode '(python-mode simpc-mode go-mode rust-mode sh-mode))
    (when (assoc mode eglot-server-programs)
      (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure)))

  ;; Disable inlay hints globally
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))

;; Enable electric indent mode
(electric-indent-mode 1)
