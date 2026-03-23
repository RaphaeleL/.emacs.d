;; LSP Configuration for Emacs using Eglot and Mason
;; (use-package mason :ensure t :config (mason-ensure))
;; (mason-ensure
;;  (lambda ()
;;    (ignore-errors (mason-install "rust-analyzer"))
;;    (ignore-errors (mason-install "gopls"))
;;    (ignore-errors (mason-install "clangd"))))

;; Initialize exec-path from shell PATH
(when (memq system-type '(gnu/linux darwin))
  (let* ((shell-path (shell-command-to-string "echo $PATH"))
         (paths (split-string (string-trim shell-path) ":" t)))
    (setq exec-path (append paths exec-path))
    (setenv "PATH" (mapconcat 'identity exec-path ":"))))

;; Add extra paths
; (dolist (extra-path '("~/.local/bin" "/usr/local/bin" "/opt/homebrew/bin" "~/bin" "~/go/bin" "~/.cargo/bin"))
;   (let ((expanded-path (expand-file-name extra-path)))
;     (when (file-directory-p expanded-path)
;       (add-to-list 'exec-path expanded-path t)
;       (setenv "PATH" (concat expanded-path ":" (getenv "PATH"))))))

;; Manage Tsoding's Simpc Mode
; (load "~/.emacs.d/modes/simpc.el" 'noerror 'nomessage)
; (require 'simpc-mode)
; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; Treesitter or Base Mode
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . c-ts-mode))

;; Configure Eglot
(when (require 'eglot nil 'noerror)
  ;; Configure LSP servers
  (let ((server-programs nil))
    (when (executable-find "pylsp")         (push '(python-mode . ("pylsp")) server-programs))
    (when (executable-find "gopls")         (push '(go-mode     . ("gopls")) server-programs))
    (when (executable-find "clangd")        (push '(c-mode      . ("clangd")) server-programs))
    (when (executable-find "clangd")        (push '(c-ts-mode   . ("clangd")) server-programs))
    (when (executable-find "rust-analyzer") (push '(rust-mode   . ("rust-analyzer")) server-programs))
    (setq eglot-server-programs (append server-programs eglot-server-programs)))

  ;; Enable Eglot only for modes with configured servers
  (dolist (mode '(python-mode c-mode c-ts-mode go-mode rust-mode sh-mode))
    (when (assoc mode eglot-server-programs)
      (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure)))

  ;; Disable inlay hints globally
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1))))

(electric-indent-mode 1) ;; Enable electric indent mode

;; Do some C Settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-set-style "k&r")
(setq c-ts-mode-indent-offset 4)
(setq-default c-basic-offset 4)
(setq-default c-basic-offset 4)

; Do some LSP Settings
(setq eglot-extend-to-xref t)
(setq eglot-autoshutdown t)
(add-hook 'eglot-managed-mode-hook (lambda () (setq-local completion-category-defaults nil)))
(setq treesit-font-lock-level 4)

;; optional: usage of semantic tokens
(setq eglot-stay-out-of '(flymake))
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-category-defaults nil)
            (setq-local eglot-events-buffer-size 0)))
