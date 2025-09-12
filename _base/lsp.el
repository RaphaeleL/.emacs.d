; ==================================================
; ===== LSP ========================================
; ==================================================

;; TODO: Add more LSP Servers
;; - Tailwind
;; - JS/TS
;; - Java

;; TODO: Auto Install the used Servers if they are
;;  not installed on the System.

;; Map the LSP Servers to the Lang Mode
(with-eval-after-load 'eglot
  (setq eglot-server-programs
        '((python-mode  . ("pylsp"))
          (c-mode       . ("clangd"))
          (c++-mode     . ("clangd"))
          (go-mode      . ("gopls"))
          (rust-mode    . ("rust-analyzer")))))

;; Hook eglot to major modes
(dolist (mode '(python-mode c-mode c++-mode go-mode rust-mode))
  (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

;; Get the full PATH from shell environment and set it for all processes
(let ((shell-path (shell-command-to-string "echo $PATH")))
  (when shell-path
    (let ((paths (split-string (string-trim shell-path) ":" t)))
      (dolist (path paths)
        (when (and path (file-exists-p path))
          (add-to-list 'exec-path path)))
      ;; Set the full PATH for subprocesses
      (setenv "PATH" shell-path))))

;; Ensure these are also in exec-path
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(setenv "PATH" (concat (expand-file-name "~/go/bin") ":" (getenv "PATH")))
(setenv "PATH" (concat (expand-file-name "/opt/homebrew/bin") ":" (getenv "PATH")))

;; (with-eval-after-load 'company
;;   (setq company-backends '((company-capf company-dabbrev-code company-dabbrev))))

;; LSP
(when (require 'eglot nil 'noerror)
  (when (executable-find "pylsp") (add-hook 'python-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c-mode-hook 'eglot-ensure))
  (when (executable-find "clangd") (add-hook 'c++-mode-hook 'eglot-ensure))
  (when (executable-find "gopls") (add-hook 'go-mode-hook 'eglot-ensure))
  (when (executable-find "rust-analyzer") (add-hook 'rust-mode-hook 'eglot-ensure)))

;; Make the LSP Toggleable whenever i want or dont want it. LSP may slow down things on Big files
;; This is not working perfectly, thereby we are enabling it on default right now.
(defvar lr/lsp-enabled t
  "Whether LSP (Eglot, hooks, etc.) is enabled.")

(defun lr/toggle-lsp ()
  "Toggle all LSP-related features (just eglot & hooks for now)."
  (interactive)
  (if lr/lsp-enabled
      ;; Disable LSP
      (progn
        ;; Remove eglot hooks
        (dolist (mode '(python-mode c-mode c++-mode go-mode rust-mode web-mode rust-mode))
          (remove-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

        ;; Shut down eglot sessions in all buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (bound-and-true-p eglot--managed-mode)
              (ignore-errors (eglot-shutdown (eglot-current-server))))))

        (setq lr/lsp-enabled nil)
        (message "LSP disabled."))
    ;; Enable LSP
    (progn
      (dolist (mode '(python-mode c-mode c++-mode go-mode rust-mode web-mode rust-mode))
        (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))

      (setq lr/lsp-enabled t)
      (message "LSP enabled."))))

;; Bind a Key to it
(global-set-key (kbd "C-c L") #'lr/toggle-lsp)
