(load "~/.emacs.d/_base/lsp.el")
(load "~/.emacs.d/_base/ssh.el")

;; Theme
(lr/enable-custom-font-iosevka)
(lr/load-theme 'gruberdarker)
(global-whitespace-mode 1)

;; Extend exec-path and PATH environment variable
(let ((paths '("~/bin"
               "~/go/bin"
               "~/.local/bin"
               "~/.cargo/bin"
               "/usr/local/bin")))

  (dolist (p (reverse paths))
    (when (file-directory-p (expand-file-name p))
      (add-to-list 'exec-path (expand-file-name p))
      (setenv "PATH" (concat (expand-file-name p) path-separator (getenv "PATH"))))))
