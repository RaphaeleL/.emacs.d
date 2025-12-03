(load "~/.emacs.d/_base/lsp.el")

;; Theme
(lr/enable-custom-font-iosevka)
(lr/theme 'gruberdarker)
(global-whitespace-mode 1)

;; Extend exec-path and PATH environment variable
(let ((paths '("~/bin"
               "~/.local/bin"
               "/usr/local/bin"
               "~/go/bin"
               "~/.cargo/bin")))

  (dolist (p (reverse paths))
    (when (file-directory-p (expand-file-name p))
      (add-to-list 'exec-path (expand-file-name p))
      (setenv "PATH" (concat (expand-file-name p) path-separator (getenv "PATH"))))))
