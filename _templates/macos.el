;; Key remapping:
(setq mac-command-modifier 'meta     ; Command as Meta
    mac-option-modifier 'control     ; Option as Control
    mac-control-modifier 'control    ; Ensure Control is Control
    default-input-method "MacOSX")

;; WTF
(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

;; LSP
(load "~/.emacs.d/_base/lsp.el")

;; Use GNU ls from coreutils (via Homebrew)
(setq insert-directory-program "/opt/homebrew/bin/gls")

;; Theme
(lr/enable-custom-font-iosevka)
;; (lr/load-theme 'gruberdarker)
(lr/load-theme 'solarized_light)

;; Extend exec-path and PATH environment variable
(let ((paths '("~/bin"
               "~/go/bin"
               "~/.local/bin"
               "~/.cargo/bin"
               "/usr/local/bin"
			   "/Library/TeX/texbin"
               "/opt/homebrew/opt/llvm/bin")))
  (dolist (p (reverse paths))
    (when (file-directory-p (expand-file-name p))
      (add-to-list 'exec-path (expand-file-name p))
      (setenv "PATH" (concat (expand-file-name p) path-separator (getenv "PATH"))))))

;; Extra environment variables for LLVM
(setenv "LDFLAGS" "-L/opt/homebrew/opt/llvm/lib")
(setenv "CPPFLAGS" "-I/opt/homebrew/opt/llvm/include")

;; Default Size and Position of Emacs on MacOS
(setq default-frame-alist
      '((height . 33)
        (width . 88)
        (left . 320)
        (top . 70)))
