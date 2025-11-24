;; Emacs Plus: https://github.com/d12frosted/homebrew-emacs-plus
(when (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated-round . t)))

;; Key remapping for MacOS
(setq mac-command-modifier 'meta     ; Command as Meta
    mac-option-modifier 'control     ; Option as Control
    mac-control-modifier 'control    ; Ensure Control is Control
    default-input-method "MacOSX")

;; MacOS with a German Keyboard
(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

(global-set-key (kbd "C-c C-8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c C-9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c C-5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c C-6") (lambda () (interactive) (insert "]")))

;; LSP
(load "~/.emacs.d/_base/lsp.el")

;; Use GNU-ls from coreutils (via Homebrew) for dired
(setq insert-directory-program "/opt/homebrew/bin/gls")

;; Theme
(lr/modern)
(lr/line-off)
;; (lr/legacy)
(menu-bar-mode 1)

;; Extend exec-path and PATH environment variable
(let ((paths '("~/bin" "~/.local/bin" "/usr/local/bin" "/Library/TeX/texbin"
               "/opt/homebrew/opt/llvm/bin" "~/go/bin" "~/.cargo/bin")))

  (dolist (p (reverse paths))
    (when (file-directory-p (expand-file-name p))
      (add-to-list 'exec-path (expand-file-name p))
      (setenv "PATH" (concat (expand-file-name p) path-separator (getenv "PATH"))))))

;; Linker and Include Flags for LLVM
(setenv "LDFLAGS" "-L/opt/homebrew/opt/llvm/lib")
(setenv "CPPFLAGS" "-I/opt/homebrew/opt/llvm/include")

;; Default Size and Position of Emacs on MacOS
(add-to-list 'default-frame-alist '(width . 110))
(add-to-list 'default-frame-alist '(height . 38))
;(add-to-list 'default-frame-alist '(left . 320))
;(add-to-list 'default-frame-alist '(top . 70))
