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
(lr/enable-custom-font)
(lr/load-theme 'solarized_light) ;; gruberdarker 
