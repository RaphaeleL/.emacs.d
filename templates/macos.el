;; -*- lexical-binding: t; -*-

;; Emacs Plus: https://github.com/d12frosted/homebrew-emacs-plus
;; (when (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated-round . t)))

;; Look and Feel
(lr/use-font "Comic Code-16")
(lr/default-theme) ;; (lr/theme 'lr_gruberdarker)
(mood-line-mode 1)
(global-whitespace-mode 0)
(set-fringe-mode '(8 . 8)) ;; (set-fringe-mode 0)
(lr/disable-vertico)
(lr/line-off) ;; (lr/line-relative)

;; LSP
(load "~/.emacs.d/base/lsp.el")

;; menu bar is not distracting on macos gui.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))

;; Default Size and Position of Emacs on MacOS
(add-to-list 'default-frame-alist '(width  . 136))
(add-to-list 'default-frame-alist '(height . 38))
;; (add-to-list 'default-frame-alist '(left   . 150))
;; (add-to-list 'default-frame-alist '(top    . 120))

;; Key remapping for MacOS
(setq mac-command-modifier 'meta     ; Command as Meta
      mac-option-modifier 'control   ; Option as Control
      mac-control-modifier 'control  ; Ensure Control is Control
      default-input-method "MacOSX")

;; Use GNU-ls from coreutils (via Homebrew) for dired
(setq insert-directory-program "/opt/homebrew/bin/gls")

;; MacOS with a German Keyboard
(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

(global-set-key (kbd "C-c C-8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c C-9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c C-5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c C-6") (lambda () (interactive) (insert "]")))
