;; Emacs Plus: https://github.com/d12frosted/homebrew-emacs-plus
;; (when (eq system-type 'darwin) (add-to-list 'default-frame-alist '(undecorated-round . t)))

;; LSP
(load "~/.emacs.d/_base/lsp.el")

;; Theme
(lr/modern)
(lr/line-relative)
(lr/theme 'solarized_light)

;; menu bar is not distracting on macos gui.
(if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
;; (if (display-graphic-p)
;;     (progn (visual-line-mode 1) (toggle-truncate-lines -1))
;;   (progn (visual-line-mode -1) (toggle-truncate-lines 1)))

;; Default Size and Position of Emacs on MacOS
(add-to-list 'default-frame-alist '(width . 136))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(left . 320))
(add-to-list 'default-frame-alist '(top . 120))

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
