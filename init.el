;; ---------------------------------------------------------------------------------
;; -------- Appearance -------------------------------------------------------------
;; ---------------------------------------------------------------------------------

;; No Startup Message
(setq inhibit-startup-message t)
(setq initial-scratch-message "\n\n\n")

;; Cleanup the UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(menu-bar-mode -1)
(spacious-padding-mode 1)

;; Theme
(load-theme 'modus-operandi-tinted t)

;; Treesitter
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Font
(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-14")
   ((eq system-type 'darwin) "Iosevka-14")
   ((eq system-type 'gnu/linux) "Iosevka-14")))

(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

;; Ido Mode for Files
(ido-mode 1)
(ido-everywhere 1)

;; Package Manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ido Mode for M-x
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Relative Line Numbering
(global-display-line-numbers-mode t)

;; Bigger Font
(set-face-attribute 'default nil :height 130)

;; Window Size
(when window-system (set-frame-size (selected-frame) 120 39))

;; Disable Backup and Autosave Settings
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ---------------------------------------------------------------------------------
;; -------- Tastatur ---------------------------------------------------------------
;; ---------------------------------------------------------------------------------

;; Deutsche Mac Tastatur
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

;; ---------------------------------------------------------------------------------
;; -------- Shortcuts --------------------------------------------------------------
;; ---------------------------------------------------------------------------------

;; Dired
(dired-preview-global-mode 1)
(global-set-key (kbd "C-x d") 'dired)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Buffer Navigation
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)
(global-set-key (kbd "C-c l") 'buffer-menu)
(global-set-key (kbd "C-c s") 'switch-to-buffer)

;; Window Navigation
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x j") 'windmove-down)

;; Close window
(global-set-key (kbd "C-x 0") 'delete-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)

;; Compile
(global-set-key (kbd "C-x m") 'compile)

;; Copy and Paste
(require 'simpleclip)
(simpleclip-mode 1)
(global-set-key (kbd "C-x b") 'simpleclib-cut)
(global-set-key (kbd "C-x v") 'simpleclip-copy)
(global-set-key (kbd "C-x p") 'simpleclip-paste)

;; Multi Cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mtark-next-like-this)
(global-set-key (kbd "C->") 'mc/mark-previous-like-this)

;; Font Size
(global-set-key (kbd "M-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "M--") (lambda () (interactive) (text-scale-decrease 1)))

;; Kill Current Buffer
(global-set-key (kbd "C-c k") (lambda () (interactive) (kill-current-buffer)))

;; Terminal
(global-set-key (kbd "C-c t") (lambda () (interactive) (vterm)))

g;; Which Key
(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; Move Text
(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Selection
(global-set-key (kbd "M-f") 'mark-word)
(global-set-key (kbd "M-a") 'mark-page)
(global-set-key (kbd "M-d") 'mark-defun)
(global-set-key (kbd "M-s") 'mark-paragraph)

;; ---------------------------------------------------------------------------------
;; -------- lsp --------------------------------------------------------------------
;; ---------------------------------------------------------------------------------

(set-fringe-mode 0)

(setq package-selected-packages '(lsp-mode yasnippet helm-lsp
    projectile hydra flycheck company avy helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'lisp-mode-hook 'lsp)
(add-hook 'bash-mode-hook 'lsp)

;; todo: add more lsps
;; - https://emacs-lsp.github.io/lsp-mode/page/lsp-cmake/
;; - cmake
;; - make
;; - java
;; - js/ts

(setq lsp-headerline-breadcrumb-enable nil)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

;; ---------------------------------------------------------------------------------
;; -------- modeline ---------------------------------------------------------------
;; ---------------------------------------------------------------------------------

(column-number-mode t)

(defun simple-mode-line-render (left right)
  "return a string of `window-width' length.
containing left, and right aligned respectively."
  (let ((available-width
	 (- (window-total-width)
	    (+ (length (format-mode-line left))
	       (length (format-mode-line right))))))
    (append left
	    (list (format (format "%%%ds" available-width) ""))
	    right)))

(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; left.
     (quote ("%e "
	     mode-line-buffer-identification
	     " %l : %c"
	     evil-mode-line-tag
	     "[%*]"))
     ;; right.
     (quote ("%p "
	     mode-line-frame-identification
	     mode-line-modes
	     mode-line-misc-info))))))

;; ---------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.
 '(custom-safe-themes
   '("a6713be6bfeae396adac720d62f46cef70c38b31e1c87aad9e57f2d60732f237"
     "aed3a896c4ea7cd7603f7a242fe2ab21f1539ab4934347e32b0070a83c9ece01"
     "3cbfdfce26469ddf69164b28d07852cc0f09a7b4b14d25ca40b34369db7e1664"
     "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     default))
 '(package-selected-packages
   '(avy bash-completion company flycheck helm-lsp helm-xref hydra
	 jupyter lsp-mode projectile rust-mode tree-sitter
	 tree-sitter-langs vterm yasnippet)))
(custom-set-faces )
