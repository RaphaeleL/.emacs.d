(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Remove this is losing move-text and multiple-cursor
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default truncate-lines t)

(setq use-dialog-box nil)
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)

(setq mouse-yank-at-point t)
(setq compilation-scroll-output t)

(setq x-stretch-cursor nil)

(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

(setq ns-use-proxy-icon nil)

(blink-cursor-mode 0)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(set-fringe-mode 0)
(electric-indent-mode -1)
(global-auto-revert-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :font "Comic Code-16")

(setq custom-file "~/.emacs.d/output.el")
(load custom-file t)

(delete-selection-mode 1)

(setq mac-command-modifier 'meta
      mac-option-modifier 'control
      mac-control-modifier 'control
      default-input-method "MacOSX")

(setq insert-directory-program "/opt/homebrew/bin/gls")

(defun lr/delete-line ()
  (interactive)
  (delete-region (line-beginning-position) (line-beginning-position 2)))

(defun lr/duplicate-line ()
  (interactive)
  (save-excursion
    (let ((text (buffer-substring (line-beginning-position) (line-end-position)))) (end-of-line) (newline) (insert text))))

(defun lr/delete-trailing-whitespace () (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'lr/delete-trailing-whitespace)

(add-hook 'c-mode-common-hook
          (lambda ()
            (setq-local comment-start "// ")
            (setq-local comment-end "")))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "N")
              #'dired-create-empty-file))

(autoload 'mc/edit-lines "multiple-cursors" nil t)
(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)
(autoload 'mc/mark-previous-like-this "multiple-cursors" nil t)

(autoload 'move-text-up "move-text" nil t)
(autoload 'move-text-down "move-text" nil t)

(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(global-set-key (kbd "C-.") #'dired)
(global-set-key (kbd "C-,") #'find-file)
(global-set-key (kbd "M-C-,") #'project-find-file)

(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "M-o") #'switch-to-buffer)

(global-set-key (kbd "M-:") #'goto-line)

(global-set-key (kbd "M-i") #'buffer-menu)

(global-set-key (kbd "M-c") #'compile)
(global-set-key (kbd "M-s") #'shell-command)
(global-set-key (kbd "M-q") #'kill-compilation)

(global-set-key (kbd "C-x (") #'start-kbd-macro)
(global-set-key (kbd "C-x )") #'end-kbd-macro)
(global-set-key (kbd "C-x e") #'call-last-kbd-macro)

(global-set-key (kbd "M-w") #'mark-word)
(global-set-key (kbd "M-k") #'mark-sexp)
(global-set-key (kbd "M-a") #'mark-page)
(global-set-key (kbd "M-t") #'mark-paragraph)
(global-set-key (kbd "M-F") #'mark-defun)

(global-set-key (kbd "M-z") #'undo)

(global-set-key (kbd "M-d") #'lr/duplicate-line)
(global-set-key (kbd "M-r") #'lr/delete-line)

(global-set-key (kbd "M-p") #'move-text-up)
(global-set-key (kbd "M-n") #'move-text-down)

(global-set-key (kbd "M-e") #'mc/edit-lines)
(global-set-key (kbd "C-c C-j") #'mc/mark-next-like-this)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)

(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-decrease 1)))

(global-set-key (kbd "C-x 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-x 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-x 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-x 6") (lambda () (interactive) (insert "]")))
