(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun lr/delete-line ()
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun lr/duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t))) (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun lr/use-font (font)
  (interactive)
  (when (member (car (split-string font "-")) (font-family-list))
    (set-frame-font font t t)
    (setq-default line-spacing 0)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (force-window-update (get-buffer-window buf))))))

(defun lr/on_save () (interactive) (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(defun lr/cut () (interactive) (simpleclip-cut (region-beginning) (region-end)) (deactivate-mark) (sit-for 1))
(defun lr/copy () (interactive) (simpleclip-copy (region-beginning) (region-end)) (deactivate-mark) (sit-for 1))
(defun lr/paste () (interactive) (simpleclip-paste) (deactivate-mark) (sit-for 1))

(defun lr/font-increase() (interactive) (text-scale-increase 1) (lr/update-line-number-font-size))
(defun lr/font-decrease() (interactive) (text-scale-decrease 1) (lr/update-line-number-font-size))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(with-eval-after-load 'dired (define-key dired-mode-map (kbd "N") 'dired-create-empty-file))
(add-to-list 'custom-theme-load-path
             (expand-file-name "emacs-themes/enhanced" user-emacs-directory))
(with-current-buffer "*scratch*" (fundamental-mode) (auto-fill-mode 1) (visual-line-mode 1) (toggle-word-wrap 1))

(when (boundp 'native-comp-enable-subr-trampolines) (setq native-comp-enable-subr-trampolines nil))
(when (boundp 'native-comp-async-report-warnings-errors) (setq native-comp-async-report-warnings-errors nil))

(save-place-mode 1)
(simpleclip-mode 1)

(setq window-resize-pixelwise t
      frame-resize-pixelwise  t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-revert-mode t)
(delete-selection-mode 1)
(electric-indent-mode -1)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq-default toggle-word-wrap t)

(setq use-dialog-box nil)

(set-fringe-mode 0)

(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq x-stretch-cursor nil)
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)
(setq mouse-yank-at-point t)
(setq compilation-scroll-output t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(setq window-divider-default-right-width 0
      window-divider-default-bottom-width 0
      window-divider-default-places nil)

(when (fboundp 'lr/remove-window-dividers)
  (lr/remove-window-dividers)
  (add-hook 'after-init-hook #'lr/remove-window-dividers)
  (add-hook 'after-make-frame-functions
            (lambda (_frame) (lr/remove-window-dividers)))
  (add-hook 'window-configuration-change-hook #'lr/remove-window-dividers))

(with-eval-after-load 'frame
  (set-face-attribute 'window-divider nil :foreground (face-background 'default))
  (set-face-attribute 'window-divider-first-pixel nil :foreground (face-background 'default))
  (set-face-attribute 'window-divider-last-pixel nil :foreground (face-background 'default)))

(setq ns-use-proxy-icon nil)

(require 'cl-lib)

(use-package multiple-cursors :defer t
  :bind (("M-SPC"   . rectangle-mark-mode)
         ("C-x SPC" . rectangle-mark-mode)
         ("M-e"     . mc/edit-lines)
         ("C-c C-j" . mc/mark-next-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)))

(use-package move-text :defer t :bind (("M-p" . move-text-up) ("M-n" . move-text-down)))

(use-package emacs
  :bind (("C-,"     . find-file)
         ("M-C-,"   . project-find-file)
         ("C-o"     . other-window)
         ("M-o"     . switch-to-buffer)
         ("M-:"     . goto-line)

         ("M-i"     . buffer-menu)
         ("M-c"     . compile)
         ("M-s"     . shell-command)
         ("M-q"     . kill-compilation)

         ("C-x ("   . start-kbd-macro)
         ("C-x )"   . end-kbd-macro)
         ("C-x e"   . call-last-kbd-macro)

         ("M-w"     . mark-word)
         ("M-k"     . mark-sexp)
         ("M-a"     . mark-page)
         ("M-t"     . mark-paragraph)
         ("M-F"     . mark-defun)

         ("M-z"     . undo)
         ("M-d"     . lr/duplicate-line)
         ("C-c C-d" . lr/duplicate-line)
         ("M-r"     . lr/delete-line)
         ("C-c C-r" . lr/delete-line)

         ("C-="     . (lambda () (interactive) (text-scale-increase 1)))
         ("C-+"     . (lambda () (interactive) (text-scale-increase 1)))
         ("C--"     . (lambda () (interactive) (text-scale-decrease 1)))))

(use-package custom-keys :bind
  (("M-3" . isearch-forward-thing-at-point) ("<f3>" . isearch-forward-thing-at-point)
   ("M-9" . fundamental-mode)               ("<f9>" . fundamental-mode)))

(use-package simpleclip
  :demand t
  :config
  (global-set-key (kbd "C-w") #'lr/copy)
  (global-set-key (kbd "C-y") #'lr/paste)
  (global-set-key (kbd "C-t") #'lr/cut)
  (with-eval-after-load 'multiple-cursors
    (define-key mc/keymap (kbd "C-w") #'kill-ring-save)
    (define-key mc/keymap (kbd "C-y") #'yank)
    (define-key mc/keymap (kbd "C-t") #'kill-region)))

(use-package savehist :init (savehist-mode 1))
(use-package recentf :init (recentf-mode 1))

(use-package display-line-numbers :defer t :config
  (setq-default display-line-numbers-widen t)
  (setq-default display-line-numbers-type 'on)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  :init (global-display-line-numbers-mode 1))

(setq custom-file "~/.emacs.d/output.el")
(load custom-file)

(lr/use-font "Comic Code-16")
(global-whitespace-mode 0)

(setq mac-command-modifier 'meta
      mac-option-modifier 'control
      mac-control-modifier 'control
      default-input-method "MacOSX")

(setq insert-directory-program "/opt/homebrew/bin/gls")

(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

(global-set-key (kbd "C-c C-8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c C-9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c C-5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c C-6") (lambda () (interactive) (insert "]")))

(add-hook 'after-init-hook (lambda () (advice-add 'yes-or-no-p :override #'y-or-n-p)))

(dolist (mode '(c-mode c++-mode go-mode rust-mode python-mode simpc-mode
                      markdown-mode text-mode emacs-lisp-mode jenkinsfile-mode
                      dockerfile-mode makefile-mode rpm-spec-mode))
  (add-hook (intern (format "%s-hook" mode)) #'lr/on_save))

(add-hook 'c-mode-common-hook (lambda () (setq-local comment-start "// ") (setq-local comment-end "")))
(add-hook 'c-ts-mode-hook (lambda () (setq-local comment-start "// ") (setq-local comment-end "")))
