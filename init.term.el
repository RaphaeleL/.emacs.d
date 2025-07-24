; =================================================
; ===== MINIMAL EMACS CONFIGURATION ===============
; =================================================

; This Configs aims to provide a minimal, fast and yet robust emacs config. It doesn't use any
; package manager, the only external package is simpleclip to improve copy/paste/cut behaviour
; on the different operation systems. In addition the config should work seamlessly and fast on any
;  operation system. For a fully fledged, configured and ide like emacs, check out init.el.

; to hold both configs (init.el and init.term.el) considure to alias one of them, like following:

; alias em="emacs -q -l ~/.emacs.d/init.term.el"
; alias emt="emacs -q -l ~/.emacs.d/init.term.el -nw"

; emacs with no config needs (on my system) 0.35s to load with a gui. this config needs around 0.3 - 0.4s
; to load with a gui. ... at least if it possible to trust the garbage output

;; === PLUGIN MANAGER ============================
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; === BASIC AND DEFAULT CONFIGURATION ===========

(use-package simpleclip
  :ensure t
  :defer t
  :bind (("C-y" . lr/paste)
         ("C-w" . lr/copy)
         ("C-t" . lr/cut)
         ("M-w" . mark-word)
         ("M-a" . mark-page)
         ("M-F" . mark-defun)
         ("M-s" . mark-paragraph)))

(use-package move-text
  :ensure t
  :defer t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)
         ("C-c C-j" . join-line)
         ("M-d" . lr/duplicate-line)
         ("M-r" . lr/delete-line)
		 ("M-z" . undo)
         ("C-c g" . indent-region)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-SPC" . rectangle-mark-mode)
         ("M-e"   . mc/edit-lines)
         ("C-j"   . mc/mark-next-like-this)
         ("C-k"   . mc/mark-previous-like-this)))

(use-package dired
  :ensure nil
  :defer t
  :bind (("C-." . dired-jump)))

(use-package dired-x :after dired :config
  (with-eval-after-load 'dired
    (setq dired-recursive-copies 'top)
    (setq dired-recursive-deletes 'top)
    (setq dired-dwim-target t)
    (setq dired-listing-switches "-laGh1Dv --group-directories-first")
    (setq ls-lisp-ignore-case t)
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (add-hook 'dired-mode-hook 'auto-revert-mode)
    (define-key dired-mode-map (kbd "M-o") #'dired-omit-mode)
    (define-key dired-mode-map (kbd "M-r") #'wdired-change-to-wdired-mode)
    (setq dired-omit-files "^\\.[^.].*")))

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(use-package markdown-mode :ensure t :mode ("\\.md\\'" . markdown-mode))
(use-package dockerfile-mode :ensure t :mode ("Dockerfile\\'" . dockerfile-mode))
(use-package jenkinsfile-mode :ensure t :mode ("Jenkinsfile\\'" . jenkinsfile-mode))
(use-package yaml-mode :ensure t :mode ("\\.ya?ml\\'" . yaml-mode))
(use-package jinja2-mode :ensure t :mode ("\\.j2\\'" . jinja2-mode))
(use-package go-mode :ensure t :mode ("\\.go\\'" . go-mode))
(use-package rust-mode :ensure t :mode ("\\.rs\\'" . rust-mode))
(use-package rpm-spec-mode :ensure t :mode ("\\.spec\\'" . rpm-spec-mode))
(use-package web-mode :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

; === History / Recent Files ===

(use-package savehist :ensure t :init (savehist-mode 1))
(use-package recentf :ensure t :init (recentf-mode 1))

(use-package whitespace
  :ensure nil
  :defer t
  :config
  (setq whitespace-style
        '(face
          tabs
          spaces
          tab-mark
          space-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space)))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :config
;  (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  (setq-default display-line-numbers-widen t)
  :init
  (global-display-line-numbers-mode 1))

(setq prefix-help-command #'embark-prefix-help-command)
(use-package emacs
  :ensure nil
  :bind (("C-," . find-file)
         ("M-," . project-find-file)
         ("M-i" . ibuffer)
         ("M-c" . compile)
         ("C-l" . shell-command)
         ("M-q" . kill-compilation)
         ("C-o" . other-window)
         ("C-=" . lr/font-increase)
         ("C-+" . lr/font-increase)
         ("C--" . lr/font-decrease)
         ("M-=" . global-text-scale-adjust)
         ("M-+" . global-text-scale-adjust)))

; === BASIC SETTINGS ============================

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(global-display-line-numbers-mode 1)

(setq-default mode-line-format nil)

(setq display-line-numbers-major-tick 0)
(setq display-line-numbers-minor-tick 0)
(setq-default display-line-numbers-widen t)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(set-fringe-mode 0)

(setq dired-dwim-target t)

(setq use-dialog-box nil)

(setq inhibit-startup-message t)
; (setq initial-scratch-message nil)
; (setq initial-major-mode 'text-mode)

(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.01)
(setq mouse-yank-at-point t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(delete-selection-mode 1)

(setq line-move-visual nil)

(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

; === HELPER ====================================

; --- BETTER COPY HELPER FUNCTIONS ---
(defun lr/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark))

(defun lr/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark))

(defun lr/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark))

(defun lr/delete-line ()
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun lr/duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun lr/font-increase() (interactive) (text-scale-increase 1))
(defun lr/font-decrease() (interactive) (text-scale-decrease 1))

;; =============================================================================
;; GARBAGE COLLECTION & PERFORMANCE OPTIMIZATIONS
;; =============================================================================

(defvar file-name-handler-alist-original file-name-handler-alist
  "Original `file-name-handler-alist` to restore after startup.")

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defvar efs/gc-cons-threshold 800000  ; ~0.8MB
  "Normal value for `gc-cons-threshold` after startup.")

(defvar efs/gc-cons-percentage 0.1
  "Normal value for `gc-cons-percentage` after startup.")

(defvar efs/last-focus-time (current-time)
  "Last time focus was lost.")

(defun efs/restore-startup-settings ()
  "Restore GC and file handler settings after startup."
  (setq gc-cons-threshold efs/gc-cons-threshold
        gc-cons-percentage efs/gc-cons-percentage)
  (setq file-name-handler-alist file-name-handler-alist-original)
  (when (boundp 'file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original))
  (garbage-collect))

(defun efs/focus-gc ()
  "Run GC if Emacs has been unfocused for more than 30 seconds."
  (let ((now (current-time)))
    (when (and (not (frame-focus-state))
               (> (float-time (time-subtract now efs/last-focus-time)) 30))
      (setq efs/last-focus-time now)
      (garbage-collect))))

(defun efs/minibuffer-setup-hook ()
  "Increase GC threshold while minibuffer is active."
  (setq gc-cons-threshold most-positive-fixnum))

(defun efs/minibuffer-exit-hook ()
  "Restore normal GC threshold after minibuffer use."
  (setq gc-cons-threshold efs/gc-cons-threshold))

(defun efs/handle-memory-pressure ()
  "Monitor memory usage and GC aggressively if needed."
  (when (and (fboundp 'memory-info)
             (let* ((stats (memory-info))
                    (used (/ (float (nth 3 stats)) (nth 2 stats))))
               (> used 0.9)))
    (message "High memory usage, collecting garbage...")
    (garbage-collect)))

(add-hook 'emacs-startup-hook #'efs/restore-startup-settings)
(add-hook 'minibuffer-setup-hook #'efs/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'efs/minibuffer-exit-hook)
(run-with-idle-timer 60 t #'garbage-collect)

(when (fboundp 'add-function)
  (add-function :after after-focus-change-function #'efs/focus-gc))

(when (fboundp 'memory-info)
  (run-with-idle-timer 300 t #'efs/handle-memory-pressure))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))
