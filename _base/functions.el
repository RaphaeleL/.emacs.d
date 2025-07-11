; ==================================================
; ===== CUSTOM FUNCTIONS ===========================
; ==================================================

; some those functions are copied from
; -> https://github.com/rexim/dotfiles/blob/master/.emacs.rc/rc.el

(defvar lr/package-contents-refreshed nil)

(defun lr/package-refresh-contents-once ()
  (unless lr/package-contents-refreshed
    (setq lr/package-contents-refreshed t)
    (package-refresh-contents)))

(defun lr/package-available-p (package)
  (and package-archive-contents
       (assq package package-archive-contents)))

(defun lr/require-one-package (package)
  "Install and require a package with better error handling"
  (unless (package-installed-p package)
    (lr/package-refresh-contents-once)
    (if (lr/package-available-p package)
        (condition-case err
            (package-install package)
          (error
           (message "Warning: Package '%s' failed to install: %s"
                    package (error-message-string err))
           nil))
      (message "Warning: Package '%s' is not available in any configured archive" package))))

(defun lr/require (&rest packages)
  (dolist (package packages)
    (lr/require-one-package package)))

(defun lr/require-theme (theme)
  (let
	(theme-package-name (concat theme-name "-theme"))
	(theme-package (intern theme-package-name)))
    (lr/require theme-package)
    (load-theme theme t))

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

(defun lr/get-default-font-family ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka")
   ((eq system-type 'darwin)     "Iosevka")
   ((eq system-type 'gnu/linux)  "IosevkaNerdFont")))

(defvar lr/original-font (face-attribute 'default :font))

(defun lr/enable-custom-font ()
  (interactive)
  (let ((font (lr/get-default-font)))
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-frame-font font t t)))

(defun lr/disable-custom-font ()
  (interactive)
  (setq default-frame-alist
        (cl-remove-if (lambda (entry)
                        (eq (car entry) 'font))
                      default-frame-alist))
  (set-frame-font lr/original-font t t))

(defun lr/get-default-font-size ()
  (cond
   ((eq system-type 'windows-nt) 14)
   ((eq system-type 'darwin)     20)
   ((eq system-type 'gnu/linux)  12)))

(defun lr/get-default-font ()
  (let ((family (lr/get-default-font-family))
        (size (lr/get-default-font-size)))
    (if (and family size)
        (format "%s-%d" family size)
      "Iosevka-20")))  ; fallback font

(defun lr/turn-on-paredit () (interactive) (paredit-mode 1))
(defun lr/on_save () (interactive) (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun lr/macoshhkb ()
  (interactive)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta))

(defun lr/load-theme (theme)
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (setq custom-safe-themes t)
  (load-theme theme t))

(defun lr/bare ()
  (interactive)
  (lr/disable-custom-font)
  (mapc #'disable-theme custom-enabled-themes))

(defun lr/fancy ()
  (interactive)
  (lr/enable-custom-font)
  (load-theme 'solarized_light t))


(defun lr/visual-or-line-copy ()
  (interactive)
  (let* ((start (if (use-region-p)
                    (region-beginning)
                  (line-beginning-position)))
         (end (if (use-region-p)
                  (region-end)
                (line-end-position)))
         (text (buffer-substring-no-properties start end)))
    (if (fboundp 'simpleclip-set-contents)
        (simpleclip-set-contents text)
      (kill-new text))
    (delete-region start end)
    (sit-for 0.05)
    (goto-char start)
    (insert text)
    (when (use-region-p) (deactivate-mark))
	(save-buffer)
	(beginning-of-line)
    (message (if (use-region-p) "Copied region" "Copied line"))))

(defun lr/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark)
  (message "Cutted")
  (sit-for 1))

(defun lr/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark)
  (message "Copied")
  (sit-for 1))

(defun lr/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark)
  (message "Pasted")
  (sit-for 1))

(defun lr/toggle-mini-buffer-mode ()
  (interactive)
  (if (bound-and-true-p fido-mode)
      (progn
        (fido-mode -1)
        (vertico-mode 1)
        (marginalia-mode 1)
        (message "Switched to vertico-mode"))
    (progn
      (vertico-mode -1)
      (marginalia-mode -1)
      (fido-mode 1)
      (message "Switched to fido-mode"))))

(defun lr/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle))

(defun lr/toggle-buffer (buffer-name)
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

(defun lr/toggle-scratch-buffer() (interactive) (lr/toggle-buffer "*scratch*"))
(defun lr/toggle-compilation-buffer() (interactive) (lr/toggle-buffer "*compilation*"))
(defun lr/open_config() (interactive) (find-file "~/.emacs.d/init.el"))
(defun lr/font-increase() (interactive) (text-scale-increase 1))
(defun lr/font-decrease() (interactive) (text-scale-decrease 1))

(defun lr/update-line-number-font-size ()
  (let ((base-height (face-attribute 'default :height))
        (scale-factor (if (boundp 'text-scale-mode-amount) (expt text-scale-mode-step text-scale-mode-amount) 1)))
        (face-remap-add-relative 'line-number :height (round (* base-height scale-factor)))))

(defun lr/my-compile-minibuffer-setup ()
  (when (eq this-command 'compile)
    (local-set-key (kbd "C-n") 'next-history-element)
    (local-set-key (kbd "C-p") 'previous-history-element)))

(defun lr/my-fido-minibuffer-setup ()
  (when (or fido-mode fido-vertical-mode)
    (local-set-key (kbd "C-s") 'next-history-element)
    (local-set-key (kbd "C-r") 'previous-history-element)
    (local-set-key (kbd "C-n") 'icomplete-forward-completions)
    (local-set-key (kbd "C-p") 'icomplete-backward-completions)))

(defun lr/create-keymap     (key action) (global-set-key (kbd key) action))
(defun lr/create-keymap-cc  (key action) (global-set-key (kbd (concat "C-c "   key)) action))
(defun lr/create-keymap-ccc (key action) (global-set-key (kbd (concat "C-c C-" key)) action))
(defun lr/create-keymap-cx  (key action) (global-set-key (kbd (concat "C-x "   key)) action))
(defun lr/create-keymap-m   (key action) (global-set-key (kbd (concat "M-"     key)) action))
(defun lr/create-keymap-c   (key action) (global-set-key (kbd (concat "C-"     key)) action))

(defun lr/reload ()
  (interactive)
  (load-file user-init-file)
  (message "Emacs reloaded."))

(defun lr/minibuffer-setup-combined ()
  (lr/my-compile-minibuffer-setup)
  (lr/my-fido-minibuffer-setup))
