; ==================================================
; ===== CUSTOM FUNCTIONS ===========================
; ==================================================

; some 'rc'-functions are copied from
; https://github.com/rexim/dotfiles/blob/master/.emacs.rc/rc.el

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (unless rc/package-contents-refreshed
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (unless (package-installed-p package)
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let
         (theme-package-name (concat theme-name "-theme"))
         (theme-package (intern theme-package-name)))
    (rc/require theme-package)
    (load-theme theme t))

(defun rc/delete-line ()
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun rc/duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun rc/on_save ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-20")
   ((eq system-type 'gnu/linux) "IosevkaNerdFont-12")))

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(defun rc/cut ()
  (interactive)
  (simpleclip-cut (region-beginning) (region-end))
  (deactivate-mark)
  (message "Cutted")
  (sit-for 1))

(defun rc/copy ()
  (interactive)
  (simpleclip-copy (region-beginning) (region-end))
  (deactivate-mark)
  (message "Copied")
  (sit-for 1))

(defun rc/paste ()
  (interactive)
  (simpleclip-paste)
  (deactivate-mark)
  (message "Pasted")
  (sit-for 1))

(defun rc/toggle-mini-buffer-mode ()
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

(defun rc/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle)
)

(defun rc/toggle-themes ()
  "Toggle between themes."
  (interactive)
  (if (member 'kaolin-valley-light custom-enabled-themes)
      (progn
        (disable-theme 'kaolin-valley-light)
        (load-theme 'gruber-darker t)
        (custom-set-faces))
    (progn
      (disable-theme 'gruber-darker)
      (load-theme 'kaolin-valley-light t)
      (custom-set-faces))))

(defun rc/toggle-buffer (buffer-name)
  "Toggle the visibility of the buffer named BUFFER-NAME in another window."
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

(defun rc/update-line-number-font-size ()
  "Update the font size of line numbers based on the current text scale."
  (let ((base-height (face-attribute 'default :height))
        (scale-factor (if (boundp 'text-scale-mode-amount)
                          (expt text-scale-mode-step text-scale-mode-amount)
                        1)))
    (face-remap-add-relative 'line-number
                             :height (round (* base-height scale-factor)))))


(defun rc/my-compile-minibuffer-setup ()
  (when (eq this-command 'compile)
    (local-set-key (kbd "C-n") 'next-history-element)
    (local-set-key (kbd "C-p") 'previous-history-element)))

(defun rc/my-fido-minibuffer-setup ()
  (when (or fido-mode fido-vertical-mode)
    (local-set-key (kbd "C-s") 'next-history-element)
    (local-set-key (kbd "C-r") 'previous-history-element)
    (local-set-key (kbd "C-n") 'icomplete-forward-completions)
    (local-set-key (kbd "C-p") 'icomplete-backward-completions)))
