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

(defun rc/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle)
  )

(defun rc/toggle-themes ()
  "Toggle between 'doom-one-light' and 'gruber-darker' themes."
  (interactive)
  (if (member 'doom-one-light custom-enabled-themes)
      (progn
        (disable-theme 'doom-one-light)
        (load-theme 'gruber-darker t))
    (progn
      (disable-theme 'gruber-darker)
      (load-theme 'doom-one-light t))))
