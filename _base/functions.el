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

(defun lr/modern ()
  (interactive)
  (lr/enable-custom-font-iosevka)
  ;; (lr/theme 'gruberdarker)
  (global-whitespace-mode 0)
  (fido-mode 0)
  (vertico-mode 1)
  (mood-line-mode 1)
  (marginalia-mode 1)
  (lr/line-relative)
  (set-fringe-mode 0))

(defun lr/legacy ()
  (interactive)
  (lr/disable-custom-font)
  (lr/enable-custom-font-legacy)
  (mapc #'disable-theme custom-enabled-themes)
  (cond
   ((eq system-type 'windows-nt) (lr/disable-custom-font))
   ((eq system-type 'darwin)     (lr/enable-custom-font-legacy))
   ((eq system-type 'gnu/linux)  (lr/disable-custom-font)))
  (global-whitespace-mode 0)
  (fido-mode 1)
  (vertico-mode 0)
  (marginalia-mode 0)
  (lr/line-off)
  (set-fringe-mode 0))

(defun lr/disable-custom-font ()
  (interactive)
  (setq default-frame-alist
        (cl-remove-if (lambda (entry) (eq (car entry) 'font)) default-frame-alist))
  (set-frame-font (face-attribute 'default :font) t t))

(defun lr/get-iosevka-font ()
  (let ((family (cond
                 ((eq system-type 'windows-nt) "Iosevka")
                 ((eq system-type 'darwin)     "Iosevka")
                 ((eq system-type 'gnu/linux)  "IosevkaNerdFont")))
        (size (cond
               ((eq system-type 'windows-nt) 12)
               ((eq system-type 'darwin)     18)
               ((eq system-type 'gnu/linux)  12))))
    (if (and family size) (format "%s-%d" family size) "Iosevka-20")))

(defun lr/enable-custom-font-iosevka ()
  (interactive)
  (let ((font (lr/get-iosevka-font)))
    (when (member (car (split-string font "-")) (font-family-list))
      (set-frame-font font t t)
      (setq-default line-spacing 0)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (force-window-update (get-buffer-window buf)))))))

(defun lr/enable-custom-font-legacy ()
  (interactive)
  (let ((fonts '("Consoleet Darwin" "Fairfax" "Consoleet Darwin Smooth"))
        (size 18)
        chosen-font)
    (dolist (f fonts)
      (when (and (member f (font-family-list)) (not chosen-font))
        (setq chosen-font f)))
    (if chosen-font
        (progn
          (set-frame-font (format "%s-%d" chosen-font size) t t)
          (setq-default line-spacing 0.2)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (force-window-update (get-buffer-window buf)))))
      (message "Consoleet Darwin not found, using default font"))))


(defun lr/theme (theme)
  (interactive
   (list (intern (completing-read "Theme: " (custom-available-themes)))))
  (mapc #'disable-theme custom-enabled-themes)
  (setq custom-safe-themes t)
  (load-theme theme t))

(defun lr/on_save () (interactive) (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(defun lr/cut () (interactive) (simpleclip-cut (region-beginning) (region-end)) (deactivate-mark) (sit-for 1))
(defun lr/copy () (interactive) (simpleclip-copy (region-beginning) (region-end)) (deactivate-mark) (sit-for 1))
(defun lr/paste () (interactive) (simpleclip-paste) (deactivate-mark) (sit-for 1))

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
  (interactive "Buffer name: ")
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

(defun lr/open_config() (interactive) (find-file "~/.emacs.d/init.el"))
(defun lr/toggle-config ()
  (interactive)
  (let* ((file "~/.emacs.d/init.el")
         (buffer (find-file-noselect file)))
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (switch-to-buffer-other-window buffer))))
(defun lr/toggle-scratch-buffer () (interactive) (lr/toggle-buffer "*scratch*"))
(defun lr/toggle-compilation-buffer () (interactive) (lr/toggle-buffer "*compilation*"))

(defun lr/update-line-number-font-size ()
  (interactive)
  (let ((base-height (face-attribute 'default :height))
        (scale-factor (if (boundp 'text-scale-mode-amount) (expt text-scale-mode-step text-scale-mode-amount) 1)))
    (face-remap-add-relative 'line-number :height (round (* base-height scale-factor)))))

(defun lr/font-increase() (interactive) (text-scale-increase 1) (lr/update-line-number-font-size))
(defun lr/font-decrease() (interactive) (text-scale-decrease 1) (lr/update-line-number-font-size))

(defun lr/line-normal() (interactive) (menu-bar--display-line-numbers-mode-absolute))
(defun lr/line-relative() (interactive) (menu-bar--display-line-numbers-mode-relative))
(defun lr/line-off() (interactive) (menu-bar--display-line-numbers-mode-none))

(defun lr/my-compile-minibuffer-setup ()
  (when (eq this-command 'compile)
    (local-set-key (kbd "C-n") 'next-history-element)
    (local-set-key (kbd "C-p") 'previous-history-element)))

(defun lr/my-fido-minibuffer-setup ()
  (when (or fido-mode fido-vertical-mode)
    (local-set-key (kbd "C-s") 'next-history-element)
    (local-set-key (kbd "C-r") 'previous-history-element)
    (unless (eq this-command 'compile)
      (local-set-key (kbd "C-n") 'icomplete-forward-completions)
      (local-set-key (kbd "C-p") 'icomplete-backward-completions))))

(defun lr/create-keymap     (key action) (global-set-key (kbd key) action))
(defun lr/create-keymap-cc  (key action) (global-set-key (kbd (concat "C-c "   key)) action))
(defun lr/create-keymap-ccc (key action) (global-set-key (kbd (concat "C-c C-" key)) action))
(defun lr/create-keymap-cx  (key action) (global-set-key (kbd (concat "C-x "   key)) action))
(defun lr/create-keymap-m   (key action) (global-set-key (kbd (concat "M-"     key)) action))
(defun lr/create-keymap-c   (key action) (global-set-key (kbd (concat "C-"     key)) action))

(defun lr/reload () (interactive) (load-file user-init-file) (message "Emacs reloaded."))
(defun lr/minibuffer-setup-combined () (lr/my-compile-minibuffer-setup) (lr/my-fido-minibuffer-setup))

;; Function to remove dividers from all windows
(defun lr/remove-window-dividers ()
  (window-divider-mode -1)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (set-window-parameter window 'window-divider-right-width 0)
      (set-window-parameter window 'window-divider-bottom-width 0))))
