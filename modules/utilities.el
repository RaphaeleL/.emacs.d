;;; utilities.el -*- lexical-binding: t; -*-
;; Utility functions and helpers

;; === KEYMAP HELPER FUNCTIONS ===================
(defun lira-create-keymap (key action)
  (define-key global-map (kbd key) action))

(defun lira-create-keymap-cc (key action)
  (define-key global-map (kbd (concat "C-c " key)) action))

(defun lira-create-keymap-ccc (key action)
  (define-key global-map (kbd (concat "C-c C-" key)) action))

(defun lira-create-keymap-cx (key action)
  (define-key global-map (kbd (concat "C-x " key)) action))

(defun lira-create-keymap-m (key action)
  (define-key global-map (kbd (concat "M-" key)) action))

(defun lira-create-keymap-c (key action)
  (define-key global-map (kbd (concat "C-" key)) action))

;; === EDITOR UTILITY FUNCTIONS ==================
(defun lira-delete-line ()
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun lira-duplicate-line ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

;; === CLIPBOARD UTILITY FUNCTIONS ===============
(defun lira-cut ()
  (interactive)
  (condition-case err
      (progn
        (simpleclip-cut (region-beginning) (region-end))
        (deactivate-mark)
        (message "Cut")
        (sit-for 1))
    (error
     (message "Warning: simpleclip not available, using kill-region")
     (kill-region (region-beginning) (region-end))
     (deactivate-mark))))

(defun lira-copy ()
  (interactive)
  (condition-case err
      (progn
        (simpleclip-copy (region-beginning) (region-end))
        (deactivate-mark)
        (message "Copied")
        (sit-for 1))
    (error
     (message "Warning: simpleclip not available, using kill-ring-save")
     (kill-ring-save (region-beginning) (region-end))
     (deactivate-mark))))

(defun lira-paste ()
  (interactive)
  (condition-case err
      (progn
        (simpleclip-paste)
        (deactivate-mark)
        (message "Pasted")
        (sit-for 1))
    (error
     (message "Warning: simpleclip not available, using yank")
     (yank)
     (deactivate-mark))))

;; === BUFFER UTILITY FUNCTIONS ==================
(defun lira-toggle-buffer (buffer-name)
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))
      (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

(defun lira-toggle-scratch-buffer() (interactive) (lira-toggle-buffer "*scratch*"))
(defun lira-toggle-compilation-buffer() (interactive) (lira-toggle-buffer "*compilation*"))
(defun lira-open-config() (interactive) (find-file "~/.emacs.d/init.el"))

;; === FONT UTILITY FUNCTIONS ====================
(defun lira-font-increase() (interactive) (text-scale-increase 1))
(defun lira-font-decrease() (interactive) (text-scale-decrease 1))
