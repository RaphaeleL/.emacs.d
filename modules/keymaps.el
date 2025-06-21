;;; keymaps.el -*- lexical-binding: t; -*-
;; Keybindings configuration

(when (boundp 'lira-module-keymaps)
  ;; === HELPER FUNCTIONS ==========================
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

  ;; === CUSTOM FUNCTIONS ==========================
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

  (defun lira-toggle-mini-buffer-mode ()
    (interactive)
    (condition-case err
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
            (message "Switched to fido-mode")))
      (error
       (message "Warning: Completion modes not available"))))

  (defun lira-toggle-buffer (buffer-name)
    (interactive)
    (let ((buffer (get-buffer buffer-name)))
      (if (and buffer (get-buffer-window buffer))
          (delete-window (get-buffer-window buffer))
        (switch-to-buffer-other-window (get-buffer-create buffer-name)))))

  (defun lira-toggle-scratch-buffer() (interactive) (lira-toggle-buffer "*scratch*"))
  (defun lira-toggle-compilation-buffer() (interactive) (lira-toggle-buffer "*compilation*"))
  (defun lira-open-config() (interactive) (find-file "~/.emacs.d/init.el"))
  (defun lira-font-increase() (interactive) (text-scale-increase 1))
  (defun lira-font-decrease() (interactive) (text-scale-decrease 1))

  (defun lira-load-theme (theme)
    (interactive
     (list (intern (completing-read "Theme: " (custom-available-themes)))))
     (condition-case err
         (progn
           (setq custom-safe-themes t)
           (load-theme theme t)
           (message "Theme '%s' loaded successfully" theme))
       (error
        (message "Warning: Could not load theme '%s': %s" theme (error-message-string err)))))

  (defun lira-reload ()
    (interactive)
    (condition-case err
        (progn
          (load-file user-init-file)
          (message "Emacs reloaded."))
      (error
       (message "Warning: Could not reload Emacs: %s" (error-message-string err)))))

  ;; === KEYBINDINGS ===============================
  ;; ESC key - make it work as proper escape
  (lira-create-keymap "<escape>" 'keyboard-escape-quit)
  
  ;; Selecting Stuff
  (lira-create-keymap-m   "w"     'mark-word)
  (lira-create-keymap-m   "a"     'mark-page)
  (lira-create-keymap-m   "F"     'mark-defun)
  (lira-create-keymap-m   "s"     'mark-paragraph)

  ;; Line Handling
  (lira-create-keymap-ccc "j"     'join-line)
  (lira-create-keymap-m   "z"     'undo)
  (lira-create-keymap-m   "d"     'lira-duplicate-line)
  (lira-create-keymap-m   "r"     'lira-delete-line)
  (lira-create-keymap-c   "g"     'indent-region)

  ;; Default Emacs stuff, just simpler
  (lira-create-keymap-c   ","     'find-file)
  (lira-create-keymap-c   "."     'dired)
  (lira-create-keymap-m   ","     'project-find-file)
  (lira-create-keymap-m   "g"     'magit-status)

  ;; Shell Commands
  (lira-create-keymap-m   "c"     'compile)
  (lira-create-keymap-c   "l"     'shell-command)
  (lira-create-keymap-m   "q"     'kill-compilation)

  ;; Buffer Management
  (lira-create-keymap-m   "i"     'ibuffer)
  (lira-create-keymap-m   "o"     'switch-to-buffer)

  ;; Copy and Paste
  (lira-create-keymap-c   "w"     'lira-copy)
  (lira-create-keymap-c   "y"     'lira-paste)
  (lira-create-keymap-c   "t"     'lira-cut)

  ;; Moving a Selection up and down
  (lira-create-keymap-m   "p"     'move-text-up)
  (lira-create-keymap-m   "n"     'move-text-down)

  ;; Multi line Editing
  (lira-create-keymap-m   "SPC"   'rectangle-mark-mode)
  (lira-create-keymap-m   "e"     'mc/edit-lines)
  (lira-create-keymap-c   "j"     'mc/mark-next-like-this)
  (lira-create-keymap-c   "k"     'mc/mark-previous-like-this)

  ;; Zooming
  (lira-create-keymap-c   "="     'lira-font-increase)
  (lira-create-keymap-c   "+"     'lira-font-increase)
  (lira-create-keymap-c   "-"     'lira-font-decrease)
  (lira-create-keymap-m   "="     'global-text-scale-adjust)
  (lira-create-keymap-m   "+"     'global-text-scale-adjust)

  ;; Stuff i do not need daily, but i definitely want a shortcut for
  (lira-create-keymap     "<f1>"  'lira-toggle-scratch-buffer)
  (lira-create-keymap     "<f2>"  'lira-toggle-compilation-buffer)
  (lira-create-keymap     "<f3>"  'lira-load-theme)
  (lira-create-keymap     "<f4>"  'lira-open-config)
  (lira-create-keymap     "<f5>"  'lira-toggle-mini-buffer-mode)
  (lira-create-keymap     "<f6>"  'whitespace-mode)
  (lira-create-keymap     "<f7>"  'display-line-numbers-mode)
  (lira-create-keymap     "<f8>"  'which-key-mode)

  ;; If there are any new keymaps from the modular system not present above, add them here as well.
) 
