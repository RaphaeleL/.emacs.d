;; ==================================================
;; ===== KEYMAPS ====================================
;; ==================================================

;; Selecting Stuff
(rc/create-keymap-m   "w"     'mark-word)
(rc/create-keymap-m   "a"     'mark-page)
(rc/create-keymap-m   "F"     'mark-defun)
(rc/create-keymap-m   "s"     'mark-paragraph)

;; Line Handling
(rc/create-keymap-m   "j"     'join-line)
(rc/create-keymap-m   "z"     'undo)
(rc/create-keymap-m   "d"     'rc/duplicate-line)
(rc/create-keymap-m   "r"     'rc/delete-line)
(rc/create-keymap-m   "g"     'indent-region)

;; Default Emacs stuff, just simpler
(rc/create-keymap-c   ","     'find-file)
(rc/create-keymap-c   "."     'dired)
(rc/create-keymap-m   ","     'project-find-file)
(rc/create-keymap-c   "g"     'magit-status)
(rc/create-keymap-m   "g"     'magit-status)

;; Shell Commands
(rc/create-keymap-m   "c"     'compile)
(rc/create-keymap-c   "l"     'shell-command)

;; Buffer Management
(rc/create-keymap-m   "i"     'ibuffer)
(rc/create-keymap-m   "o"     'switch-to-buffer)
(rc/create-keymap-m   "e"     'next-buffer)
(rc/create-keymap-m   "q"     'previous-buffer)

;; Copy and Paste
(rc/create-keymap-c   "w"     'rc/copy)
(rc/create-keymap-c   "y"     'rc/paste)
(rc/create-keymap-c   "t"     'rc/cut)

;; Moving a Selection up and down
(rc/create-keymap-m   "p"     'move-text-up)
(rc/create-keymap-m   "n"     'move-text-down)

;; Multi line Editing
(rc/create-keymap-m   "SPC"   'rectangle-mark-mode)
(rc/create-keymap-m   "i"     'mc/edit-lines)
(rc/create-keymap-c   "j"     'mc/mark-next-like-this)
(rc/create-keymap-c   "k"     'mc/mark-previous-like-this)

;; Stuff i do not need daily, but i definitely want a shortcut for
(rc/create-keymap     "<f1>"  (lambda () (interactive) (rc/toggle-buffer "*scratch*")))
(rc/create-keymap     "<f2>"  (lambda () (interactive) (rc/toggle-buffer "*compilation*")))
(rc/create-keymap     "<f3>"  'rc/toggle-themes)
(rc/create-keymap     "<f4>"  (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(rc/create-keymap     "<f5>"  'rc/toggle-mini-buffer-mode)
(rc/create-keymap     "<f6>"  'whitespace-mode)
(rc/create-keymap     "<f7>"  'display-line-numbers-mode)
(rc/create-keymap     "<f8>"  'which-key-mode)

;; Zooming
(rc/create-keymap-c   "="     (lambda () (interactive) (text-scale-increase 1)))
(rc/create-keymap-c   "+"     (lambda () (interactive) (text-scale-increase 1)))
(rc/create-keymap-c   "-"     (lambda () (interactive) (text-scale-decrease 1)))
(rc/create-keymap-m   "="     'global-text-scale-adjust)
(rc/create-keymap-m   "+"     'global-text-scale-adjust)
