;; ==================================================
;; ===== KEYMAPS ====================================
;; ==================================================

;; Selecting Stuff
(rc/create-keymap-m   "w"     'mark-word)
(rc/create-keymap-m   "a"     'mark-page)
(rc/create-keymap-m   "F"     'mark-defun)
(rc/create-keymap-m   "s"     'mark-paragraph)

;; Line Handling
(rc/create-keymap-ccc "j"     'join-line)
(rc/create-keymap-m   "z"     'undo)
(rc/create-keymap-m   "d"     'rc/duplicate-line)
(rc/create-keymap-m   "r"     'rc/delete-line)
(rc/create-keymap-c   "g"     'indent-region)

;; Default Emacs stuff, just simpler
(rc/create-keymap-c   ","     'find-file)
(rc/create-keymap-c   "."     'dired)
(rc/create-keymap-m   ","     'project-find-file)
(rc/create-keymap-m   "g"     'magit-status)

;; Shell Commands
(rc/create-keymap-m   "c"     'compile)
(rc/create-keymap-c   "l"     'shell-command)
(rc/create-keymap-m   "q"     'kill-compilation)

;; Buffer Management
(rc/create-keymap-m   "i"     'ibuffer)
(rc/create-keymap-m   "o"     'switch-to-buffer)

;; Copy and Paste
(rc/create-keymap-c   "w"     'rc/copy)
(rc/create-keymap-c   "y"     'rc/paste)
(rc/create-keymap-c   "t"     'rc/cut)

;; Moving a Selection up and down
(rc/create-keymap-m   "p"     'move-text-up)
(rc/create-keymap-m   "n"     'move-text-down)

;; Multi line Editing
(rc/create-keymap-m   "SPC"   'rectangle-mark-mode)
(rc/create-keymap-m   "e"     'mc/edit-lines)
(rc/create-keymap-c   "j"     'mc/mark-next-like-this)
(rc/create-keymap-c   "k"     'mc/mark-previous-like-this)

;; Zooming
(rc/create-keymap-c   "="     'rc/font-increase)
(rc/create-keymap-c   "+"     'rc/font-increase)
(rc/create-keymap-c   "-"     'rc/font-decrease)
(rc/create-keymap-m   "="     'global-text-scale-adjust)
(rc/create-keymap-m   "+"     'global-text-scale-adjust)

;; Stuff i do not need daily, but i definitely want a shortcut for
(rc/create-keymap     "<f1>"  'rc/toggle-scratch-buffer)
(rc/create-keymap     "<f2>"  'rc/toggle-compilation-buffer)
(rc/create-keymap     "<f3>"  'rc/load-theme)
(rc/create-keymap     "<f4>"  'rc/open_config)
(rc/create-keymap     "<f5>"  'rc/toggle-mini-buffer-mode)
(rc/create-keymap     "<f6>"  'whitespace-mode)
(rc/create-keymap     "<f7>"  'display-line-numbers-mode)
(rc/create-keymap     "<f8>"  'which-key-mode)
