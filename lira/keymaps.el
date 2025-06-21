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
