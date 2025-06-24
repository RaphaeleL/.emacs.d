;; ==================================================
;; ===== KEYMAPS ====================================
;; ==================================================

;; Selecting Stuff
(lr/create-keymap-m   "w"     'mark-word)       ;; Default is M-@
(lr/create-keymap-m   "a"     'mark-page)       ;; Default is C-x C-p
(lr/create-keymap-m   "F"     'mark-defun)      ;; Default is C-M-h
(lr/create-keymap-m   "s"     'mark-paragraph)  ;; Default is M-h

;; Line Handling
(lr/create-keymap-ccc "j"     'join-line)
(lr/create-keymap-m   "z"     'undo)
(lr/create-keymap-m   "d"     'lr/duplicate-line)
(lr/create-keymap-m   "r"     'lr/delete-line)
(lr/create-keymap-cc  "g"     'indent-region)

(lr/create-keymap-ccc "k"     'consult-line)
(lr/create-keymap-ccc "g"     'consult-ripgrep)
(lr/create-keymap-c   "r"     'consult-recent-file)

;; Default Emacs stuff, just simpler
(lr/create-keymap-c   ","     'find-file)
(lr/create-keymap-c   "."     'dired)
(lr/create-keymap-m   ","     'project-find-file)
(lr/create-keymap-m   "g"     'magit-status)

;; Shell Commands
(lr/create-keymap-m   "c"     'compile)
(lr/create-keymap-c   "l"     'shell-command)
(lr/create-keymap-m   "q"     'kill-compilation)

;; Buffer Management
(lr/create-keymap-m   "i"     'ibuffer)
(lr/create-keymap-m   "o"     'consult-buffer)

;; Copy and Paste
(lr/create-keymap-c   "y"     'lr/paste)
(lr/create-keymap-c   "w"     'lr/copy)
(lr/create-keymap-c   "t"     'lr/cut)
(lr/create-keymap-c   "/"     'lr/visual-or-line-copy)

;; Moving a Selection up and down
(lr/create-keymap-m   "p"     'move-text-up)
(lr/create-keymap-m   "n"     'move-text-down)

;; Multi line Editing
(lr/create-keymap-m   "SPC"   'rectangle-mark-mode)
(lr/create-keymap-m   "e"     'mc/edit-lines)
(lr/create-keymap-c   "j"     'mc/mark-next-like-this)
(lr/create-keymap-c   "k"     'mc/mark-previous-like-this)

;; Zooming
(lr/create-keymap-c   "="     'lr/font-increase)
(lr/create-keymap-c   "+"     'lr/font-increase)
(lr/create-keymap-c   "-"     'lr/font-decrease)
(lr/create-keymap-m   "="     'global-text-scale-adjust)
(lr/create-keymap-m   "+"     'global-text-scale-adjust)

;; Stuff i do not need daily, but i definitely want a shortcut for
(lr/create-keymap     "<f1>"  'lr/toggle-scratch-buffer)
(lr/create-keymap     "<f2>"  'lr/toggle-compilation-buffer)
(lr/create-keymap     "<f3>"  'lr/load-theme)
(lr/create-keymap     "<f4>"  'lr/open_config)
(lr/create-keymap     "<f5>"  'lr/toggle-mini-buffer-mode)
(lr/create-keymap     "<f6>"  'whitespace-mode)
(lr/create-keymap     "<f7>"  'display-line-numbers-mode)
(lr/create-keymap     "<f8>"  'which-key-mode)
(lr/create-keymap     "<f9>"  'embark-bindings)
