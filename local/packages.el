(load "~/.emacs.d/local/rc.el")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 27)
  (package-initialize))

;; Require Packages
(rc/require 'simpleclip)
(rc/require 'multiple-cursors)
(rc/require 'move-text)
(rc/require 'magit)
(rc/require 'rainbow-mode)
(rc/require 'smex)
(rc/require 'mood-line)
(rc/require 'doom-themes)

;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  (load-prefer-newer t)
  (auto-revert-use-notify nil)
  (auto-revert-interval 3)
  :config
  (global-auto-revert-mode t)
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda ()
                  (local-set-key (kbd "<mouse-2>") #'dired-find-alternate-file)
                  (local-set-key (kbd "RET") #'dired-find-alternate-file)
                  (local-set-key (kbd "^")
                                 (lambda () (interactive) (find-alternate-file ".."))))))

;; IBuffer
(use-package ibuffer
  :ensure t
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

;; Smex
(global-set-key (kbd "M-x") 'smex)
 (global-set-key (kbd "M-X") 'smex-major-mode-commands)
 (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
