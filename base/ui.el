(rc/require 'mood-line)
(mood-line-mode 1)

(set-fringe-mode 0)

(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-18")
   ((eq system-type 'gnu/linux) "Iosevka-12")))

(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

(setq custom-safe-themes 1) 

(set-frame-height (selected-frame) 35)
(set-frame-width (selected-frame) 120)

