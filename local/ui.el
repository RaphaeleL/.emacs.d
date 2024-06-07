;; No Startup Message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Cleanup the UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Theme
(load-theme 'gruber-darker 1)

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode 1)
)

;; Font
(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12") ;; IosevkaTermSlab NF-12
   ((eq system-type 'darwin) "Iosevka-14")
   ((eq system-type 'gnu/linux) "Iosevka-12")))

(add-to-list 'default-frame-alist `(font . ,(get-default-font)))

;; Ido Mode for Files
(ido-mode 1)
(ido-everywhere 1)

;; Package Manager
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Ido Mode for M-x
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
 (global-set-key (kbd "M-X") 'smex-major-mode-commands)
 (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Line Numbering
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Bigger Font
(set-face-attribute 'default nil :height 130)

;; Window Size
(when window-system (set-frame-size (selected-frame) 120 30))

;; Disable Backup and Autosave Settings
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ---------------------------------------------------------------------------------
;; -------- Tastatur ---------------------------------------------------------------
;; ---------------------------------------------------------------------------------

;; Deutsche Mac Tastatur
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))
