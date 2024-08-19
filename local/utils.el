;; Default Font
(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "SF Mono-14")
   ((eq system-type 'gnu/linux) "Iosevka-12")))

;; (set-face-attribute 'default nil :font "Iosevka-16")
;; (set-face-attribute 'default nil :font "SF Mono-12")
;; (set-face-attribute 'default nil :font "SF Mono-16" :width 'semi-condensed)

;; Deutsche Mac Tastatur
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

;; Magit in Fullscreen
(defun my-magit-status ()
  "Don't split window."
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))

;; Enable mouse-support.
(setq eat-enable-mouse t)

;; Which Key
(which-key-mode 1)

;; Completion
(vertico-mode 1)
(marginalia-mode 1)
(counsel-mode 1)
