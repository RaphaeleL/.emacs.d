;; Default Font
(defun get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-12")
   ((eq system-type 'darwin) "Iosevka-14")
   ((eq system-type 'gnu/linux) "Iosevka-12")))

;; Deutsche Mac Tastatur
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

