(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX"))

(when (eq system-type 'darwin)
  (setq insert-directory-program "/opt/homebrew/bin/gls"))
