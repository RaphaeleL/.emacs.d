(load "~/.emacs.d/local/rc.el")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 27)
  (package-initialize))

;; Require Packages
(rc/require 'simpleclip)
(rc/require 'multiple-cursors)
(rc/require 'move-text)
(rc/require 'magit)

(rc/require 'vertico)
(rc/require 'marginalia)
(rc/require 'counsel)
(rc/require 'consult)
(rc/require 'orderless)

(rc/require 'mood-line)
(rc/require 'web-mode)
(rc/require 'flycheck)
(rc/require 'modus-themes)
(rc/require 'spacious-padding)
