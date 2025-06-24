;; === PLUGIN MANAGER ============================
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; === BASIC AND DEFAULT CONFIGURATION ===========
(load "~/.emacs.d/_base/functions.el")
(load "~/.emacs.d/_base/packages.el")
(load "~/.emacs.d/_base/garbage.el")
(load "~/.emacs.d/_base/basic.el")
(load "~/.emacs.d/_base/keymaps.el")
(load "~/.emacs.d/_base/custom.el")
(load "~/.emacs.d/_base/hooks.el")

;; === CONFIGURATION BASED ON THE OS =============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/windows.el"))
 ((eq system-type 'darwin) (load "~/.emacs.d/_templates/macos.el"))
 ((eq system-type 'gnu/linux) (load "~/.emacs.d/_templates/linux.el")))
