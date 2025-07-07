; ==================================================
; ===== EMACS CONFIG ===============================
; ==================================================

; A more minimal 'kickstart like' config can be found under

; $ wget https://gist.githubusercontent.com/RaphaeleL/36c1a3de3526f1e8e1e3719900c320fb/raw/d7fa8380ed242e43dabf061cb320afdf5e1061c6/init.el
; $ emacs -q -l ~/path/to/gist/init.el

;; === PLUGIN MANAGER ============================
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; === BASIC AND DEFAULT CONFIGURATION ===========
(load "~/.emacs.d/_base/functions.el")  ;; a collection of useful functions
(load "~/.emacs.d/_base/packages.el")   ;; all packages with their keymaps
(load "~/.emacs.d/_base/garbage.el")    ;; performance and garbage handling
(load "~/.emacs.d/_base/basic.el")      ;; basic settings for my emacs
(load "~/.emacs.d/_base/hooks.el")      ;; auto call functions and modes

;; === CONFIGURATION BASED ON THE OS =============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/windows.el"))
 ((eq system-type 'darwin) (load "~/.emacs.d/_templates/macos.el"))
 ((eq system-type 'gnu/linux) (load "~/.emacs.d/_templates/linux.el")))

;; === NO JUNK PLEASE ============================
(setq custom-file "~/.emacs.d/output.el")
(load custom-file)
