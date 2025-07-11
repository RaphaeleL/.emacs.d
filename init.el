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
(load "~/.emacs.d/_base/packages.el")   ;; all packages with their keymaps, settings
(load "~/.emacs.d/_base/garbage.el")    ;; performance impr. and garbage collection
(load "~/.emacs.d/_base/basic.el")      ;; basic settings for ui, saving, etc.
(load "~/.emacs.d/_base/hooks.el")      ;; auto call some functions and modes

;; === CONFIGURATION BASED ON THE OS =============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/windows.el")) ;; Windows
 ((eq system-type 'darwin)     (load "~/.emacs.d/_templates/macos.el"))   ;; MacOS
 ((eq system-type 'gnu/linux)  (load "~/.emacs.d/_templates/linux.el")))  ;; Linux

;; === NO JUNK PLEASE ============================
(setq custom-file "~/.emacs.d/output.el") ;; Save the Output Junk into a seperate file
(load custom-file)                        ;; -> custom-set-variables, custom-set-faces
