; ==================================================
; ===== EMACS CONFIG ===============================
; ==================================================

;; === PLUGIN MANAGER ==============================
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; === BASIC AND DEFAULT CONFIGURATION =============
(load "~/.emacs.d/_base/functions.el" 'noerror 'nomessage)  ;; a collection of useful functions
(load "~/.emacs.d/_base/packages.el"  'noerror 'nomessage)  ;; all packages with their keymaps, settings
(load "~/.emacs.d/_base/garbage.el"   'noerror 'nomessage)  ;; performance impr. and garbage collection
(load "~/.emacs.d/_base/basic.el"     'noerror 'nomessage)  ;; basic settings for ui, saving, etc.
(load "~/.emacs.d/_base/hooks.el"     'noerror 'nomessage)  ;; auto call some functions and modes

;; === CONFIGURATION BASED ON THE OS ===============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/windows.el" 'noerror 'nomessage))   ;; Windows
 ((eq system-type 'darwin)     (load "~/.emacs.d/_templates/macos.el"   'noerror 'nomessage))	;; MacOS
 ((eq system-type 'gnu/linux)  (load "~/.emacs.d/_templates/linux.el"   'noerror 'nomessage)))  ;; Linux

;; === CUSTOM USER CONFIGURATION ===================
(defvar my-custom-file "~/.emacs.d/_custom.el")   ;; Custom User Settings on top of the current config
(unless (file-exists-p my-custom-file)            ;; Create the file if it doesn't exist
  (with-temp-buffer (write-file my-custom-file)))
  (load my-custom-file 'noerror 'nomessage)       ;; Load it safely

;; === CUSTOM SET PLACE ============================
(setq custom-file "~/.emacs.d/output.el") ;; Save the Output Junk into a seperate file
(load custom-file)                        ;; -> custom-set-variables, custom-set-faces

; ==================================================
; ===== EMACS KICKSTART ============================
; ==================================================

; For a more minimal 'kickstart like' config use following:

; $ wget https://gist.githubusercontent.com/RaphaeleL/36c1a3de3526f1e8e1e3719900c320fb/raw/d7fa8380ed242e43dabf061cb320afdf5e1061c6/init.el
; $ emacs -q -l ~/path/to/gist/init.el

; Alternatively there is also the option of using (lr/legacy) in the current emacs config.
