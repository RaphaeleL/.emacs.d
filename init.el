;; === Plugin Manager ==============================
(require 'package)                                     ;; load Emacs' built-in package manager
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; workaround for TLS1.3 endpoint issues
(setq package-archives                                 ;; define packages location
      '(("melpa" . "https://melpa.org/packages/")      ;; -> MELPA for community packages
        ("gnu"   . "https://elpa.gnu.org/packages/"))) ;; -> GNU ELPA for official GNU packages
(package-initialize)                                   ;; init pkg system and activate installed pkgs
(unless package-archive-contents                       ;; if package list is empty
  (package-refresh-contents))                          ;; -> fetch the latest pkg index from archives

;; === Load Basic and Default Configuration ========
(load "~/.emacs.d/base/functions.el" 'noerror 'nomessage)  ;; a collection of useful functions
(load "~/.emacs.d/base/packages.el"  'noerror 'nomessage)  ;; all packages with their keymaps, settings
(load "~/.emacs.d/base/basic.el"     'noerror 'nomessage)  ;; basic settings for ui, saving, etc.
(load "~/.emacs.d/base/hooks.el"     'noerror 'nomessage)  ;; auto call some functions and modes

;; === Configuration based on the OS ===============
(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/templates/windows.el" 'noerror 'nomessage))   ;; Windows
 ((eq system-type 'darwin)     (load "~/.emacs.d/templates/macos.el"   'noerror 'nomessage))   ;; MacOS
 ((eq system-type 'gnu/linux)  (load "~/.emacs.d/templates/linux.el"   'noerror 'nomessage)))  ;; Linux

;; === Look and Feel of Emacs ======================
(defvar lr/switch t)                    ;; switch for a modern and old look
(if lr/switch (lr/modern) (lr/legacy))  ;; initial look and feel on load

;; === Custom File Location ========================
(setq custom-file "~/.emacs.d/output.el") ;; Save the Output Junk into a seperate file
(load custom-file)                        ;; -> custom-set-variables, custom-set-faces
