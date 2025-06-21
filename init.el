;;; init.el -*- lexical-binding: t; -*-
;; This file controls what modules are enabled and what order they load in.
;; Remember to run 'emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 t)" after modifying it!

;; === CONFIGURATION DIRECTORY SETUP =============
(defun lira-setup-config-directory ()
  "Set up the user configuration directory if it doesn't exist.
Copies default configuration from ~/.emacs.d/templates/lira/ to ~/.config/lira/."
  (let ((default-config-dir "~/.emacs.d/templates/lira")
        (user-config-dir "~/.config/lira"))
    (unless (file-exists-p user-config-dir)
      (message "Setting up user configuration directory...")
      (condition-case err
          (progn
            ;; Create the directory
            (make-directory user-config-dir t)
            ;; Copy all .el files from default config
            (dolist (file (directory-files default-config-dir t "\\.el$"))
              (let ((target-file (expand-file-name (file-name-nondirectory file) user-config-dir)))
                (unless (file-exists-p target-file)
                  (copy-file file target-file t)
                  (message "Copied %s to %s" (file-name-nondirectory file) user-config-dir))))
            (message "User configuration directory setup complete"))
        (error
         (message "Warning: Could not set up configuration directory: %s" 
                  (error-message-string err)))))))

;; Run the setup
(lira-setup-config-directory)

;; === CORE CONFIGURATION ========================
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; === MODULE SYSTEM =============================
(defvar lira-modules '()
  "List of enabled modules.")

(defmacro lira! (name &rest flags)
  "Enable a module with optional flags."
  (let ((module (intern (format "lira-module-%s" name))))
    (add-to-list 'lira-modules module)
    `(progn
       (defvar ,module t)
       ,@(mapcar (lambda (flag)
                   (when (symbolp flag)
                     `(defvar ,(intern (format "%s-%s" module (symbol-name flag))) t)))
                 flags))))

;; === MODULE DEFINITIONS ========================
;; Safely load the user's module definitions
(condition-case err
    (load "~/.config/lira/init.el")
  (error
   (message "Warning: Could not load ~/.config/lira/init.el: %s" (error-message-string err))
   ;; No fallback modules - only load what user explicitly defines
   ))

;; === LOAD MODULES ==============================
;; Load utilities module first (provides helper functions)
(condition-case err
    (load "~/.emacs.d/modules/utilities")
  (error
   (message "Warning: Could not load utilities module: %s" (error-message-string err))))

;; Load all other modules except keymaps
(dolist (module lira-modules)
  (when (and (boundp module) 
             (not (string= (symbol-name module) "lira-module-keymaps")))
    (condition-case err
        (load (format "~/.emacs.d/modules/%s" (substring (symbol-name module) 12)))
      (error
       (message "Warning: Could not load module %s: %s" 
                (substring (symbol-name module) 12) 
                (error-message-string err))))))

;; Load user keymaps from ~/.config/lira/keymaps.el
(condition-case err
    (load "~/.config/lira/keymaps.el")
  (error
   (message "Warning: Could not load ~/.config/lira/keymaps.el: %s" (error-message-string err))))

;; === CUSTOM USER SETTINGS ======================
;; Load custom variables
(condition-case err
    (load "~/.emacs.d/custom/custom-vars.el")
  (error
   (message "Warning: Could not load custom-vars.el: %s" (error-message-string err))))

;; Load user custom settings
(condition-case err
    (load "~/.config/lira/custom.el")
  (error
   (message "Warning: Could not load ~/.config/lira/custom.el: %s" (error-message-string err))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
