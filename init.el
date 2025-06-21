;;; init.el -*- lexical-binding: t; -*-
;; This file controls what modules are enabled and what order they load in.
;; Remember to run 'emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d\" 0 t)" after modifying it!

;; === STARTUP PROFILING ========================
(defvar lira-startup-times (make-hash-table :test 'equal)
  "Hash table to track startup times of different components.")

(defmacro lira-time (name &rest body)
  "Time the execution of BODY and store it under NAME."
  `(let ((start-time (current-time)))
     ,@body
     (puthash ,name (float-time (time-subtract (current-time) start-time))
              lira-startup-times)))

(defun lira-report-startup-times ()
  "Report startup times for all components."
  (when (and (boundp 'lira-startup-times) (> (hash-table-count lira-startup-times) 0))
    (message "=== Lira Startup Times ===")
    (let ((total-time 0))
      (maphash (lambda (name time)
                 (message "%-25s %.3fs" name time)
                 (setq total-time (+ total-time time)))
               lira-startup-times)
      (message "%-25s %.3fs" "TOTAL" total-time)
      (message "========================="))))

;; === CORE CONFIGURATION ========================
(lira-time "Core Setup"
  ;; Defer package initialization to after startup
  (setq package-enable-at-startup nil)
  
  ;; Aggressive startup optimizations
  (setq gc-cons-threshold (* 200 1024 1024))  ; 200MB during startup
  (setq gc-cons-percentage 0.8)
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  
  ;; Disable things we don't need during startup
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq inhibit-startup-echo-area-message t)
  (setq inhibit-default-init t)
  
  ;; Disable file name handler cache
  (setq file-name-handler-alist nil)
  
  ;; Disable auto-save during startup
  (setq auto-save-default nil)
  
  ;; Disable desktop save during startup
  (setq desktop-save-mode nil))

;; === CONFIGURATION DIRECTORY SETUP =============
(lira-time "Config Setup"
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
  (lira-setup-config-directory))

;; === MODULE SYSTEM =============================
(lira-time "Module System"
  (defvar lira-modules '()
    "List of enabled modules.")

  (defvar lira-lazy-modules '()
    "List of modules to load lazily.")

  (defvar lira-heavy-modules '()
    "List of heavy modules that should be loaded only when needed.")

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

  (defmacro lira-lazy! (name &rest flags)
    "Enable a module to be loaded lazily with optional flags."
    (let ((module (intern (format "lira-module-%s" name))))
      (add-to-list 'lira-lazy-modules module)
      `(lira! ,name ,@flags)))

  (defmacro lira-heavy! (name &rest flags)
    "Enable a heavy module that should be loaded only when needed."
    (let ((module (intern (format "lira-module-%s" name))))
      (add-to-list 'lira-heavy-modules module)
      `(lira-lazy! ,name ,@flags))))

;; === MODULE DEFINITIONS ========================
(lira-time "Module Definitions"
  ;; Safely load the user's module definitions
  (condition-case err
      (load "~/.config/lira/init.el")
    (error
     (message "Warning: Could not load ~/.config/lira/init.el: %s" (error-message-string err))
     ;; No fallback modules - only load what user explicitly defines
     )))

;; === PACKAGE INITIALIZATION ===================
(lira-time "Package Init"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; === LOAD CORE MODULES ========================
(lira-time "Core Modules"
  ;; Load utilities module first (provides helper functions)
  (condition-case err
      (load "~/.emacs.d/modules/utilities")
    (error
     (message "Warning: Could not load utilities module: %s" (error-message-string err))))

  ;; Load essential modules immediately (lightweight ones)
  (dolist (module lira-modules)
    (when (and (boundp module) 
               (not (member module lira-lazy-modules))
               (not (member module lira-heavy-modules))
               (not (string= (symbol-name module) "lira-module-keymaps")))
      (condition-case err
          (load (format "~/.emacs.d/modules/%s" (substring (symbol-name module) 12)))
        (error
         (message "Warning: Could not load module %s: %s" 
                  (substring (symbol-name module) 12) 
                  (error-message-string err)))))))

;; === LOAD USER CONFIGURATION ==================
(lira-time "User Config"
  ;; Load user keymaps from ~/.config/lira/keymaps.el
  (condition-case err
      (load "~/.config/lira/keymaps.el")
    (error
     (message "Warning: Could not load ~/.config/lira/keymaps.el: %s" (error-message-string err))))

  ;; Load custom variables
  (condition-case err
      (load "~/.emacs.d/custom/custom-vars.el")
    (error
     (message "Warning: Could not load custom-vars.el: %s" (error-message-string err))))

  ;; Load user custom settings
  (condition-case err
      (load "~/.config/lira/custom.el")
    (error
     (message "Warning: Could not load ~/.config/lira/custom.el: %s" (error-message-string err)))))

;; === LAZY LOADING SETUP =======================
(lira-time "Lazy Loading Setup"
  ;; Set up lazy loading for heavy modules
  (dolist (module lira-heavy-modules)
    (let ((module-name (substring (symbol-name module) 12)))
      ;; Load tools module when magit is used
      (when (string= module-name "tools")
        (eval `(with-eval-after-load 'magit
                 (condition-case err
                     (load "~/.emacs.d/modules/tools")
                   (error
                    (message "Warning: Could not lazy load tools module: %s" 
                             (error-message-string err)))))))
      
      ;; Load language modules when their modes are activated
      (when (string= module-name "lang")
        (eval `(dolist (mode '(rust-mode go-mode python-mode web-mode markdown-mode))
                 (with-eval-after-load mode
                   (condition-case err
                       (load "~/.emacs.d/modules/lang")
                     (error
                      (message "Warning: Could not lazy load lang module: %s" 
                               (error-message-string err))))))))))
  
  ;; Set up lazy loading for regular lazy modules
  (dolist (module lira-lazy-modules)
    (when (not (member module lira-heavy-modules))
      (let ((module-name (substring (symbol-name module) 12)))
        (eval `(with-eval-after-load ',module-name
                 (condition-case err
                     (load (format "~/.emacs.d/modules/%s" ,module-name))
                   (error
                    (message "Warning: Could not lazy load module %s: %s" 
                             ,module-name (error-message-string err))))))))))

;; === POST-STARTUP OPTIMIZATIONS ===============
(lira-time "Post-Startup"
  ;; Restore normal GC settings after startup
  (run-with-idle-timer 1 nil
    (lambda ()
      (setq gc-cons-threshold (* 16 1024 1024))  ; 16MB
      (setq gc-cons-percentage 0.1)
      (setq file-name-handler-alist lira--file-name-handler-alist)
      (setq auto-save-default t)
      (setq desktop-save-mode t)
      (garbage-collect)))
  
  ;; Report startup times
  (run-with-idle-timer 2 nil 'lira-report-startup-times))

;; === CUSTOM SETTINGS ==========================
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
