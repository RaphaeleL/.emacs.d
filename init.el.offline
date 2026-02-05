;;; init.el --- Offline, deterministic Emacs init -*- lexical-binding: t -*-

;; --------------------------------------------------
;; 1. Core paths
;; --------------------------------------------------

(defvar emacs-dir (file-name-as-directory user-emacs-directory))
(defvar base-dir  (expand-file-name "_base/" emacs-dir))
(defvar modes-dir (expand-file-name "_modes/" emacs-dir))
(defvar site-dir  (expand-file-name "site-lisp/" emacs-dir))

(add-to-list 'load-path base-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path site-dir)

;; --------------------------------------------------
;; 2. Package system (offline, passive)
;; --------------------------------------------------

(require 'package)

(setq package-archives nil
      package-check-signature nil
      package-enable-at-startup nil)

(package-initialize)

;; Optional: harden load-path (audit-friendly)
; (setq load-path
;       (seq-filter
;        (lambda (p)
;          (or (string-prefix-p emacs-dir p)
;              (string-prefix-p "/usr/share/emacs" p)))
;        load-path))

;; --------------------------------------------------
;; 3. Core configuration (NO packages here)
;; --------------------------------------------------

(load (expand-file-name "functions.el" base-dir) 'noerror)
(load (expand-file-name "garbage.el"   base-dir) 'noerror)
(load (expand-file-name "basic.el"     base-dir) 'noerror)
(load (expand-file-name "hooks.el"     base-dir) 'noerror)

;; --------------------------------------------------
;; 4. Package-dependent configuration
;; --------------------------------------------------

(load (expand-file-name "magit.el"        modes-dir) 'noerror)
(load (expand-file-name "org.el"          modes-dir) 'noerror)
(load (expand-file-name "programming.el" modes-dir) 'noerror)

;; --------------------------------------------------
;; 5. OS-specific configuration
;; --------------------------------------------------

(cond
 ((eq system-type 'gnu/linux)
  (load (expand-file-name "linux.el"   (expand-file-name "_templates/" emacs-dir)) 'noerror))
 ((eq system-type 'darwin)
  (load (expand-file-name "macos.el"   (expand-file-name "_templates/" emacs-dir)) 'noerror))
 ((eq system-type 'windows-nt)
  (load (expand-file-name "windows.el" (expand-file-name "_templates/" emacs-dir)) 'noerror)))

;; --------------------------------------------------
;; 6. Custom file
;; --------------------------------------------------

(setq custom-file (expand-file-name "output.el" emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;;; init.el ends here
