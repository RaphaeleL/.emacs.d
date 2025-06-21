;;; os.el -*- lexical-binding: t; -*-
;; Operating system specific configurations

;; === MACOS =====================================
(when (boundp 'lira-module-os-macos)
  ;; Key remapping
  (setq mac-command-modifier 'meta       ; Command as Meta
        mac-option-modifier 'control     ; Option as Control
        mac-control-modifier 'control    ; Ensure Control is Control
        default-input-method "MacOSX")

  ;; Use GNU ls from coreutils (via Homebrew)
  (setq insert-directory-program "/opt/homebrew/bin/gls")

  ;; Homebrew
  (add-to-list 'exec-path "/opt/homebrew/bin")

  ;; Fix MacOS key mappings for special characters
  ;; These mappings ensure Option+key combinations work properly
  (when (eq system-type 'darwin)
    ;; Option+8 = {, Option+9 = }, Option+[ = [, Option+] = ]
    (global-set-key (kbd "M-8") (lambda () (interactive) (insert "{")))
    (global-set-key (kbd "M-9") (lambda () (interactive) (insert "}")))
    (global-set-key (kbd "M-[") (lambda () (interactive) (insert "[")))
    (global-set-key (kbd "M-]") (lambda () (interactive) (insert "]")))
    
    ;; Additional common MacOS key mappings
    (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))
    (global-set-key (kbd "M-4") (lambda () (interactive) (insert "$")))
    (global-set-key (kbd "M-7") (lambda () (interactive) (insert "&")))
    (global-set-key (kbd "M-=") (lambda () (interactive) (insert "+")))
    (global-set-key (kbd "M--") (lambda () (interactive) (insert "_")))
    (global-set-key (kbd "M-`") (lambda () (interactive) (insert "~")))
    (global-set-key (kbd "M-1") (lambda () (interactive) (insert "!")))
    (global-set-key (kbd "M-2") (lambda () (interactive) (insert "@")))
    (global-set-key (kbd "M-5") (lambda () (interactive) (insert "%")))
    (global-set-key (kbd "M-6") (lambda () (interactive) (insert "^")))
    (global-set-key (kbd "M-0") (lambda () (interactive) (insert ")")))
    (global-set-key (kbd "M-;") (lambda () (interactive) (insert ":")))
    (global-set-key (kbd "M-'") (lambda () (interactive) (insert "\"")))
    (global-set-key (kbd "M-,") (lambda () (interactive) (insert "<")))
    (global-set-key (kbd "M-.") (lambda () (interactive) (insert ">")))
    (global-set-key (kbd "M-/") (lambda () (interactive) (insert "?")))
    (global-set-key (kbd "M-\\") (lambda () (interactive) (insert "|")))))

;; === LINUX =====================================
(when (boundp 'lira-module-os-linux)
  ;; Linux-specific settings
  (setq insert-directory-program "ls"))

;; === WINDOWS ===================================
(when (boundp 'lira-module-os-windows)
  ;; Windows-specific settings
  (setq w32-get-true-file-attributes nil))

;; === CUSTOM KEYBINDINGS ========================
;; Fallback keybindings for all platforms (C-c combinations)
(global-set-key (kbd "C-c 8") (lambda () (interactive) (insert "{")))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (insert "}")))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "[")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "]")))

;; === FONT SETTINGS =============================
(defun lira-get-default-font-family ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka")
   ((eq system-type 'darwin)     "Iosevka")
   ((eq system-type 'gnu/linux)  "IosevkaNerdFont")))

(defun lira-get-default-font-size ()
  (cond
   ((eq system-type 'windows-nt) 16)
   ((eq system-type 'darwin)     20)
   ((eq system-type 'gnu/linux)  10)))

(defun lira-get-default-font ()
  (format "%s-%d" (lira-get-default-font-family) (lira-get-default-font-size)))

(defvar lira-original-font (face-attribute 'default :font))

(defun lira-enable-custom-font ()
  (interactive)
  (let ((font (lira-get-default-font)))
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-frame-font font t t)))

(defun lira-disable-custom-font ()
  (interactive)
  (setq default-frame-alist
        (cl-remove-if (lambda (entry)
                        (eq (car entry) 'font))
                      default-frame-alist))
  (set-frame-font lira-original-font t t)) 