;; Default Font-Size Variables
(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;; Answer with y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; MacOS Keyboard
(setq default-input-method "MacOSX")
(setq mac-command-modifier 'meta
      mac-option-modifier nil
      mac-allow-anti-aliasing t
      mac-command-key-is-meta t)

;; Display Startup Time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

;; Change UI
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set Transparency
;; (set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font
(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Some own Keymaps
(global-set-key (kbd "M-r") 'compile)

;; 
(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

;; VIM MOVEMENT!!!
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-s") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; 
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Icons
(use-package all-the-icons)

;; Display Keymaps
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; 
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; 
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; 
(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

;; 
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; Man Pages for Emacs
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; 
(use-package hydra
  :defer t)

;; MacOS Dired Stuff
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; 
(defhydra hydra-dired (:exit t)
  "dired"
  ("w" (dired "~/Downloads") "Downloads")
  ("d" (dired "~/Documents") "Documents"))
(global-set-key (kbd "C-c C-d") 'hydra-dired/body)

;; 
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; 
(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; LSP
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; LSP Mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; LSP UI
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; LSP Filetree
(use-package lsp-treemacs
  :after lsp)

;; 
(use-package lsp-ivy
  :after lsp)

;; TS Mode
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; C Mode
(add-hook 'c-mode-hook 'lsp)

;; C++ Mode
(add-hook 'c++-mode-hook 'lsp)

;; Python Mode
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
(setq lsp-pyls-server-command "/opt/homebrew/Caskroom/miniforge/base/bin/pylsp")
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;;
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;
(use-package company-box
  :hook (company-mode . company-box-mode))

;; 
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; 
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Git Client
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Terminal
(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc

  ;; Shell Prompt 
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;; Terminal Colors
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; Emacs Shell
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))
(use-package eshell-git-prompt
  :after eshell)
(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;; File Explorer
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single
  :commands (dired dired-jump))
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "30dc9873c16a0efb187bb3f8687c16aae46b86ddc34881b7cae5273e56b97580" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(package-selected-packages
   '(json-mode flycheck dired-hide-dotfiles dired-open all-the-icons-dired dired-single eshell-git-prompt vterm eterm-256color rainbow-delimiters evil-nerd-commenter forge magit counsel-projectile projectile company-box company pyvenv python-mode typescript-mode lsp-ivy lsp-treemacs lsp-ui lsp-mode visual-fill-column which-key use-package no-littering ivy-rich ivy-prescient hydra helpful general evil-collection doom-themes counsel command-log-mode auto-package-update all-the-icons))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
