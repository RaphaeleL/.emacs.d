;; Initialisiert Paketverwaltung
(require 'package)

;; Einstellungen
(setq user-full-name "Raphaele Salvatore Licciardo, B.Sc."
      user-mail-address "raphaele.salvatore@outlook.de"
      display-line-numbers-type 'relative ;; Relative Zeilen Nummber
      global-display-line-numbers-mode t  ;; Zeilen Nummern
      mac-command-modifier 'meta ;; Deutsche MacOS Tastatur
      mac-option-modifier 'none
      default-input-method "MacOSX"
      make-backup-files nil ;; Verhindert das Erstellen von Backup-Dateien
      auto-save-default nil ;; Verhindert das Erstellen von automatisch gespeicherten Dateien
      package-enable-at-startup nil ;; Paketverwaltung
      ring-bell-function 'ignore ;; Keine Klingel mehr
)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Aktualisiert die Paketdatenbank, falls sie noch nicht vorhanden ist
(unless package-archive-contents
  (package-refresh-contents))

;; Installiert use-package, falls es noch nicht vorhanden ist
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Benutzen Sie Paketverwaltung für alle weiteren Pakete
(require 'use-package)
(setq use-package-always-ensure t) ;; Automatisches Herunterladen und Installieren fehlender Pakete

;; File Explorer
(if (executable-find "gls")
    (setq insert-directory-program "gls"))
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

;; Lädt und konfiguriert evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  )

;; Git Client verwenden
(use-package magit
  :ensure t)

;; Automatischen schließen von Klammern und co.
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode))

;; Themes
(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil) ; this prevents a weird bug with doom themes
  ;;:init (load-theme 'doom-one t)
  )

;; yes / no Fragen abkürzen 
(fset 'yes-or-no-p 'y-or-n-p)

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :hook ((python-mode c-mode c++-mode) . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp))))  ; or lsp-deferred

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

;; Shortcuts
(defun increase-font-size ()
  (interactive)
  (text-scale-increase 1))

(defun decrease-font-size ()
  (interactive)
  (text-scale-decrease 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements "SPC w" "Window")
  (which-key-add-key-based-replacements "SPC b" "Buffer")
  (which-key-add-key-based-replacements "SPC w r" "Enlarge horiz. split")
  (which-key-add-key-based-replacements "SPC w l" "Shrink horiz. split")
  (which-key-add-key-based-replacements "SPC w u" "Enlarge vert. split")
  (which-key-add-key-based-replacements "SPC w d" "Shrink vert horizontally"))

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :states '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  (my-leader-def
    "." 'dired
    "b i" 'buffer-menu
    "b b" 'switch-to-buffer
    "b p" 'previous-buffer
    "b n" 'next-buffer
    "b k" 'kill-this-buffer
    "b s" 'save-buffer
    "w v" 'split-window-right
    "w s" 'split-window-below
    "w w" 'other-window
    "w q" 'delete-window
    "w +" 'enlarge-window
    "w -" 'shrink-window
    "w r" 'enlarge-window-horizontally
    "w l" 'shrink-window-horizontally
    "w u" 'enlarge-window-horizontally
    "w d" 'shrink-window-horizontally
))

(global-set-key (kbd "M-+") 'increase-font-size)
(global-set-key (kbd "M--") 'decrease-font-size)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes smartparens magit use-package lsp-ui lsp-python-ms lsp-java lsp-ivy helm-lsp evil ccls)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
