; ==================================================
; ===== PACKAGES ===================================
; ==================================================

; === Essential Editing & Programming Tools ===

(lr/require 'simpleclip)        ;; Integrates Emacs kill-ring with system clipboard
(lr/require 'move-text)         ;; Move lines or regions up/down with ease
(lr/require 'multiple-cursors)  ;; Edit multiple lines or places at once (like in Sublime)
(lr/require 'magit)             ;; Full-featured Git interface inside Emacs
(lr/require 'eglot)             ;; Lightweight LSP client for IDE-like code features
; (lr/require 'paredit)           ;; Structured editing of Lisp code (maintains parentheses)

; === Optional UI Enhancements ===

(lr/require 'mood-line)         ;; Minimal and pretty modeline replacement
(lr/require 'ansi-color)        ;; Display ANSI colors in compilation or shell buffers
(lr/require 'which-key)         ;; Show possible keybindings after a prefix is typed
; (lr/require 'smooth-scrolling)  ;; Make Emacs scroll smoothly

; === Completion Frameworks (minibuffer/UI) ===

(lr/require 'vertico)           ;; Vertical minibuffer completion UI (replaces default M-x and more)
(lr/require 'orderless)         ;; Completion style allowing out-of-order, fuzzy-like matching
(lr/require 'marginalia)        ;; Annotations next to minibuffer candidates (e.g. describe functions)
(lr/require 'corfu)             ;; Popup-style in-buffer completion UI (replaces company)
(lr/require 'consult)           ;; Powerful completion commands for files, buffers, search, etc.
; (lr/require 'company)           ;; In-buffer completion popup (mostly replaced by corfu if used)
; (lr/require 'counsel)           ;; Ivy-based commands (e.g. counsel-M-x), not needed if using Vertico/Consult

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(lr/require 'markdown-mode)     ;; Major mode for editing Markdown files
(lr/require 'dockerfile-mode)   ;; Syntax highlighting for Dockerfiles
(lr/require 'jenkinsfile-mode)  ;; Major mode for Jenkins pipeline scripts
(lr/require 'web-mode)          ;; General mode for HTML, JS, templates, etc.
(lr/require 'yaml-mode)         ;; Major mode for YAML files
(lr/require 'jinja2-mode)       ;; Support for Jinja2 templates (used in Ansible, etc.)
(lr/require 'go-mode)           ;; Major mode for Go programming language
(lr/require 'rust-mode)         ;; Major mode for Rust programming language

; === History / Recent Files ===

(lr/require 'savehist)          ;; Save minibuffer history across Emacs sessions
(lr/require 'recentf)           ;; Track recently opened files and offer easy reopening
