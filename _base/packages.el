; ==================================================
; ===== PACKAGES ===================================
; ==================================================

; === Essential Editing & Programming Tools ===

(rc/require 'simpleclip)        ;; Integrates Emacs kill-ring with system clipboard
(rc/require 'move-text)         ;; Move lines or regions up/down with ease
(rc/require 'multiple-cursors)  ;; Edit multiple lines or places at once (like in Sublime)
(rc/require 'paredit)           ;; Structured editing of Lisp code (maintains parentheses)
(rc/require 'magit)             ;; Full-featured Git interface inside Emacs
(rc/require 'eglot)             ;; Lightweight LSP client for IDE-like code features
(rc/require 'company)           ;; In-buffer completion popup (mostly replaced by corfu if used)

; === Optional UI Enhancements ===

(rc/require 'mood-line)         ;; Minimal and pretty modeline replacement
(rc/require 'ansi-color)        ;; Display ANSI colors in compilation or shell buffers
(rc/require 'which-key)         ;; Show possible keybindings after a prefix is typed
(rc/require 'smooth-scrolling)  ;; Make Emacs scroll smoothly 

; === Completion Frameworks (minibuffer/UI) ===

(rc/require 'vertico)           ;; Vertical minibuffer completion UI (replaces default M-x and more)
(rc/require 'orderless)         ;; Completion style allowing out-of-order, fuzzy-like matching
(rc/require 'marginalia)        ;; Annotations next to minibuffer candidates (e.g. describe functions)
(rc/require 'counsel)           ;; Ivy-based commands (e.g. counsel-M-x), not needed if using Vertico/Consult
(rc/require 'consult)           ;; Powerful completion commands for files, buffers, search, etc.
(rc/require 'corfu)             ;; Popup-style in-buffer completion UI (replaces company)

; === Language Modes (Syntax highlighting, indentation, etc.) ===

(rc/require 'markdown-mode)     ;; Major mode for editing Markdown files
(rc/require 'dockerfile-mode)   ;; Syntax highlighting for Dockerfiles
(rc/require 'jenkinsfile-mode)  ;; Major mode for Jenkins pipeline scripts
(rc/require 'web-mode)          ;; General mode for HTML, JS, templates, etc.
(rc/require 'yaml-mode)         ;; Major mode for YAML files
(rc/require 'jinja2-mode)       ;; Support for Jinja2 templates (used in Ansible, etc.)
(rc/require 'go-mode)           ;; Major mode for Go programming language
(rc/require 'rust-mode)         ;; Major mode for Rust programming language

; === History / Recent Files ===

(rc/require 'savehist)          ;; Save minibuffer history across Emacs sessions
(rc/require 'recentf)           ;; Track recently opened files and offer easy reopening

