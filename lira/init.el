;; === MODULE DEFINITIONS ========================
(lira! completion
       company                ; the ultimate code completion backend
       ; vertico                ; the search engine of the future
       ; orderless              ; fuzzy completion
       ; marginalia             ; rich annotations in the minibuffer
       fido)                  ; fallback completion

(lira! ui
       mood-line              ; snazzy modeline
       ; which-key              ; key binding hints
       hl-todo                ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       popup                  ; tame sudden yet inevitable temporary windows
       smooth-scroll          ; smooth scrolling
       line-numbers           ; enable line numbers (absolute)
       relative-line-numbers) ; use relative line numbers

(lira! editor
       multiple-cursors       ; editing in many places at once
       snippets               ; my elves. They type so I don't have to
       ; fold                   ; universal code folding
       ; file-templates         ; auto-snippets for empty files
       word-wrap)             ; soft wrapping with language-aware indent

(lira! emacs
       dired                  ; making dired pretty [functional]
       electric               ; smarter, keyword-based electric-indent
       undo                   ; persistent, smarter undo for your inevitable mistakes
       vc)                    ; version-control and Emacs, sitting in a tree

(lira! checkers
       syntax)                ; tasing you for every semicolon you forget

(lira! tools
       magit                  ; a git porcelain for Emacs
       lsp                    ; M-x vscode
       eval                   ; run code, run (also, repls)
       lookup)                ; navigate your code and its documentation

(lira! lang
       emacs-lisp             ; drown in parentheses
       markdown               ; writing docs for people to ignore
       web                    ; the tubes
       yaml                   ; JSON, but readable
       json                   ; At least it ain't XML
       go                     ; the hipster dialect
       rust                   ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                     ; she sells {ba,z,fi}sh shells on the C xor
       dockerfile             ; containerization
       jenkinsfile)           ; CI/CD

(lira! os
       macos)                 ; improve compatibility with macOS
