;;; krait-mode.el --- Major mode for the Krait programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Krait compiler repository
;;
;; Author: Krait compiler repository
;; Version: 1.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A major mode for Krait (*.krt) source files.  It provides font-lock
;; highlighting for the whole lexical surface (directives, keywords,
;; builtin and capitalized type names, function names, `#bake' constants,
;; strings, character literals, numbers), correct handling of nestable
;; `/* ... */' block comments (plus `//' line comments and `/** ... */'
;; documentation comments), imenu entries, and `beginning-of-defun' /
;; `end-of-defun' navigation.

;;; Code:

(require 'syntax)

(defgroup krait nil
  "Major mode for editing Krait code."
  :group 'languages
  :prefix "krait-")

(defvar krait-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Identifiers: [A-Za-z_][A-Za-z0-9_]*.
    (modify-syntax-entry ?_ "w" table)
    ;; `//' line comments ...
    (modify-syntax-entry ?/ ". 124b" table)
    ;; ... and `/* ... */' block comments.  Nesting is handled by
    ;; `krait-syntax-propertize' below.
    (modify-syntax-entry ?* ". 23" table)
    ;; A newline ends `//' comments (but not block comments).
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings and character literals, with backslash escapes.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; `#' introduces directives; it never starts a comment.
    (modify-syntax-entry ?# "." table)
    table)
  "Syntax table for `krait-mode'.")

(defun krait-syntax-propertize (start end)
  "Handle nested block comments between START and END for `krait-mode'.
The syntax table only supports flat `/* ... */' comments, so a nested `/*'
marks its `*' as a generic string delimiter, and the matching `*/' marks
its `*' the same way: the pair toggles a pseudo-string inside the comment,
and the enclosing comment stays open until its own `*/'."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; A nested `/*' inside a block comment (but not inside a string or a
    ;; `//' line comment).
    ("/\\(\\*\\)"
     (1 (let ((state (syntax-ppss (match-beginning 0))))
          (when (and (nth 4 state)                 ; inside a comment,
                     (not (nth 3 state))           ; not in a nested level,
                     (let ((cstart (nth 8 state))) ; and it is /*, not //
                       (and cstart
                            (eq (char-after cstart) ?/)
                            (eq (char-after (1+ cstart)) ?*))))
            (string-to-syntax "|")))))
    ;; The matching `*/' while inside a nested level.
    ("\\(\\*\\)/"
     (1 (let ((state (syntax-ppss (match-beginning 0))))
          (when (and (nth 4 state) (nth 3 state))
            (string-to-syntax "|"))))))
   start end)
  (krait-syntax-propertize-strings start end))

(defun krait-syntax-propertize-strings (start end)
  "Mark multi-line and raw strings between START and END.
`\"\"\"...\"\"\"' and raw `r\"...\"' / `r\"\"\"...\"\"\"' strings become generic
strings (fence `|' on the first and last quote) so the quotes inside them do
not end the string; inside raw strings, backslashes are plain punctuation."
  (goto-char start)
  (while (re-search-forward
          "\\(\\_<r\\)?\\(\"\"\"\\(?:.\\|\n\\)*?\"\"\"\\)\\|\\_<r\\(\"[^\"\n]*\"\\)" end t)
    (let* ((group (if (match-beginning 2) 2 3))
           (raw (or (match-beginning 1) (match-beginning 3)))
           (open (match-beginning group))
           (close (1- (match-end group))))
      (unless (nth 8 (save-excursion (syntax-ppss open))) ; not in a comment/string
        (put-text-property open (1+ open) 'syntax-table (string-to-syntax "|"))
        (put-text-property close (1+ close) 'syntax-table (string-to-syntax "|"))
        (save-excursion
          (goto-char (1+ open))
          (while (re-search-forward (if raw "[\"\\\\]" "\"") close t)
            (put-text-property (1- (point)) (point) 'syntax-table
                               (string-to-syntax "."))))))))

(defun krait-syntactic-face (state)
  "Return the font-lock face for the syntactic context STATE.
Comments win over strings so that text inside a nested block comment keeps
the comment face; `/** ... */' comments use `font-lock-doc-face'."
  (cond
   ((nth 4 state)
    (let ((cstart (nth 8 state)))
      (if (and cstart
               (eq (char-after cstart) ?/)
               (eq (char-after (1+ cstart)) ?*)
               (eq (char-after (+ cstart 2)) ?*)
               (not (eq (char-after (+ cstart 3)) ?/)))
          font-lock-doc-face
        font-lock-comment-face)))
   ((nth 3 state)
    font-lock-string-face)))

(defconst krait-mode--defun-regexp
  "^\\s-*\\(?:pub\\s-+\\)?\\(?:fn\\|struct\\|enum\\|#bake\\|#extern\\)\\_>"
  "Regexp matching the start of a top-level Krait definition.")

(defun krait-beginning-of-defun (&optional arg)
  "Move backward to the start of the current or previous Krait definition.
With ARG, do it that many times; negative ARG moves forward."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((search-fn (if (>= arg 0) #'re-search-backward #'re-search-forward))
        (count (abs arg)))
    (while (> count 0)
      (if (funcall search-fn krait-mode--defun-regexp nil t)
          (progn
            (goto-char (match-beginning 0))
            (setq count (1- count)))
        (goto-char (if (>= arg 0) (point-min) (point-max)))
        (setq count 0)))
    (beginning-of-line)))

(defun krait-end-of-defun (&optional arg)
  "Move forward to the start of the next Krait definition, ARG times."
  (interactive "^p")
  (setq arg (max 1 (or arg 1)))
  (while (> arg 0)
    (forward-line 1)
    (if (re-search-forward krait-mode--defun-regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (setq arg (1- arg)))
      (goto-char (point-max))
      (setq arg 0))))

(defconst krait-mode-font-lock-keywords
  (let* ((kw (regexp-opt '("and" "as" "break" "case" "continue" "default"
                           "defer" "deinit" "else" "enum" "fails" "fn" "for" "if"
                           "in" "match" "not" "or" "pub" "ref" "return"
                           "struct" "then" "to" "until" "while" "using")
                         'symbols))
         (reserved (regexp-opt '("async" "await" "class" "comptime" "impl"
                                 "module" "nil" "none" "some" "trait" "use"
                                 "yield" "ok" "error")
                               'symbols))
         (types (regexp-opt '("bool" "char" "byte" "short" "ushort" "int"
                              "uint" "long" "ulong" "i8" "i16" "i32" "i64"
                              "u8" "u16" "u32" "u64" "f32" "f64" "float"
                              "double" "string")
                            'symbols))
         (builtins (regexp-opt '("alloc" "free" "sizeof" "alignof" "offset" "error") 'symbols))
         ;; Capitalized identifiers that are not SCREAMING_CASE:
         ;; `Shape', `List', `Point2D', and single-letter type params `T'.
         (user-types "\\_<[[:upper:]]\\(?:\\|[[:alnum:]_]*[[:lower:]][[:alnum:]_]*\\)\\_>")
         ;; SCREAMING_CASE names: by convention `#bake' constants.
         (constants "\\_<[[:upper:]][[:upper:]_0-9]\\{1,\\}\\_>")
         (number-face (if (facep 'font-lock-number-face)
                          'font-lock-number-face
                        'font-lock-constant-face)))
    `(;; Directives: #import, #load, #library, #assert, #all, #bake, #copy, #on, #extern, #char, #repeat, #type.
      ("#\\(?:import\\|load\\|library\\|assert\\|all\\|bake\\|copy\\|on\\|extern\\|char\\|repeat\\|type\\)\\_>" 0 font-lock-preprocessor-face)
      ;; `#bake NAME' — compile-time constants.
      ("#bake\\s-+\\([[:word:]]+\\)" 1 font-lock-constant-face)
      ;; `fn name'.
      ("\\_<fn\\s-+\\([[:word:]]+\\)" 1 font-lock-function-name-face)
      ;; Booleans.
      ("\\_<\\(?:true\\|false\\)\\_>" 0 font-lock-constant-face)
      (,constants 0 font-lock-constant-face)
      (,user-types 0 font-lock-type-face)
      (,types 0 font-lock-type-face)
      (,builtins 0 font-lock-builtin-face)
      (,reserved 0 font-lock-keyword-face)
      (,kw 0 font-lock-keyword-face)
      ;; Numbers.
      ("\\_<0[xX][[:xdigit:]_]+\\_>" 0 ',number-face)
      ("\\_<0[bB][01_]+\\_>" 0 ',number-face)
      ("\\_<0[oO][0-7_]+\\_>" 0 ',number-face)
      ("\\_<[[:digit:]][[:digit:]_]*\\(?:\\.[[:digit:]_]+\\)?\\(?:[eE][+-]?[[:digit:]]+\\)?\\_>" 0 ',number-face)))
  "Font-lock rules for `krait-mode'.")

(defun krait--previous-non-empty-line ()
  (save-excursion
    (move-beginning-of-line nil)
    (if (bobp) nil
      (forward-line -1)
      (while (and (not (bobp)) (string-empty-p (string-trim-right (thing-at-point 'line t)))) (forward-line -1))
      (if (string-empty-p (string-trim-right (thing-at-point 'line t))) nil
        (cons (thing-at-point 'line t) (current-indentation))))))

(defun krait--desired-indentation ()
  (let ((prev (krait--previous-non-empty-line)))
    (if (not prev) (current-indentation)
      (let ((indent-len 4)
            (cur-line (string-trim-right (thing-at-point 'line t)))
            (prev-line (string-trim-right (car prev)))
            (prev-indent (cdr prev)))
        (cond
         ((string-match-p "^\\s-*match\\s-*(.+)" prev-line) prev-indent)
         ((and (string-suffix-p "{" prev-line) (string-prefix-p "}" (string-trim-left cur-line))) prev-indent)
         ((string-suffix-p "{" prev-line) (+ prev-indent indent-len))
         ((string-prefix-p "}" (string-trim-left cur-line)) (max (- prev-indent indent-len) 0))
         ((string-suffix-p ":" prev-line) (if (string-suffix-p ":" cur-line) prev-indent (+ prev-indent indent-len)))
         ((string-suffix-p ":" cur-line) (max (- prev-indent indent-len) 0))
         (t prev-indent))))))

(defun krait-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation (krait--desired-indentation)) (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation) (forward-char n))))

;;;###autoload
(define-derived-mode krait-mode prog-mode "Krait"
  (setq-local indent-line-function 'krait-indent-line)
  :syntax-table krait-mode-syntax-table
  (setq-local indent-line-function #'krait-indent-line)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function #'krait-syntax-propertize)
  (setq-local font-lock-defaults
              '(krait-mode-font-lock-keywords nil nil nil
                (font-lock-syntactic-face-function . krait-syntactic-face)))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local imenu-generic-expression
              '(("Functions" "\\_<fn\\s-+\\([[:word:]]+\\)" 1)
                ("Types" "\\_<\\(?:struct\\|enum\\)\\s-+\\([[:word:]]+\\)" 1)
                ("Constants" "#bake\\s-+\\([[:word:]]+\\)" 1)))
  (setq-local imenu-case-fold-search nil)
  (setq-local beginning-of-defun-function #'krait-beginning-of-defun)
  (setq-local end-of-defun-function #'krait-end-of-defun))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.krt\\'" . krait-mode))

;; Language server: `krait --lsp' (see docs/lsp.md).  Eglot (built into
;; Emacs 29) and lsp-mode both know it once loaded; start it with `M-x eglot'
;; or `M-x lsp', or add `eglot-ensure' to `krait-mode-hook'.
(defcustom krait-lsp-command '("krait" "--lsp")
  "Command that starts the Krait language server.
Use an absolute path when `krait' is not on `exec-path' (GUI Emacs on
macOS does not inherit the shell's PATH)."
  :type '(repeat string)
  :group 'krait)

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs `(krait-mode . ,(lambda (&rest _) krait-lsp-command))))

(declare-function lsp-register-client "lsp-mode")
(declare-function make-lsp-client "lsp-mode")
(declare-function lsp-stdio-connection "lsp-mode")
(declare-function lsp-activate-on "lsp-mode")
(defvar lsp-language-id-configuration)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(krait-mode . "krait"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () krait-lsp-command))
                    :activation-fn (lsp-activate-on "krait")
                    :server-id 'krait)))

(provide 'krait-mode)

;;; krait-mode.el ends here
