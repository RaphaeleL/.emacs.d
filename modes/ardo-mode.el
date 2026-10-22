;;; ardo-mode.el --- Major mode for the Ardo programming language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ardo compiler repository
;;
;; Author: Ardo compiler repository
;; Version: 1.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; A major mode for Ardo (*.ardo) source files.  It provides font-lock
;; highlighting for the whole lexical surface (directives, keywords,
;; builtin and capitalized type names, function names, `#bake' constants,
;; strings, character literals, numbers), correct handling of nestable
;; `/* ... */' block comments (plus `//' line comments and `/** ... */'
;; documentation comments), imenu entries, and `beginning-of-defun' /
;; `end-of-defun' navigation.

;;; Code:

(require 'syntax)

(defgroup ardo nil
  "Major mode for editing Ardo code."
  :group 'languages
  :prefix "ardo-")

(defvar ardo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Identifiers: [A-Za-z_][A-Za-z0-9_]*.
    (modify-syntax-entry ?_ "w" table)
    ;; `//' line comments ...
    (modify-syntax-entry ?/ ". 124b" table)
    ;; ... and nestable `/* ... */' block comments (the `n' flag).
    (modify-syntax-entry ?* ". 23n" table)
    ;; A newline ends `//' comments (but not block comments).
    (modify-syntax-entry ?\n "> b" table)
    ;; Strings and character literals, with backslash escapes.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; `#' introduces directives; it never starts a comment.
    (modify-syntax-entry ?# "." table)
    table)
  "Syntax table for `ardo-mode'.")

(defun ardo-syntax-propertize (start end)
  "Apply `syntax-table' properties between START and END for `ardo-mode'.
Nested block comments need none: the syntax table marks them nestable."
  (ardo-syntax-propertize-strings start end)
  (ardo-syntax-propertize-labels start end))

(defun ardo-syntax-propertize-labels (start end)
  "Mark the quote of loop labels (`'outer') between START and END as punctuation.
A label has no closing quote, so it must not open a character literal."
  (goto-char start)
  (while (re-search-forward "'[[:alpha:]_][[:alnum:]_]*\\_>\\([^']\\|$\\)" end t)
    (let ((quote (match-beginning 0)))
      (goto-char (match-beginning 1)) ; rescan the character after the name
      (unless (nth 8 (save-excursion (syntax-ppss quote))) ; not in a comment/string
        (put-text-property quote (1+ quote) 'syntax-table (string-to-syntax "."))))))

(defun ardo-syntax-propertize-strings (start end)
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

(defun ardo-syntactic-face (state)
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

(defconst ardo-mode--defun-regexp
  "^\\s-*\\(?:pub\\s-+\\)?\\(?:fn\\|struct\\|enum\\|#bake\\|#extern\\)\\_>"
  "Regexp matching the start of a top-level Ardo definition.")

(defun ardo-beginning-of-defun (&optional arg)
  "Move backward to the start of the current or previous Ardo definition.
With ARG, do it that many times; negative ARG moves forward."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((search-fn (if (>= arg 0) #'re-search-backward #'re-search-forward))
        (count (abs arg)))
    (while (> count 0)
      (if (funcall search-fn ardo-mode--defun-regexp nil t)
          (progn
            (goto-char (match-beginning 0))
            (setq count (1- count)))
        (goto-char (if (>= arg 0) (point-min) (point-max)))
        (setq count 0)))
    (beginning-of-line)))

(defun ardo-end-of-defun (&optional arg)
  "Move forward to the start of the next Ardo definition, ARG times."
  (interactive "^p")
  (setq arg (max 1 (or arg 1)))
  (while (> arg 0)
    (forward-line 1)
    (if (re-search-forward ardo-mode--defun-regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (setq arg (1- arg)))
      (goto-char (point-max))
      (setq arg 0))))

(defconst ardo-mode-font-lock-keywords
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
         (builtins (regexp-opt '("alloc" "free" "sizeof" "alignof" "offset" "move_out" "assert" "error") 'symbols))
         ;; Capitalized identifiers that are not SCREAMING_CASE:
         ;; `Shape', `List', `Point2D', and single-letter type params `T'.
         (user-types "\\_<[[:upper:]]\\(?:\\|[[:alnum:]_]*[[:lower:]][[:alnum:]_]*\\)\\_>")
         ;; SCREAMING_CASE names: by convention `#bake' constants.
         (constants "\\_<[[:upper:]][[:upper:]_0-9]\\{1,\\}\\_>")
         (number-face (if (facep 'font-lock-number-face)
                          'font-lock-number-face
                        'font-lock-constant-face)))
    `(;; Directives: #import, #load, #library, #assert, #all, #bake, #copy, #on, #extern, #char, #repeat, #type, #ignore, #entrypoint.
      ("#\\(?:import\\|load\\|library\\|assert\\|all\\|bake\\|copy\\|on\\|extern\\|char\\|repeat\\|type\\|ignore\\|entrypoint\\)\\_>" 0 font-lock-preprocessor-face)
      ;; Loop labels: `'outer for ...', `break 'outer;'.
      ("'[[:alpha:]_][[:alnum:]_]*\\_>\\(?:[^']\\|$\\)" 0 font-lock-constant-face)
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
  "Font-lock rules for `ardo-mode'.")

(defun ardo--previous-non-empty-line ()
  (save-excursion
    (move-beginning-of-line nil)
    (if (bobp) nil
      (forward-line -1)
      (while (and (not (bobp)) (string-empty-p (string-trim-right (thing-at-point 'line t)))) (forward-line -1))
      (if (string-empty-p (string-trim-right (thing-at-point 'line t))) nil
        (cons (thing-at-point 'line t) (current-indentation))))))

(defun ardo--desired-indentation ()
  (let ((prev (ardo--previous-non-empty-line)))
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

(defun ardo-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation (ardo--desired-indentation)) (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation) (forward-char n))))

;;;###autoload
(define-derived-mode ardo-mode prog-mode "Ardo"
  :syntax-table ardo-mode-syntax-table
  (setq-local indent-line-function #'ardo-indent-line)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function #'ardo-syntax-propertize)
  (setq-local font-lock-defaults
              '(ardo-mode-font-lock-keywords nil nil nil
                (font-lock-syntactic-face-function . ardo-syntactic-face)))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local imenu-generic-expression
              '(("Functions" "\\_<fn\\s-+\\([[:word:]]+\\)" 1)
                ("Types" "\\_<\\(?:struct\\|enum\\)\\s-+\\([[:word:]]+\\)" 1)
                ("Constants" "#bake\\s-+\\([[:word:]]+\\)" 1)))
  (setq-local imenu-case-fold-search nil)
  (setq-local beginning-of-defun-function #'ardo-beginning-of-defun)
  (setq-local end-of-defun-function #'ardo-end-of-defun))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ardo\\'" . ardo-mode))

;; Language server: `ardo --lsp' (see docs/lsp.md).  Eglot (built into
;; Emacs 29) and lsp-mode both know it once loaded; start it with `M-x eglot'
;; or `M-x lsp', or add `eglot-ensure' to `ardo-mode-hook'.
(defcustom ardo-lsp-command '("ardo" "--lsp")
  "Command that starts the Ardo language server.
Use an absolute path when `ardo' is not on `exec-path' (GUI Emacs on
macOS does not inherit the shell's PATH)."
  :type '(repeat string)
  :group 'ardo)

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs `(ardo-mode . ,(lambda (&rest _) ardo-lsp-command))))

(declare-function lsp-register-client "lsp-mode")
(declare-function make-lsp-client "lsp-mode")
(declare-function lsp-stdio-connection "lsp-mode")
(declare-function lsp-activate-on "lsp-mode")
(defvar lsp-language-id-configuration)
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(ardo-mode . "ardo"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection (lambda () ardo-lsp-command))
                    :activation-fn (lsp-activate-on "ardo")
                    :server-id 'ardo)))

(provide 'ardo-mode)

;;; ardo-mode.el ends here
