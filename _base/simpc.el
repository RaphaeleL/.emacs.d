;; TOOK FROM: https://github.com/rexim/dotfiles/blob/a96479ac248e8d1b3ad307fdd667eb4593eec56d/.emacs.local/simpc-mode.el#L29

(require 'subr-x)

(defvar simpc-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
	(modify-syntax-entry ?/ ". 124b" table)
	(modify-syntax-entry ?* ". 23" table)
	(modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff?
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)

    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun simpc-types ()
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"))

(defun simpc-keywords ()
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return"  "sizeof" "static" "struct" "switch" "typedef"
    "union"  "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch"  "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false"
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun simpc-font-lock-keywords ()
  (list
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (simpc-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (simpc-types) 'symbols) . font-lock-type-face)))

(defun simpc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun simpc--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun simpc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (simpc--previous-non-empty-line)))
         (indent-len 4)
         (prev-indent (simpc--indentation-of-previous-non-empty-line)))
    (cond
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

;;; TODO: customizable indentation (amount of spaces, tabs, etc)
(defun simpc-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (simpc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;; ==================================================
;;; ===== C HEADER FILE COMPLETION ===================
;;; ==================================================

;; Completion based on symbols found in open C/C++ header files
;; No LSP, no system includes, no ctags - just parse open buffers

(defvar simpc-header-completion-cache nil
  "Cache of symbols extracted from header files.
Format: (buffer-name . (symbol1 symbol2 ...))")

(defvar simpc-header-completion-cache-timestamp nil
  "Timestamp when cache was last updated.")

(defun simpc-header-file-p (buffer)
  "Check if BUFFER is a C/C++ header file."
  (let ((file-name (buffer-file-name buffer)))
    (and file-name
         (string-match-p "\\.\\(h\\|hpp\\|hh\\)\\'" file-name))))

(defun simpc-extract-symbols-from-buffer (buffer)
  "Extract C/C++ identifiers from BUFFER.
Returns a list of unique symbols (functions, types, macros, etc.)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (let ((symbols '())
              (case-fold-search nil)
              (keywords (append (simpc-keywords) (simpc-types)
                                '("include" "define" "undef" "ifdef" "ifndef" "endif"
                                  "elif" "error" "pragma" "line" "warning"))))
          (goto-char (point-min))
          ;; Extract C identifiers: [a-zA-Z_][a-zA-Z0-9_]*
          ;; Look for patterns like:
          ;; - Function declarations: void DrawLine(...) or RLAPI void DrawLine(...)
          ;; - Macros: #define DrawLine ...
          ;; - Types: typedef struct DrawLine ...
          ;; - Variables: extern int DrawLine;
          (while (re-search-forward
                  "\\<[a-zA-Z_][a-zA-Z0-9_]*\\>"
                  nil t)
            (let ((match (match-string 0)))
              (when (and match
                         (> (length match) 1)
                         ;; Not a keyword
                         (not (member match keywords))
                         ;; Not a number
                         (not (string-match-p "^[0-9]" match)))
                (push match symbols))))
          (delete-dups symbols))))))

(defun simpc-update-header-completion-cache ()
  "Update the cache of symbols from all open header files."
  (let ((header-buffers (seq-filter #'simpc-header-file-p (buffer-list)))
        (new-cache '()))
    (dolist (buf header-buffers)
      (let ((symbols (simpc-extract-symbols-from-buffer buf)))
        (when symbols
          (push (cons (buffer-name buf) symbols) new-cache))))
    (setq simpc-header-completion-cache new-cache)
    (setq simpc-header-completion-cache-timestamp (current-time))))

(defun simpc-get-all-header-symbols ()
  "Get all symbols from cached header files."
  (if (not simpc-header-completion-cache)
      '()
    (let ((all-symbols '()))
      (dolist (entry simpc-header-completion-cache)
        (when (and (consp entry) (cdr entry))
          (setq all-symbols (append all-symbols (cdr entry)))))
      (delete-dups all-symbols))))

(defun simpc-header-completion-at-point ()
  "Completion function for simpc-mode based on open header files.
Returns completion candidates from symbols found in open .h/.hpp files."
  (when (eq major-mode 'simpc-mode)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((start (car bounds))
              (end (cdr bounds))
              (prefix (downcase (buffer-substring-no-properties (car bounds) (cdr bounds)))))
          ;; Update cache if needed (when buffers change or on first use)
          (let ((current-header-count (length (seq-filter #'simpc-header-file-p (buffer-list))))
                (cached-header-count (length simpc-header-completion-cache)))
            (when (or (not simpc-header-completion-cache)
                      (not (= current-header-count cached-header-count)))
              (simpc-update-header-completion-cache)))
          
          ;; Get all symbols and filter by prefix (case-insensitive)
          (let ((all-symbols (simpc-get-all-header-symbols))
                (matches '()))
            (when all-symbols
              (dolist (sym all-symbols)
                (when (string-prefix-p prefix (downcase sym) t)
                  (push sym matches)))
              (setq matches (nreverse matches))
              (when matches
                (list start end matches :annotation-function nil)))))))))

(define-derived-mode simpc-mode prog-mode "Simple C"
  "Simple major mode for editing C files."
  :syntax-table simpc-mode-syntax-table
  (setq-local font-lock-defaults '(simpc-font-lock-keywords))
  (setq-local indent-line-function 'simpc-indent-line)
  (setq-local comment-start "// ")
  ;; Add header file completion
  (add-hook 'completion-at-point-functions
            #'simpc-header-completion-at-point
            nil t))

;; Update cache when header files are opened
(add-hook 'find-file-hook
          (lambda ()
            (when (simpc-header-file-p (current-buffer))
              (simpc-update-header-completion-cache))))

;; Update cache when buffers are killed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (simpc-header-file-p (current-buffer))
              (simpc-update-header-completion-cache))))

(provide 'simpc-mode)
