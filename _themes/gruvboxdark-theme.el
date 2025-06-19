(deftheme gruvboxdark
  "Gruvbox Dark theme - Retro groove color scheme.")

(let* ((class '((class color) (min-colors 89)))
       (gruvboxdark-colors
        '(
          (bg         . "#282828")  ;; Gruvbox Dark background
          (fg         . "#ebdbb2")  ;; Gruvbox Dark foreground
          (cursor     . "#b8bb26")  ;; Main color: green
          (cursor-txt . "#282828")  ;; Gruvbox Dark cursor text
          (selection  . "#504945")  ;; Gruvbox Dark selection
          (selection-fg . "#ebdbb2")  ;; Gruvbox Dark selection fg
          (black      . "#282828")  ;; Gruvbox Dark black
          (red        . "#cc241d")  ;; Gruvbox Dark red
          (green      . "#98971a")  ;; Gruvbox Dark green
          (yellow     . "#d79921")  ;; Gruvbox Dark yellow
          (blue       . "#458588")  ;; Gruvbox Dark blue
          (magenta    . "#b16286")  ;; Gruvbox Dark magenta
          (cyan       . "#689d6a")  ;; Gruvbox Dark cyan
          (white      . "#a89984")  ;; Gruvbox Dark white
          (br-black   . "#928374")  ;; Gruvbox Dark comment
          (br-red     . "#fb4934")  ;; Gruvbox Dark bright red
          (br-green   . "#b8bb26")  ;; Gruvbox Dark bright green - MAIN COLOR
          (br-yellow  . "#fabd2f")  ;; Gruvbox Dark bright yellow
          (br-blue    . "#83a598")  ;; Gruvbox Dark bright blue
          (br-magenta . "#d3869b")  ;; Gruvbox Dark bright magenta
          (br-cyan    . "#8ec07c")  ;; Gruvbox Dark bright cyan
          (br-white   . "#ebdbb2")  ;; Gruvbox Dark bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key gruvboxdark-colors)) 'unspecified))))

  (custom-theme-set-faces
   'gruvboxdark

   ;; Basic faces
   `(default ((,class (:background ,(funcall safe-get-color 'bg)
                                   :foreground ,(funcall safe-get-color 'fg)))))
   `(cursor ((,class (:background ,(funcall safe-get-color 'cursor)
                                  :foreground ,(funcall safe-get-color 'cursor-txt)))))
   `(region ((,class (:background ,(funcall safe-get-color 'selection)
                                  :foreground ,(funcall safe-get-color 'selection-fg)))))
   `(highlight ((,class (:background ,(funcall safe-get-color 'selection)))))
   `(fringe ((,class (:background ,(funcall safe-get-color 'bg)))))
   `(minibuffer-prompt ((,class (:foreground ,(funcall safe-get-color 'blue)
                                             :weight bold))))
   `(show-paren-match ((,class (:background ,(funcall safe-get-color 'br-blue)
                                            :foreground ,(funcall safe-get-color 'bg)
                                            :weight bold))))
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))

   ;; Line number faces - main color for current line
   `(line-number ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'br-green) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Gruvbox Dark style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'br-blue)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'br-green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'br-green)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'br-yellow) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'br-white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))))

(provide-theme 'gruvboxdark)