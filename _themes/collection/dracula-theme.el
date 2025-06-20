(deftheme dracula
  "Dracula theme - A dark theme for the colorblind and non-colorblind coders.")

(let* ((class '((class color) (min-colors 89)))
       (dracula-colors
        '(
          (bg         . "#282a36")  ;; Dracula background
          (fg         . "#f8f8f2")  ;; Dracula foreground
          (cursor     . "#f8f8f2")  ;; Dracula cursor
          (cursor-txt . "#282a36")  ;; Dracula cursor text
          (selection  . "#44475a")  ;; Dracula selection
          (selection-fg . "#f8f8f2")  ;; Dracula selection fg
          (black      . "#282a36")  ;; Dracula black
          (red        . "#ff5555")  ;; Dracula red
          (green      . "#50fa7b")  ;; Dracula green
          (yellow     . "#f1fa8c")  ;; Dracula yellow
          (blue       . "#bd93f9")  ;; Dracula purple
          (magenta    . "#ff79c6")  ;; Dracula pink
          (cyan       . "#8be9fd")  ;; Dracula cyan
          (white      . "#f8f8f2")  ;; Dracula white
          (br-black   . "#6272a4")  ;; Dracula comment
          (br-red     . "#ff6e6e")  ;; Dracula bright red
          (br-green   . "#69ff94")  ;; Dracula bright green
          (br-yellow  . "#ffffa5")  ;; Dracula bright yellow
          (br-blue    . "#d6acff")  ;; Dracula bright purple
          (br-magenta . "#ff92df")  ;; Dracula bright pink
          (br-cyan    . "#a4ffff")  ;; Dracula bright cyan
          (br-white   . "#ffffff")  ;; Dracula bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key dracula-colors)) 'unspecified))))

  (custom-theme-set-faces
   'dracula

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

   ;; Font lock faces - carefully tuned for Dracula style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'blue) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'dracula)