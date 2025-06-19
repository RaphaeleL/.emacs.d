(deftheme solarized_light
  "Solarized Light theme - Ethan Schoonover's precision color scheme.")

(let* ((class '((class color) (min-colors 89)))
       (solarized_light-colors
        '(
          (bg         . "#fdf6e3")  ;; Base 3 - background
          (fg         . "#657b83")  ;; Base 00 - primary content
          (cursor     . "#268bd2")  ;; Main color: blue
          (cursor-txt . "#fdf6e3")  ;; Base 3 - background
          (selection  . "#eee8d5")  ;; Base 2 - selection
          (selection-fg . "#586e75")  ;; Base 01 - emphasized content
          (black      . "#073642")  ;; Base 02 - dark content
          (red        . "#dc322f")  ;; Red - error
          (green      . "#859900")  ;; Green - success
          (yellow     . "#b58900")  ;; Yellow - warning
          (blue       . "#268bd2")  ;; Blue - primary - MAIN COLOR
          (magenta    . "#d33682")  ;; Magenta - special
          (cyan       . "#2aa198")  ;; Cyan - strings
          (white      . "#fdf6e3")  ;; Base 3 - background
          (br-black   . "#93a1a1")  ;; Base 1 - comments
          (br-red     . "#cb4b16")  ;; Orange - keywords
          (br-green   . "#859900")  ;; Green - success
          (br-yellow  . "#b58900")  ;; Yellow - warning
          (br-blue    . "#268bd2")  ;; Blue - primary
          (br-magenta . "#6c71c4")  ;; Violet - rare
          (br-cyan    . "#2aa198")  ;; Cyan - strings
          (br-white   . "#fdf6e3")  ;; Base 3 - background
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key solarized_light-colors)) 'unspecified))))

  (custom-theme-set-faces
   'solarized_light

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
   `(show-paren-match ((,class (:background ,(funcall safe-get-color 'br-cyan)
                                            :foreground ,(funcall safe-get-color 'bg)
                                            :weight bold))))
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))

   ;; Line number faces - main color for current line
   `(line-number ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Solarized Light style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'solarized_light)