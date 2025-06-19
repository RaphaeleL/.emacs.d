(deftheme apple_system_colors
  "Apple System Colors theme - Modern macOS system colors.")

(let* ((class '((class color) (min-colors 89)))
       (apple_system_colors-colors
        '(
          (bg         . "#ffffff")  ;; Apple System background
          (fg         . "#000000")  ;; Apple System foreground
          (cursor     . "#007aff")  ;; Main color: blue
          (cursor-txt . "#ffffff")  ;; Apple System cursor text
          (selection  . "#007aff")  ;; Apple System selection
          (selection-fg . "#ffffff")  ;; Apple System selection fg
          (black      . "#000000")  ;; Apple System black
          (red        . "#ff3b30")  ;; Apple System red
          (green      . "#34c759")  ;; Apple System green
          (yellow     . "#ff9500")  ;; Apple System yellow
          (blue       . "#007aff")  ;; Apple System blue - MAIN COLOR
          (magenta    . "#af52de")  ;; Apple System magenta
          (cyan       . "#5ac8fa")  ;; Apple System cyan
          (white      . "#ffffff")  ;; Apple System white
          (br-black   . "#8e8e93")  ;; Apple System comment
          (br-red     . "#ff453a")  ;; Apple System bright red
          (br-green   . "#30d158")  ;; Apple System bright green
          (br-yellow  . "#ffd60a")  ;; Apple System bright yellow
          (br-blue    . "#0a84ff")  ;; Apple System bright blue
          (br-magenta . "#bf5af2")  ;; Apple System bright magenta
          (br-cyan    . "#64d2ff")  ;; Apple System bright cyan
          (br-white   . "#ffffff")  ;; Apple System bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key apple_system_colors-colors)) 'unspecified))))

  (custom-theme-set-faces
   'apple_system_colors

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
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Apple System Colors style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'magenta)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green) :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'br-blue) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'black)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'apple_system_colors)