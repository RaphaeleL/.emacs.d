(deftheme githubdarkhighcontrast
  "githubdarkhighcontrast-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (githubdarkhighcontrast-colors
        '(
(bg         . "#0a0c10")
          (fg         . "#f0f3f6")
          (cursor     . "#71b7ff")
          (cursor-txt . "#71b7ff")
          (selection  . "#f0f3f6")
          (selection-fg . "#0a0c10")
          (black      . "#7a828e")
          (red        . "#ff9492")
          (green      . "#26cd4d")
          (yellow     . "#f0b72f")
          (blue       . "#71b7ff")
          (magenta    . "#cb9eff")
          (cyan       . "#39c5cf")
          (white      . "#d9dee3")
          (br-black   . "#9ea7b3")
          (br-red     . "#ffb1af")
          (br-green   . "#4ae168")
          (br-yellow  . "#f7c843")
          (br-blue    . "#91cbff")
          (br-magenta . "#dbb7ff")
          (br-cyan    . "#56d4dd")
          (br-white   . "#ffffff")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key githubdarkhighcontrast-colors)) 'unspecified))))

  (custom-theme-set-faces
   'githubdarkhighcontrast

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
   `(show-paren-match ((,class (:background ,(funcall safe-get-color 'br-yellow)
                                            :foreground ,(funcall safe-get-color 'bg)
                                            :weight bold))))
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'br-red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-green) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-green) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))))

(provide-theme 'githubdarkhighcontrast)