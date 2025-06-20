(deftheme iterm2_pastel_dark_background
  "iterm2_pastel_dark_background-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (iterm2_pastel_dark_background-colors
        '(
(bg         . "#000000")
          (fg         . "#c7c7c7")
          (cursor     . "#ffb473")
          (cursor-txt . "#ffffff")
          (selection  . "#454d96")
          (selection-fg . "#f4f4f4")
          (black      . "#626262")
          (red        . "#ff8373")
          (green      . "#b4fb73")
          (yellow     . "#fffdc3")
          (blue       . "#a5d5fe")
          (magenta    . "#ff90fe")
          (cyan       . "#d1d1fe")
          (white      . "#f1f1f1")
          (br-black   . "#8f8f8f")
          (br-red     . "#ffc4be")
          (br-green   . "#d6fcba")
          (br-yellow  . "#fffed5")
          (br-blue    . "#c2e3ff")
          (br-magenta . "#ffb2fe")
          (br-cyan    . "#e6e6fe")
          (br-white   . "#ffffff")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key iterm2_pastel_dark_background-colors)) 'unspecified))))

  (custom-theme-set-faces
   'iterm2_pastel_dark_background

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

(provide-theme 'iterm2_pastel_dark_background)