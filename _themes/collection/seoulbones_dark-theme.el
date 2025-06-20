(deftheme seoulbones_dark
  "seoulbones_dark-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (seoulbones_dark-colors
        '(
(bg         . "#4b4b4b")
          (fg         . "#dddddd")
          (cursor     . "#e2e2e2")
          (cursor-txt . "#4b4b4b")
          (selection  . "#777777")
          (selection-fg . "#dddddd")
          (black      . "#4b4b4b")
          (red        . "#e388a3")
          (green      . "#98bd99")
          (yellow     . "#ffdf9b")
          (blue       . "#97bdde")
          (magenta    . "#a5a6c5")
          (cyan       . "#6fbdbe")
          (white      . "#dddddd")
          (br-black   . "#6c6465")
          (br-red     . "#eb99b1")
          (br-green   . "#8fcd92")
          (br-yellow  . "#ffe5b3")
          (br-blue    . "#a2c8e9")
          (br-magenta . "#b2b3da")
          (br-cyan    . "#6bcacb")
          (br-white   . "#a8a8a8")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key seoulbones_dark-colors)) 'unspecified))))

  (custom-theme-set-faces
   'seoulbones_dark

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

(provide-theme 'seoulbones_dark)