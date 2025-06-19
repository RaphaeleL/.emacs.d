(deftheme monokai_classic
  "Monokai Classic theme - Sublime Text's original Monokai theme.")

(let* ((class '((class color) (min-colors 89)))
       (monokai_classic-colors
        '(
          (bg         . "#272822")  ;; Classic dark gray background
          (fg         . "#F8F8F2")  ;; Off-white text
          (cursor     . "#F8F8F2")  ;; Match foreground
          (cursor-txt . "#272822")  ;; Match background
          (selection  . "#49483E")  ;; Soft brown selection
          (selection-fg . "#F8F8F2")  ;; Off-white selection text
          (black      . "#272822")  ;; Base dark
          (red        . "#F92672")  ;; Pink-red for keywords
          (green      . "#A6E22E")  ;; Bright green for functions
          (yellow     . "#E6DB74")  ;; Light yellow for strings
          (blue       . "#66D9EF")  ;; Cyan-blue for types
          (magenta    . "#FD5FF0")  ;; Pink for preprocessor
          (cyan       . "#A1EFE4")  ;; Light cyan
          (white      . "#F8F8F2")  ;; Off-white
          (br-black   . "#75715E")  ;; Brown-gray for comments
          (br-red     . "#F92672")  ;; Same as red
          (br-green   . "#A6E22E")  ;; Same as green
          (br-yellow  . "#E6DB74")  ;; Same as yellow
          (br-blue    . "#66D9EF")  ;; Same as blue
          (br-magenta . "#FD5FF0")  ;; Same as magenta
          (br-cyan    . "#A1EFE4")  ;; Same as cyan
          (br-white   . "#F9F8F5")  ;; Pure white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key monokai_classic-colors)) 'unspecified))))

  (custom-theme-set-faces
   'monokai_classic

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
   `(show-paren-match ((,class (:background ,(funcall safe-get-color 'br-magenta)
                                            :foreground ,(funcall safe-get-color 'bg)
                                            :weight bold))))
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))

   ;; Font lock faces - carefully tuned for Monokai Classic style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'purple)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'blue) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))))

(provide-theme 'monokai_classic)