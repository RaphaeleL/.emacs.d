(deftheme japanesque
  "japanesque-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (japanesque-colors
        '(
(bg         . "#1e1e1e")
          (fg         . "#f7f6ec")
          (cursor     . "#edcf4f")
          (cursor-txt . "#343935")
          (selection  . "#175877")
          (selection-fg . "#f7f6ec")
          (black      . "#343935")
          (red        . "#cf3f61")
          (green      . "#7bb75b")
          (yellow     . "#e9b32a")
          (blue       . "#4c9ad4")
          (magenta    . "#a57fc4")
          (cyan       . "#389aad")
          (white      . "#fafaf6")
          (br-black   . "#595b59")
          (br-red     . "#d18fa6")
          (br-green   . "#767f2c")
          (br-yellow  . "#78592f")
          (br-blue    . "#135979")
          (br-magenta . "#604291")
          (br-cyan    . "#76bbca")
          (br-white   . "#b2b5ae")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key japanesque-colors)) 'unspecified))))

  (custom-theme-set-faces
   'japanesque

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

(provide-theme 'japanesque)