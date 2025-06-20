(deftheme solarized_dark_higher_contrast
  "solarized_dark_higher_contrast-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (solarized_dark_higher_contrast-colors
        '(
(bg         . "#001e27")
          (fg         . "#9cc2c3")
          (cursor     . "#f34b00")
          (cursor-txt . "#002831")
          (selection  . "#003748")
          (selection-fg . "#7a8f8e")
          (black      . "#002831")
          (red        . "#d11c24")
          (green      . "#6cbe6c")
          (yellow     . "#a57706")
          (blue       . "#2176c7")
          (magenta    . "#c61c6f")
          (cyan       . "#259286")
          (white      . "#eae3cb")
          (br-black   . "#006488")
          (br-red     . "#f5163b")
          (br-green   . "#51ef84")
          (br-yellow  . "#b27e28")
          (br-blue    . "#178ec8")
          (br-magenta . "#e24d8e")
          (br-cyan    . "#00b39e")
          (br-white   . "#fcf4dc")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key solarized_dark_higher_contrast-colors)) 'unspecified))))

  (custom-theme-set-faces
   'solarized_dark_higher_contrast

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

(provide-theme 'solarized_dark_higher_contrast)