(deftheme monokai_pro_spectrum
  "monokai_pro_spectrum-inspired color theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (monokai_pro_spectrum-colors
        '(
(bg         . "#222222")
          (fg         . "#f7f1ff")
          (cursor     . "#bab6c0")
          (cursor-txt . "#bab6c0")
          (selection  . "#525053")
          (selection-fg . "#f7f1ff")
          (black      . "#222222")
          (red        . "#fc618d")
          (green      . "#7bd88f")
          (yellow     . "#fce566")
          (blue       . "#fd9353")
          (magenta    . "#948ae3")
          (cyan       . "#5ad4e6")
          (white      . "#f7f1ff")
          (br-black   . "#69676c")
          (br-red     . "#fc618d")
          (br-green   . "#7bd88f")
          (br-yellow  . "#fce566")
          (br-blue    . "#fd9353")
          (br-magenta . "#948ae3")
          (br-cyan    . "#5ad4e6")
          (br-white   . "#f7f1ff")
          
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key monokai_pro_spectrum-colors)) 'unspecified))))

  (custom-theme-set-faces
   'monokai_pro_spectrum

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

(provide-theme 'monokai_pro_spectrum)