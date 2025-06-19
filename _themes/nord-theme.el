(deftheme nord
  "Nord theme - An arctic, north-bluish color palette.")

(let* ((class '((class color) (min-colors 89)))
       (nord-colors
        '(
          (bg         . "#2E3440")  ;; Nord 0 - polar night
          (fg         . "#ECEFF4")  ;; Nord 6 - snow storm
          (cursor     . "#D8DEE9")  ;; Nord 4 - snow storm
          (cursor-txt . "#2E3440")  ;; Nord 0 - polar night
          (selection  . "#434C5E")  ;; Nord 1 - polar night
          (selection-fg . "#ECEFF4")  ;; Nord 6 - snow storm
          (black      . "#2E3440")  ;; Nord 0 - polar night
          (red        . "#BF616A")  ;; Nord 11 - aurora
          (green      . "#A3BE8C")  ;; Nord 14 - aurora
          (yellow     . "#EBCB8B")  ;; Nord 13 - aurora
          (blue       . "#81A1C1")  ;; Nord 10 - frost
          (magenta    . "#B48EAD")  ;; Nord 15 - aurora
          (cyan       . "#88C0D0")  ;; Nord 8 - frost
          (white      . "#ECEFF4")  ;; Nord 6 - snow storm
          (br-black   . "#4C566A")  ;; Nord 3 - polar night
          (br-red     . "#BF616A")  ;; Nord 11 - aurora
          (br-green   . "#A3BE8C")  ;; Nord 14 - aurora
          (br-yellow  . "#EBCB8B")  ;; Nord 13 - aurora
          (br-blue    . "#81A1C1")  ;; Nord 10 - frost
          (br-magenta . "#B48EAD")  ;; Nord 15 - aurora
          (br-cyan    . "#88C0D0")  ;; Nord 8 - frost
          (br-white   . "#ECEFF4")  ;; Nord 6 - snow storm
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key nord-colors)) 'unspecified))))

  (custom-theme-set-faces
   'nord

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

   ;; Font lock faces - carefully tuned for Nord style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'magenta)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'cyan) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'nord)