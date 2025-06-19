(deftheme materialdark
  "Material Dark theme - Google's Material Design dark theme.")

(let* ((class '((class color) (min-colors 89)))
       (materialdark-colors
        '(
          (bg         . "#263238")  ;; Material Dark background
          (fg         . "#eeffff")  ;; Material Dark foreground
          (cursor     . "#82aaff")  ;; Main color: blue
          (cursor-txt . "#263238")  ;; Material Dark cursor text
          (selection  . "#546e7a")  ;; Material Dark selection
          (selection-fg . "#eeffff")  ;; Material Dark selection fg
          (black      . "#263238")  ;; Material Dark black
          (red        . "#ff5370")  ;; Material Dark red
          (green      . "#c3e88d")  ;; Material Dark green
          (yellow     . "#ffcb6b")  ;; Material Dark yellow
          (blue       . "#82aaff")  ;; Material Dark blue - MAIN COLOR
          (magenta    . "#c792ea")  ;; Material Dark magenta
          (cyan       . "#89ddff")  ;; Material Dark cyan
          (white      . "#eeffff")  ;; Material Dark white
          (br-black   . "#546e7a")  ;; Material Dark comment
          (br-red     . "#ff5370")  ;; Material Dark bright red
          (br-green   . "#c3e88d")  ;; Material Dark bright green
          (br-yellow  . "#ffcb6b")  ;; Material Dark bright yellow
          (br-blue    . "#82aaff")  ;; Material Dark bright blue
          (br-magenta . "#c792ea")  ;; Material Dark bright magenta
          (br-cyan    . "#89ddff")  ;; Material Dark bright cyan
          (br-white   . "#ffffff")  ;; Material Dark bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key materialdark-colors)) 'unspecified))))

  (custom-theme-set-faces
   'materialdark

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

   ;; Font lock faces - carefully tuned for Material Dark style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'cyan)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'magenta)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'blue) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'materialdark)