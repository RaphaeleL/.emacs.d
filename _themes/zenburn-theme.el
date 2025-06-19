(deftheme zenburn
  "Zenburn theme - A low contrast color scheme for Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (zenburn-colors
        '(
          (bg         . "#3f3f3f")  ;; Zenburn background
          (fg         . "#dcdccc")  ;; Zenburn foreground
          (cursor     . "#7f9f7f")  ;; Main color: green
          (cursor-txt . "#3f3f3f")  ;; Zenburn cursor text
          (selection  . "#4f4f4f")  ;; Zenburn selection
          (selection-fg . "#dcdccc")  ;; Zenburn selection fg
          (black      . "#3f3f3f")  ;; Zenburn black
          (red        . "#cc9393")  ;; Zenburn red
          (green      . "#7f9f7f")  ;; Zenburn green - MAIN COLOR
          (yellow     . "#f0dfaf")  ;; Zenburn yellow
          (blue       . "#8cd0d3")  ;; Zenburn blue
          (magenta    . "#dc8cc3")  ;; Zenburn magenta
          (cyan       . "#93e0e3")  ;; Zenburn cyan
          (white      . "#dcdccc")  ;; Zenburn white
          (br-black   . "#709080")  ;; Zenburn comment
          (br-red     . "#cc9393")  ;; Zenburn bright red
          (br-green   . "#7f9f7f")  ;; Zenburn bright green
          (br-yellow  . "#f0dfaf")  ;; Zenburn bright yellow
          (br-blue    . "#8cd0d3")  ;; Zenburn bright blue
          (br-magenta . "#dc8cc3")  ;; Zenburn bright magenta
          (br-cyan    . "#93e0e3")  ;; Zenburn bright cyan
          (br-white   . "#ffffff")  ;; Zenburn bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key zenburn-colors)) 'unspecified))))

  (custom-theme-set-faces
   'zenburn

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
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'green) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Zenburn style
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

(provide-theme 'zenburn)