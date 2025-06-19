(deftheme tokyonight
  "Tokyo Night theme - A clean, dark theme inspired by the city of Tokyo.")

(let* ((class '((class color) (min-colors 89)))
       (tokyonight-colors
        '(
          (bg         . "#1a1b26")  ;; Tokyo Night background
          (fg         . "#a9b1d6")  ;; Tokyo Night foreground
          (cursor     . "#c0caf5")  ;; Tokyo Night cursor
          (cursor-txt . "#1a1b26")  ;; Tokyo Night cursor text
          (selection  . "#33467c")  ;; Tokyo Night selection
          (selection-fg . "#a9b1d6")  ;; Tokyo Night selection fg
          (black      . "#1a1b26")  ;; Tokyo Night black
          (red        . "#f7768e")  ;; Tokyo Night red
          (green      . "#9ece6a")  ;; Tokyo Night green
          (yellow     . "#e0af68")  ;; Tokyo Night yellow
          (blue       . "#7aa2f7")  ;; Tokyo Night blue
          (magenta    . "#bb9af7")  ;; Tokyo Night magenta
          (cyan       . "#7dcfff")  ;; Tokyo Night cyan
          (white      . "#c0caf5")  ;; Tokyo Night white
          (br-black   . "#565a6e")  ;; Tokyo Night comment
          (br-red     . "#f7768e")  ;; Tokyo Night bright red
          (br-green   . "#9ece6a")  ;; Tokyo Night bright green
          (br-yellow  . "#e0af68")  ;; Tokyo Night bright yellow
          (br-blue    . "#7aa2f7")  ;; Tokyo Night bright blue
          (br-magenta . "#bb9af7")  ;; Tokyo Night bright magenta
          (br-cyan    . "#7dcfff")  ;; Tokyo Night bright cyan
          (br-white   . "#c0caf5")  ;; Tokyo Night bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key tokyonight-colors)) 'unspecified))))

  (custom-theme-set-faces
   'tokyonight

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

   ;; Font lock faces - carefully tuned for Tokyo Night style
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
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'tokyonight)