(deftheme catppuccinmocha
  "Catppuccin Mocha theme - Soothing pastel theme for the high-spirited!")

(let* ((class '((class color) (min-colors 89)))
       (catppuccinmocha-colors
        '(
          (bg         . "#1e1e2e")  ;; Catppuccin Mocha base
          (fg         . "#cdd6f4")  ;; Catppuccin Mocha text
          (cursor     . "#f5e0dc")  ;; Catppuccin Mocha rosewater
          (cursor-txt . "#1e1e2e")  ;; Catppuccin Mocha base
          (selection  . "#313244")  ;; Catppuccin Mocha surface0
          (selection-fg . "#cdd6f4")  ;; Catppuccin Mocha text
          (black      . "#1e1e2e")  ;; Catppuccin Mocha base
          (red        . "#f38ba8")  ;; Catppuccin Mocha red
          (green      . "#a6e3a1")  ;; Catppuccin Mocha green
          (yellow     . "#f9e2af")  ;; Catppuccin Mocha yellow
          (blue       . "#89b4fa")  ;; Catppuccin Mocha blue
          (magenta    . "#f5c2e7")  ;; Catppuccin Mocha pink
          (cyan       . "#94e2d5")  ;; Catppuccin Mocha teal
          (white      . "#cdd6f4")  ;; Catppuccin Mocha text
          (br-black   . "#6c7086")  ;; Catppuccin Mocha overlay0
          (br-red     . "#f38ba8")  ;; Catppuccin Mocha red
          (br-green   . "#a6e3a1")  ;; Catppuccin Mocha green
          (br-yellow  . "#f9e2af")  ;; Catppuccin Mocha yellow
          (br-blue    . "#89b4fa")  ;; Catppuccin Mocha blue
          (br-magenta . "#f5c2e7")  ;; Catppuccin Mocha pink
          (br-cyan    . "#94e2d5")  ;; Catppuccin Mocha teal
          (br-white   . "#f5e0dc")  ;; Catppuccin Mocha rosewater
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key catppuccinmocha-colors)) 'unspecified))))

  (custom-theme-set-faces
   'catppuccinmocha

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

   ;; Font lock faces - carefully tuned for Catppuccin Mocha style
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

(provide-theme 'catppuccinmocha)