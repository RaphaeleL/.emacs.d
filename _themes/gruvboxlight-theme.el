(deftheme gruvboxlight
  "Gruvbox Light theme - Retro groove color scheme, light variant.")

(let* ((class '((class color) (min-colors 89)))
       (gruvboxlight-colors
        '(
          (bg         . "#fbf1c7")  ;; Gruvbox Light background
          (fg         . "#3c3836")  ;; Gruvbox Light foreground
          (cursor     . "#3c3836")  ;; Gruvbox Light cursor
          (cursor-txt . "#fbf1c7")  ;; Gruvbox Light cursor text
          (selection  . "#ebdbb2")  ;; Gruvbox Light selection
          (selection-fg . "#3c3836")  ;; Gruvbox Light selection fg
          (black      . "#fbf1c7")  ;; Gruvbox Light black
          (red        . "#cc241d")  ;; Gruvbox Light red
          (green      . "#98971a")  ;; Gruvbox Light green
          (yellow     . "#d79921")  ;; Gruvbox Light yellow
          (blue       . "#458588")  ;; Gruvbox Light blue
          (magenta    . "#b16286")  ;; Gruvbox Light magenta
          (cyan       . "#689d6a")  ;; Gruvbox Light cyan
          (white      . "#a89984")  ;; Gruvbox Light white
          (br-black   . "#928374")  ;; Gruvbox Light comment
          (br-red     . "#9d0006")  ;; Gruvbox Light bright red
          (br-green   . "#79740e")  ;; Gruvbox Light bright green
          (br-yellow  . "#b57614")  ;; Gruvbox Light bright yellow
          (br-blue    . "#076678")  ;; Gruvbox Light bright blue
          (br-magenta . "#8f3f71")  ;; Gruvbox Light bright magenta
          (br-cyan    . "#427b58")  ;; Gruvbox Light bright cyan
          (br-white   . "#3c3836")  ;; Gruvbox Light bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key gruvboxlight-colors)) 'unspecified))))

  (custom-theme-set-faces
   'gruvboxlight

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

   ;; Font lock faces - carefully tuned for Gruvbox Light style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'br-blue)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'br-green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'br-green)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'br-yellow) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'br-white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'br-red) :weight bold))))))

(provide-theme 'gruvboxlight)