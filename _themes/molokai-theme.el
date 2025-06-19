(deftheme molokai
  "Molokai theme - A dark, vibrant theme inspired by Vim's Molokai.")

(let* ((class '((class color) (min-colors 89)))
       (molokai-colors
        '(
          (bg         . "#1B1D1E")  ;; Darker background than Monokai
          (fg         . "#F8F8F2")  ;; Off-white text
          (cursor     . "#F92672")  ;; Main color: magenta
          (cursor-txt . "#1B1D1E")  ;; Match background
          (selection  . "#403D3D")  ;; Dark brown selection
          (selection-fg . "#F8F8F2")  ;; Off-white selection text
          (black      . "#1B1D1E")  ;; Base dark
          (red        . "#FF0000")  ;; Pure red for errors
          (green      . "#A6E22E")  ;; Bright green for functions
          (yellow     . "#E6DB74")  ;; Light yellow for strings
          (blue       . "#66D9EF")  ;; Cyan-blue for types
          (magenta    . "#F92672")  ;; Pink-red for keywords - MAIN COLOR
          (cyan       . "#A1EFE4")  ;; Light cyan
          (white      . "#F8F8F2")  ;; Off-white
          (br-black   . "#465457")  ;; Gray for comments
          (br-red     . "#FF0000")  ;; Same as red
          (br-green   . "#A6E22E")  ;; Same as green
          (br-yellow  . "#FD971F")  ;; Orange for constants
          (br-blue    . "#66D9EF")  ;; Same as blue
          (br-magenta . "#F92672")  ;; Same as magenta
          (br-cyan    . "#66EFD5")  ;; Brighter cyan
          (br-white   . "#FFFFFF")  ;; Pure white
          (purple     . "#AE81FF")  ;; Purple for constants
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key molokai-colors)) 'unspecified))))

  (custom-theme-set-faces
   'molokai

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

   ;; Line number faces - main color for current line
   `(line-number ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Molokai style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'magenta)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'purple)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'green)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'br-yellow)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'yellow)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'blue) :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))))

(provide-theme 'molokai)