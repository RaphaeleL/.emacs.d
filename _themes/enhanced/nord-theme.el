(deftheme nord
  "Nord theme - An arctic, north-bluish color palette.")

(let* ((class '((class color) (min-colors 89)))
       (nord-colors
        '(
          (bg         . "#2e3440")  ;; Nord background
          (fg         . "#eceff4")  ;; Nord foreground
          (cursor     . "#5e81ac")  ;; Main color: blue
          (cursor-txt . "#2e3440")  ;; Nord cursor text
          (selection  . "#434c5e")  ;; Nord selection
          (selection-fg . "#eceff4")  ;; Nord selection fg
          (black      . "#2e3440")  ;; Nord black
          (red        . "#bf616a")  ;; Nord red
          (green      . "#a3be8c")  ;; Nord green
          (yellow     . "#ebcb8b")  ;; Nord yellow
          (blue       . "#5e81ac")  ;; Nord blue - MAIN COLOR
          (magenta    . "#b48ead")  ;; Nord magenta
          (cyan       . "#88c0d0")  ;; Nord cyan
          (white      . "#eceff4")  ;; Nord white
          (br-black   . "#4c566a")  ;; Nord comment
          (br-red     . "#bf616a")  ;; Nord bright red
          (br-green   . "#a3be8c")  ;; Nord bright green
          (br-yellow  . "#ebcb8b")  ;; Nord bright yellow
          (br-blue    . "#5e81ac")  ;; Nord bright blue
          (br-magenta . "#b48ead")  ;; Nord bright magenta
          (br-cyan    . "#88c0d0")  ;; Nord bright cyan
          (br-white   . "#eceff4")  ;; Nord bright white
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

   ;; Line number faces - main color for current line
   `(line-number ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Nord style
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
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'br-white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))

   ;; Modeline faces - Nord style
   `(mode-line ((,class (:background ,(funcall safe-get-color 'bg)
                                     :foreground ,(funcall safe-get-color 'fg)))))
   `(mode-line-inactive ((,class (:background ,(funcall safe-get-color 'black)
                                              :foreground ,(funcall safe-get-color 'br-black)))))
   `(mode-line-highlight ((,class (:background ,(funcall safe-get-color 'blue)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))
   `(mode-line-emphasis ((,class (:weight bold))))
   `(mode-line-buffer-id ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
))

(provide-theme 'nord)