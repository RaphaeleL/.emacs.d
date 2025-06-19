(deftheme atom
  "Atom theme - Inspired by the Atom editor's default dark theme.")

(let* ((class '((class color) (min-colors 89)))
       (atom-colors
        '(
          (bg         . "#161719")  ;; Atom background
          (fg         . "#c5c8c6")  ;; Atom foreground
          (cursor     . "#b5bd68")  ;; Main color: green
          (cursor-txt . "#161719")  ;; Atom cursor text
          (selection  . "#373b41")  ;; Atom selection
          (selection-fg . "#c5c8c6")  ;; Atom selection fg
          (black      . "#161719")  ;; Atom black
          (red        . "#cc6666")  ;; Atom red
          (green      . "#b5bd68")  ;; Atom green - MAIN COLOR
          (yellow     . "#f0c674")  ;; Atom yellow
          (blue       . "#81a2be")  ;; Atom blue
          (magenta    . "#b294bb")  ;; Atom magenta
          (cyan       . "#8abeb7")  ;; Atom cyan
          (white      . "#c5c8c6")  ;; Atom white
          (br-black   . "#969896")  ;; Atom comment
          (br-red     . "#cc6666")  ;; Atom bright red
          (br-green   . "#b5bd68")  ;; Atom bright green
          (br-yellow  . "#f0c674")  ;; Atom bright yellow
          (br-blue    . "#81a2be")  ;; Atom bright blue
          (br-magenta . "#b294bb")  ;; Atom bright magenta
          (br-cyan    . "#8abeb7")  ;; Atom bright cyan
          (br-white   . "#ffffff")  ;; Atom bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key atom-colors)) 'unspecified))))

  (custom-theme-set-faces
   'atom

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

   ;; Font lock faces - carefully tuned for Atom style
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

   ;; Modeline faces - Atom style
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

(provide-theme 'atom)