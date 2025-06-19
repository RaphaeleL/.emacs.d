(deftheme doomone
  "Doom One theme - A dark theme inspired by Doom Emacs.")

(let* ((class '((class color) (min-colors 89)))
       (doomone-colors
        '(
          (bg         . "#282c34")  ;; Doom One background
          (fg         . "#bbc2cf")  ;; Doom One foreground
          (cursor     . "#bbc2cf")  ;; Doom One cursor
          (cursor-txt . "#282c34")  ;; Doom One cursor text
          (selection  . "#3e4451")  ;; Doom One selection
          (selection-fg . "#bbc2cf")  ;; Doom One selection fg
          (black      . "#282c34")  ;; Doom One black
          (red        . "#ff6c6b")  ;; Doom One red
          (green      . "#98be65")  ;; Doom One green
          (yellow     . "#ecbe7b")  ;; Doom One yellow
          (blue       . "#51afef")  ;; Doom One blue
          (magenta    . "#c678dd")  ;; Doom One magenta
          (cyan       . "#46d9ff")  ;; Doom One cyan
          (white      . "#dfdfdf")  ;; Doom One white
          (br-black   . "#5b6268")  ;; Doom One comment
          (br-red     . "#ff6c6b")  ;; Doom One bright red
          (br-green   . "#98be65")  ;; Doom One bright green
          (br-yellow  . "#ecbe7b")  ;; Doom One bright yellow
          (br-blue    . "#51afef")  ;; Doom One bright blue
          (br-magenta . "#c678dd")  ;; Doom One bright magenta
          (br-cyan    . "#46d9ff")  ;; Doom One bright cyan
          (br-white   . "#ffffff")  ;; Doom One bright white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key doomone-colors)) 'unspecified))))

  (custom-theme-set-faces
   'doomone

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

   ;; Font lock faces - carefully tuned for Doom One style
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

(provide-theme 'doomone)