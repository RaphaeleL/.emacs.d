(deftheme xcodedark
  "Theme inspired by Xcode Dark, Apple's premium IDE dark theme.")

(let* ((class '((class color) (min-colors 89)))
       (xcodedark-colors
        '(
          (bg         . "#1F1F24")  ;; Xcode dark background
          (fg         . "#FFFFFF")  ;; Pure white text
          (cursor     . "#4EB0CC")  ;; Main color: blue
          (cursor-txt . "#1F1F24")  ;; Dark background
          (selection  . "#515158")  ;; Dark gray selection
          (selection-fg . "#FFFFFF")  ;; White selection text
          (black      . "#1F1F24")  ;; Base dark
          (red        . "#FF8A7A")  ;; Salmon red
          (green      . "#78C2B3")  ;; Seafoam green
          (yellow     . "#FFD479")  ;; Warm yellow
          (blue       . "#4EB0CC")  ;; Xcode blue - MAIN COLOR
          (magenta    . "#FF7AB2")  ;; Pink
          (cyan       . "#78C2B3")  ;; Seafoam
          (white      . "#FFFFFF")  ;; Pure white
          (br-black   . "#8E8E93")  ;; Comment gray
          (br-red     . "#FF8A7A")  ;; Bright salmon
          (br-green   . "#78C2B3")  ;; Bright seafoam
          (br-yellow  . "#FFD479")  ;; Bright yellow
          (br-blue    . "#4EB0CC")  ;; Bright blue
          (br-magenta . "#FF7AB2")  ;; Bright pink
          (br-cyan    . "#78C2B3")  ;; Bright seafoam
          (br-white   . "#FFFFFF")  ;; Pure white
          ))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key xcodedark-colors)) 'unspecified))))

  (custom-theme-set-faces
   'xcodedark

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
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'br-red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))

   ;; Line number faces - main color for current line
   `(line-number ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-current-line ((,class (:foreground ,(funcall safe-get-color 'blue) :weight bold))))
   `(line-number-major-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(line-number-minor-tick ((,class (:foreground ,(funcall safe-get-color 'br-black)))))

   ;; Font lock faces - carefully tuned for Xcode Dark style
   `(font-lock-builtin-face ((,class (:foreground ,(funcall safe-get-color 'magenta)))))
   `(font-lock-comment-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,(funcall safe-get-color 'br-black)))))
   `(font-lock-constant-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-doc-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,(funcall safe-get-color 'br-black) :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,(funcall safe-get-color 'blue)))))
   `(font-lock-keyword-face ((,class (:foreground ,(funcall safe-get-color 'magenta) :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-string-face ((,class (:foreground ,(funcall safe-get-color 'red)))))
   `(font-lock-type-face ((,class (:foreground ,(funcall safe-get-color 'br-magenta)))))
   `(font-lock-variable-name-face ((,class (:foreground ,(funcall safe-get-color 'br-white)))))
   `(font-lock-warning-face ((,class (:foreground ,(funcall safe-get-color 'red) :weight bold))))

   ;; Modeline faces - Xcode Dark style
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

(provide-theme 'xcodedark)