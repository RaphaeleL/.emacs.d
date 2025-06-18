(deftheme solarized_light
  "solarized_light color theme for Emacs, based on your palette.")

(let* ((class '((class color) (min-colors 89)))
       (solarized_light-colors
        '((bg         . "#fdf6e3")
          (fg         . "#657b83")
          (cursor     . "#657b83")
          (cursor-txt . "#eee8d5")
          (selection  . "#eee8d5")
          (selection-fg . "#586e75")
          (black      . "#073642")
          (red        . "#dc322f")
          (green      . "#859900")
          (yellow     . "#b58900")
          (blue       . "#268bd2")
          (magenta    . "#d33682")
          (cyan       . "#2aa198")
          (white      . "#eee8d5")
          (br-black   . "#002b36")
          (br-red     . "#cb4b16")
          (br-green   . "#586e75")
          (br-yellow  . "#657b83")
          (br-blue    . "#6c71c4")
          (br-magenta . "#6c71c4")
          (br-cyan    . "#93a1a1")
          (br-white   . "#fdf6e3")))

       ;; Helper to safely get color or 'unspecified
       (safe-get-color (lambda (key)
                         (or (cdr (assoc key solarized_light-colors)) 'unspecified))))

  (custom-theme-set-faces
   'solarized_light

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
   `(show-paren-match ((,class (:background ,(funcall safe-get-color 'br-yellow)
                                            :foreground ,(funcall safe-get-color 'bg)
                                            :weight bold))))
   `(show-paren-mismatch ((,class (:background ,(funcall safe-get-color 'br-red)
                                               :foreground ,(funcall safe-get-color 'bg)
                                               :weight bold))))))

(provide-theme 'solarized_light)
