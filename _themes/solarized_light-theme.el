(deftheme solarized_light
  "Solarized light theme with toned down, less flashy colors.")

(let* ((class '((class color) (min-colors 89)))
       (solarized_light-colors
        '((bg         . "#fef9e6")  ;; slightly lighter and softer background
          (fg         . "#6b7c82")  ;; softer gray-blue foreground
          (cursor     . "#6b7c82")  ;; match fg for cursor
          (cursor-txt . "#f3ecd7")  ;; soft cream for cursor text
          (selection  . "#f3ecd7")  ;; very soft selection bg
          (selection-fg . "#526668") ;; muted selection fg
          (black      . "#173c48")  ;; softened black (dark blue-gray)
          (red        . "#c94e48")  ;; toned down red
          (green      . "#798c31")  ;; softened green
          (yellow     . "#a98f23")  ;; toned down mustard yellow
          (blue       . "#4a8ccf")  ;; softer blue
          (magenta    . "#b85575")  ;; toned down magenta
          (cyan       . "#339088")  ;; softened cyan
          (white      . "#f3ecd7")  ;; soft off-white
          (br-black   . "#1f383f")  ;; soft dark slate
          (br-red     . "#b6633d")  ;; toned down br-red
          (br-green   . "#526668")  ;; muted br-green
          (br-yellow  . "#6b7c82")  ;; muted br-yellow (same as fg)
          (br-blue    . "#7a80b5")  ;; soft periwinkle blue
          (br-magenta . "#7a80b5")  ;; same as br-blue
          (br-cyan    . "#7c8d8a")  ;; soft teal gray
          (br-white   . "#fef9e6")))

       (safe-get-color (lambda (key)
                         (or (cdr (assoc key solarized_light-colors)) 'unspecified))))

  (custom-theme-set-faces
   'solarized_light

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
