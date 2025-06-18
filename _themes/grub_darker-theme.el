(deftheme grub_darker
  "Grub Darker theme generated from given palette.")

(let* ((class '((class color) (min-colors 89)))
       (grub-darker-colors
        '((bg          . "#181818")
          (fg          . "#e4e4e4")
          (cursor      . "#ffdd33")
          (cursor-txt  . "#ffffff")
          (selection   . "#ffffff")
          (selection-fg . "#52494e")
          (black       . "#181818")
          (red         . "#f43841")
          (green       . "#73d936")
          (yellow      . "#ffdd33")
          (blue        . "#96a6c8")
          (magenta     . "#9e95c7")
          (cyan        . "#95a99f")
          (white       . "#e4e4e4")
          (br-black    . "#52494e")
          (br-red      . "#ff4f58")
          (br-green    . "#73d936")
          (br-yellow   . "#ffdd33")
          (br-blue     . "#96a6c8")
          (br-magenta  . "#afafd7")
          (br-cyan     . "#95a99f")
          (br-white    . "#f5f5f5")))

       (safe-get-color (lambda (key)
                         (or (cdr (assoc key grub-darker-colors)) 'unspecified))))

  (custom-theme-set-faces
   'grub_darker

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

(provide-theme 'grub_darker)
