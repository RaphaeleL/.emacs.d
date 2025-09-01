(load "~/.emacs.d/_base/lsp.el")
(load "~/.emacs.d/_base/ssh.el")

;; Theme
(lr/enable-custom-font-iosevka)
(lr/load-theme 'gruberdarker)

(use-package emacs
  :ensure nil
  :bind (("C-y" . yank)
         ("C-w" . clipboard-kill-ring-save)))
