(package-initialize)

(add-to-list 'load-path "~/.emacs.d/local/")

(load "~/.emacs.d/local/packages.el")
(load "~/.emacs.d/local/keymaps.el")
(load "~/.emacs.d/local/settings.el")
(load "~/.emacs.d/local/utils.el")
(load "~/.emacs.d/local/ui.el")
(load "~/.emacs.d/local/lsp.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
