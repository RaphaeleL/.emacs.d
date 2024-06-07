(package-initialize)

(add-to-list 'load-path "~/.emacs.d/local/")

(load "~/.emacs.d/local/init.el")
(load "~/.emacs.d/local/helpers.el")
(load "~/.emacs.d/local/settings.el")
(load "~/.emacs.d/local/ui.el")
(load "~/.emacs.d/local/lsp.el")
(load "~/.emacs.d/local/keymaps.el")
(load "~/.emacs.d/local/package-settings.el")

;; ---------------------------------------------------------------------------------
;; ---------------------------------------------------------------------------------

(custom-set-variables
 '(custom-safe-themes
   '("f079ef5189f9738cf5a2b4507bcaf83138ad22d9c9e32a537d61c9aae25502ef" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "e27c9668d7eddf75373fa6b07475ae2d6892185f07ebed037eedf783318761d7" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1" "d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992" "e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "4b026ac68a1vaa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" default))
 '(package-selected-packages
   '(yasnippet company tree-sitter hydra go-mode rainbow-mode rust-mode markdown-mode ## zenburn-theme super-save smex simpleclip multiple-cursors move-text mood-line magit lsp-pyright hide-mode-line helm-xref helm-lsp gruber-darker-theme format-all doom-themes)))
(custom-set-faces

)

