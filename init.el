(cond
 ((eq system-type 'windows-nt) (load "~/.emacs.d/_templates/init.windows.el"))
 ((eq system-type 'darwin) (load "~/.emacs.d/_templates/init.mac.el"))
 ((eq system-type 'gnu/linux) (load "~/.emacs.d/_templates/init.linux.el")))
