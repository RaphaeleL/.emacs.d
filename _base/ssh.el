; ==================================================
; ====== SSH =======================================
; ==================================================

(setq tramp-default-method "ssh")
(setq tramp-ssh-controlmaster-options "")
(setq tramp-verbose 10)
(defun open-devvm ()
  (interactive)
  (find-file "/plink:raliccia@10.26.15.66:/home/raliccia/"))
(defun open-ksbuild8 ()
  (interactive)
  (find-file "/plink:raliccia@ksbuild8:/home_fco/raliccia/"))
(global-set-key (kbd "C-x C-r") 'open-devvm)
(global-set-key (kbd "C-x C-e") 'open-ksbuild8)
