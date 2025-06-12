; ==================================================
; ====== SSH =======================================
; ==================================================

(setq tramp-default-method "plink")
(setq tramp-verbose 10)
(setq tramp-ssh-controlmaster-options "")

(defun ksbuild8 () (interactive) (find-file "/plink:raliccia@ksbuild8:/home_fco/raliccia/"))

(defun nikos () (interactive) (find-file "/plink:root@testvm1:/"))
(defun zeudi () (interactive) (find-file "/plink:root@testvm2:/"))
(defun giulia () (interactive) (find-file "/plink:root@testvm3:/"))
(defun sander () (interactive) (find-file "/plink:root@testvm4:/"))
(defun paola () (interactive) (find-file "/plink:root@testvm5:/"))

(global-set-key (kbd "C-c 1") 'nikos)
(global-set-key (kbd "C-c 2") 'zeudi)
(global-set-key (kbd "C-c 3") 'giulia)
(global-set-key (kbd "C-c 4") 'sander)
(global-set-key (kbd "C-c 5") 'paola)
(global-set-key (kbd "C-c 8") 'ksbuild8)

(when (eq system-type 'windows-nt)
  (defun enzo () (interactive) (find-file "/plink:raliccia@10.26.15.66:/home/raliccia/"))
  (global-set-key (kbd "C-c 0") 'enzo))
