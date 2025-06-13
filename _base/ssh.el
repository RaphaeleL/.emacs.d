; ==================================================
; ====== SSH =======================================
; ==================================================

(setq tramp-default-method "plink")
(setq tramp-verbose 10)
(setq tramp-ssh-controlmaster-options "")

(defun ksbuild8 ()
  (interactive)
  (progn
    (find-file "/plink:raliccia@ksbuild8:/home_fco/raliccia/")
    (find-file "/plink:raliccia@ksbuild8:/home_fco/raliccia/paths.txt")
    (dired "/plink:raliccia@ksbuild8:/home1/products/ccms/kickstarts/raliccia/")
    (dired "/plink:raliccia@ksbuild8:/home1/tmp/raliccia/")
    (dired "/plink:raliccia@ksbuild8:/home1/kickstart_iso/nis/ccms/rlicciar/")))

(defun nikos ()
  (interactive)
  (progn
    (find-file "/plink:root@testvm1:/")
    (dired "/plink:root@testvm1:/var/log/ccms/")
    (dired "/plink:root@testvm1:/usr/lib/python3.12/site-packages/ccms/")
    (dired "/plink:root@testvm1:/etc/ccms/")))

(defun zeudi ()
  (interactive)
  (progn
    (find-file "/plink:root@testvm2:/")
    (dired "/plink:root@testvm2:/var/log/ccms/")
    (dired "/plink:root@testvm2:/usr/lib/python3.12/site-packages/ccms/")
    (dired "/plink:root@testvm2:/etc/ccms/")))

(defun giulia ()
  (interactive)
  (progn
    (find-file "/plink:root@testvm3:/")
    (dired "/plink:root@testvm3:/var/log/ccms/")
    (dired "/plink:root@testvm3:/usr/lib/python3.12/site-packages/ccms/")
    (dired "/plink:root@testvm3:/etc/ccms/")))

(defun sander ()
  (interactive)
  (progn
    (find-file "/plink:root@testvm4:/")
    (dired "/plink:root@testvm4:/var/log/ccms/")
    (dired "/plink:root@testvm4:/usr/lib/python3.12/site-packages/ccms/")
    (dired "/plink:root@testvm4:/etc/ccms/")))

(defun paola ()
  (interactive)
  (progn
    (find-file "/plink:root@testvm5:/")
    (dired "/plink:root@testvm5:/var/log/ccms/")
    (dired "/plink:root@testvm5:/usr/lib/python3.12/site-packages/ccms/")
    (dired "/plink:root@testvm5:/etc/ccms/")))

(global-set-key (kbd "C-c 1") 'nikos)
(global-set-key (kbd "C-c 2") 'zeudi)
(global-set-key (kbd "C-c 3") 'giulia)
(global-set-key (kbd "C-c 4") 'sander)
(global-set-key (kbd "C-c 5") 'paola)
(global-set-key (kbd "C-c 8") 'ksbuild8)

(when (eq system-type 'windows-nt)
  (defun enzo () (interactive) (find-file "/plink:raliccia@10.26.15.66:/home/raliccia/"))
  (global-set-key (kbd "C-c 0") 'enzo))
