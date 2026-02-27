(setq tramp-default-method "plink")
(setq tramp-verbose 10)
(setq tramp-ssh-controlmaster-options "")

;; (defun enzo () (interactive)
;;   (progn
;;     (dired "/plink:raliccia@10.26.15.66:/home/raliccia/")
;;     (dired "/plink:raliccia@10.26.15.66:/home/raliccia/dev/fco/ccms_dev")
;;     (dired "/plink:raliccia@10.26.15.66:/home/raliccia/dev/fco/ccms-performance")))

;; (defun ksbuild8 ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:raliccia@ksbuild8:/home_fco/raliccia/")
;;     (dired "/plink:raliccia@ksbuild8:/home_fco/raliccia/paths.txt")
;;     (dired "/plink:raliccia@ksbuild8:/home1/tmp/raliccia/")
;;     (dired "/plink:raliccia@ksbuild8:/home1/kickstart_iso/nis/ccms/rlicciar/")
;; 	(dired "/plink:raliccia@ksbuild8:/home1/products/ccms/kickstarts/raliccia/")))

;; (defun nikos ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:root@testvm1:/var/log/ccms/")
;;     (dired "/plink:root@testvm1:/usr/share/ccms/ansible/")
;;     (dired "/plink:root@testvm1:/usr/lib/python3.12/site-packages/ccms/")
;;     (dired "/plink:root@testvm1:/etc/ccms/")
;;     (dired "/plink:root@testvm1:/")))

;; (defun zeudi ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:root@testvm2:/var/log/ccms/")
;;     (dired "/plink:root@testvm2:/usr/share/ccms/ansible/")
;;     (dired "/plink:root@testvm2:/usr/lib/python3.12/site-packages/ccms/")
;;     (dired "/plink:root@testvm2:/etc/ccms/")
;;     (dired "/plink:root@testvm2:/")))

;; (defun giulia ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:root@testvm3:/var/log/ccms/")
;;     (dired "/plink:root@testvm3:/usr/share/ccms/ansible/")
;;     (dired "/plink:root@testvm3:/usr/lib/python3.12/site-packages/ccms/")
;;     (dired "/plink:root@testvm3:/etc/ccms/")
;;     (dired "/plink:root@testvm3:/")))

;; (defun sander ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:root@testvm4:/var/log/ccms/")
;;     (dired "/plink:root@testvm4:/usr/share/ccms/ansible/")
;;     (dired "/plink:root@testvm4:/usr/lib/python3.12/site-packages/ccms/")
;;     (dired "/plink:root@testvm4:/etc/ccms/")
;;     (dired "/plink:root@testvm4:/")))

;; (defun paola ()
;;   (interactive)
;;   (progn
;;     (dired "/plink:root@testvm5:/var/log/ccms/")p
;;     (dired "/plink:root@testvm5:/usr/share/ccms/ansible/")
;;     (dired "/plink:root@testvm5:/usr/lib/python3.12/site-packages/ccms/")
;;     (dired "/plink:root@testvm5:/etc/ccms/")
;;     (dired "/plink:root@testvm5:/")))

;; (global-set-key (kbd "C-c 0") 'enzo)
;; (global-set-key (kbd "C-c 1") 'nikos)
;; (global-set-key (kbd "C-c 2") 'zeudi)
;; (global-set-key (kbd "C-c 3") 'giulia)
;; (global-set-key (kbd "C-c 4") 'sander)
;; (global-set-key (kbd "C-c 5") 'paola)
;; (global-set-key (kbd "C-c 8") 'ksbuild8)
