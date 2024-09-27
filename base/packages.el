(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'top)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-lah")
  (setq ls-lisp-ignore-case t))

