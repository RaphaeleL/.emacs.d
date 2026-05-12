(defvar efs/file-name-handler-alist-original file-name-handler-alist
  "Backup of `file-name-handler-alist' before startup optimisations.")

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defvar efs/gc-cons-threshold (* 64 1024 1024)) ; 64MB steady-state
(defvar efs/gc-cons-percentage 0.1)
(defvar efs/last-focus-time (current-time))

(defun efs/restore-startup-settings ()
  (setq gc-cons-threshold efs/gc-cons-threshold
        gc-cons-percentage efs/gc-cons-percentage
        file-name-handler-alist efs/file-name-handler-alist-original)
  (garbage-collect))

(defun efs/focus-gc ()
  (let ((now (current-time)))
    (when (and (not (frame-focus-state)) (> (float-time (time-subtract now efs/last-focus-time)) 30))
      (setq efs/last-focus-time now)
      (garbage-collect))))

(defun efs/minibuffer-setup-hook () (setq gc-cons-threshold most-positive-fixnum))
(defun efs/minibuffer-exit-hook () (setq gc-cons-threshold efs/gc-cons-threshold))
(defun efs/handle-memory-pressure ()
  (when (and (fboundp 'memory-info)
             (let* ((stats (memory-info)) (used (/ (float (nth 3 stats)) (nth 2 stats)))) (> used 0.9)))
    (message "High memory usage, collecting garbage...")
    (garbage-collect)))

(add-hook 'emacs-startup-hook #'efs/restore-startup-settings)
(add-hook 'minibuffer-setup-hook #'efs/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'efs/minibuffer-exit-hook)
(run-with-idle-timer 60 t #'garbage-collect)

(when (fboundp 'add-function) (add-function :after after-focus-change-function #'efs/focus-gc))
(when (fboundp 'memory-info) (run-with-idle-timer 300 t #'efs/handle-memory-pressure))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))
