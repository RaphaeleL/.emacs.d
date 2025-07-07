;; =============================================================================
;; GARBAGE COLLECTION & PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; garbage.el \u2014 performance tuning

(defvar file-name-handler-alist-original file-name-handler-alist
  "Original `file-name-handler-alist` to restore after startup.")

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defvar efs/gc-cons-threshold 800000  ; ~0.8MB
  "Normal value for `gc-cons-threshold` after startup.")

(defvar efs/gc-cons-percentage 0.1
  "Normal value for `gc-cons-percentage` after startup.")

(defvar efs/last-focus-time (current-time)
  "Last time focus was lost.")

(defun efs/restore-startup-settings ()
  "Restore GC and file handler settings after startup."
  (setq gc-cons-threshold efs/gc-cons-threshold
        gc-cons-percentage efs/gc-cons-percentage)
  (setq file-name-handler-alist file-name-handler-alist-original)
  (when (boundp 'file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original))
  (garbage-collect))

(defun efs/focus-gc ()
  "Run GC if Emacs has been unfocused for more than 30 seconds."
  (let ((now (current-time)))
    (when (and (not (frame-focus-state))
               (> (float-time (time-subtract now efs/last-focus-time)) 30))
      (setq efs/last-focus-time now)
      (garbage-collect))))

(defun efs/minibuffer-setup-hook ()
  "Increase GC threshold while minibuffer is active."
  (setq gc-cons-threshold most-positive-fixnum))

(defun efs/minibuffer-exit-hook ()
  "Restore normal GC threshold after minibuffer use."
  (setq gc-cons-threshold efs/gc-cons-threshold))

(defun efs/handle-memory-pressure ()
  "Monitor memory usage and GC aggressively if needed."
  (when (and (fboundp 'memory-info)
             (let* ((stats (memory-info))
                    (used (/ (float (nth 3 stats)) (nth 2 stats))))
               (> used 0.9)))
    (message "High memory usage, collecting garbage...")
    (garbage-collect)))

(add-hook 'emacs-startup-hook #'efs/restore-startup-settings)
(add-hook 'minibuffer-setup-hook #'efs/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'efs/minibuffer-exit-hook)
(run-with-idle-timer 60 t #'garbage-collect)

(when (fboundp 'add-function)
  (add-function :after after-focus-change-function #'efs/focus-gc))

(when (fboundp 'memory-info)
  (run-with-idle-timer 300 t #'efs/handle-memory-pressure))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))
