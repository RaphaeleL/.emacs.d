;; =============================================================================
;; GARBAGE COLLECTION & PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; Store original values for restoration after startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(defvar gc-cons-threshold-original gc-cons-threshold)
(defvar gc-cons-percentage-original gc-cons-percentage)

;; Disable file name handlers during startup for faster loading
(setq file-name-handler-alist nil)

;; Disable garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Optimized GC thresholds for different scenarios
(defvar gc-cons-threshold-normal 134217728)    ; 128MB
(defvar gc-cons-threshold-high 268435456)      ; 256MB
(defvar gc-cons-threshold-low 67108864)        ; 64MB

;; Startup optimization hook
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore file name handlers
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)
            
            ;; Set normal GC threshold
            (setq gc-cons-threshold gc-cons-threshold-normal)
            (setq gc-cons-percentage 0.6)
            
            ;; Force initial garbage collection
            (garbage-collect)
            
            ;; Clean up startup variables
            (makunbound 'gc-cons-threshold-original)
            (makunbound 'gc-cons-percentage-original)))

;; Smart focus-based garbage collection
(defvar efs/last-focus-time (current-time))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (let ((now (current-time)))
                                  ;; Only GC if we've been unfocused for more than 30 seconds
                                  (when (and (not (frame-focus-state))
                                             (> (float-time (time-subtract now efs/last-focus-time)) 30))
                                    (setq efs/last-focus-time now)
                                    (garbage-collect)))))
              (add-hook 'after-focus-change-function 
                        (lambda ()
                          (let ((now (current-time)))
                            (when (and (not (frame-focus-state))
                                       (> (float-time (time-subtract now efs/last-focus-time)) 30))
                              (setq efs/last-focus-time now)
                              (garbage-collect))))))))

;; Minibuffer performance optimization
(add-hook 'emacs-startup-hook
          (lambda ()
            (defun gc-minibuffer-setup-hook ()
              "Disable GC during minibuffer operations for better responsiveness"
              (setq gc-cons-threshold most-positive-fixnum))

            (defun gc-minibuffer-exit-hook ()
              "Restore GC and collect garbage after minibuffer operations"
              (garbage-collect)
              (setq gc-cons-threshold gc-cons-threshold-normal))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; Enhanced startup time reporting
(defun efs/display-startup-time ()
  "Display enhanced startup time information"
  (condition-case err
      (let* ((startup-time (if (and (boundp 'after-init-time) (boundp 'before-init-time))
                               (float-time (time-subtract after-init-time before-init-time))
                             0.0)))
        (message "Emacs loaded in %.2f seconds with %d garbage collections."
                 startup-time gcs-done))
    (error
     (message "Emacs loaded with %d garbage collections" gcs-done))))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Additional performance optimizations
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Optimize for better performance
            (setq read-process-output-max (* 64 1024)) ; 64KB
            (setq process-adaptive-read-buffering t)
            
            ;; Reduce redisplay frequency
            (setq redisplay-dont-pause t)
            
            ;; Optimize font rendering
            (setq inhibit-compacting-font-caches t)
            
            ;; Disable some expensive features during startup
            (setq auto-save-default nil)
            (setq auto-save-visited-file-name nil)))

;; Memory pressure handling
(defun efs/handle-memory-pressure ()
  "Handle memory pressure by triggering garbage collection"
  (when (> (car (memory-info)) (* 512 1024)) ; If memory > 512MB
    (garbage-collect)))

;; Periodic memory cleanup (every 5 minutes)
(run-with-idle-timer 300 t #'efs/handle-memory-pressure)

;; Startup completion hook for final optimizations
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 1 nil
                                (lambda ()
                                  ;; Final cleanup after startup
                                  (garbage-collect)))))

;; Provide the module
(provide 'garbage)
