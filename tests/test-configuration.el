;;; test-configuration.el -*- lexical-binding: t; -*-
;; Unit tests for Lira Emacs configuration

(require 'ert)

;; === TEST SETUP ================================
(defun lira-test-setup ()
  "Setup for running tests."
  (load-file "~/.emacs.d/init.el"))

;; === MODULE LOADING TESTS ======================
(ert-deftest test-module-loading ()
  "Test that all modules load without errors."
  (should (not (condition-case err
                   (progn
                     (load-file "~/.emacs.d/init.el")
                     nil)
                 (error
                  (message "Module loading failed: %s" (error-message-string err))
                  err)))))

(ert-deftest test-utilities-module ()
  "Test that utilities module provides required functions."
  (lira-test-setup)
  (should (fboundp 'lira-create-keymap))
  (should (fboundp 'lira-create-keymap-m))
  (should (fboundp 'lira-create-keymap-c))
  (should (fboundp 'lira-delete-line))
  (should (fboundp 'lira-duplicate-line))
  (should (fboundp 'lira-cut))
  (should (fboundp 'lira-copy))
  (should (fboundp 'lira-paste))
  (should (fboundp 'lira-toggle-buffer))
  (should (fboundp 'lira-reload)))

(ert-deftest test-completion-module ()
  "Test that completion module provides required functions."
  (lira-test-setup)
  (should (fboundp 'lira-toggle-mini-buffer-mode))
  (should (fboundp 'lira-fido-minibuffer-setup)))

(ert-deftest test-ui-module ()
  "Test that UI module provides required functions."
  (lira-test-setup)
  (should (fboundp 'lira-load-theme))
  (should (fboundp 'lira-load-theme-safe))
  (should (fboundp 'lira-enable-custom-font-safe)))

(ert-deftest test-os-module ()
  "Test that OS module provides required functions."
  (lira-test-setup)
  (should (fboundp 'lira-enable-custom-font))
  (should (fboundp 'lira-disable-custom-font))
  (should (fboundp 'lira-get-default-font)))

;; === KEYBINDING TESTS ==========================
(ert-deftest test-keymap-helper-functions ()
  "Test that keymap helper functions work correctly."
  (lira-test-setup)
  ;; Test that helper functions can create keybindings
  (let ((test-func (lambda () (message "test"))))
    (lira-create-keymap "<f12>" test-func)
    (should (eq (lookup-key global-map (kbd "<f12>")) test-func))
    ;; Clean up
    (define-key global-map (kbd "<f12>") nil)))

(ert-deftest test-user-keybindings ()
  "Test that user keybindings are properly set."
  (lira-test-setup)
  (should (fboundp (lookup-key global-map (kbd "<f1>"))))
  (should (fboundp (lookup-key global-map (kbd "<f2>"))))
  (should (fboundp (lookup-key global-map (kbd "<f3>"))))
  (should (fboundp (lookup-key global-map (kbd "<f4>"))))
  (should (fboundp (lookup-key global-map (kbd "<f5>"))))
  (should (fboundp (lookup-key global-map (kbd "<f6>"))))
  (should (fboundp (lookup-key global-map (kbd "<f7>"))))
  (should (fboundp (lookup-key global-map (kbd "<f8>")))))

(ert-deftest test-macos-keybindings ()
  "Test that MacOS keybindings are properly set."
  (lira-test-setup)
  (when (eq system-type 'darwin)
    (should (fboundp (lookup-key global-map (kbd "M-8"))))
    (should (fboundp (lookup-key global-map (kbd "M-9"))))
    (should (fboundp (lookup-key global-map (kbd "M-["))))
    (should (fboundp (lookup-key global-map (kbd "M-]"))))))

;; === SSH CONFIGURATION TESTS ===================
(ert-deftest test-ssh-functions ()
  "Test that SSH functions are available."
  (lira-test-setup)
  (should (fboundp 'nikos))
  (should (fboundp 'zeudi))
  (should (fboundp 'giulia))
  (should (fboundp 'sander))
  (should (fboundp 'paola))
  (should (fboundp 'ksbuild8)))

(ert-deftest test-ssh-keybindings ()
  "Test that SSH keybindings are set."
  (lira-test-setup)
  (should (fboundp (lookup-key global-map (kbd "C-c 1"))))
  (should (fboundp (lookup-key global-map (kbd "C-c 2"))))
  (should (fboundp (lookup-key global-map (kbd "C-c 3"))))
  (should (fboundp (lookup-key global-map (kbd "C-c 4"))))
  (should (fboundp (lookup-key global-map (kbd "C-c 5"))))
  (should (fboundp (lookup-key global-map (kbd "C-c 8")))))

;; === UTILITY FUNCTION TESTS ====================
(ert-deftest test-lira-delete-line ()
  "Test lira-delete-line function."
  (lira-test-setup)
  (with-temp-buffer
    (insert "line 1\nline 2\nline 3")
    (goto-char (point-min))
    (forward-line 1) ; Move to line 2
    (lira-delete-line)
    (should (equal (buffer-string) "line 1\nline 3"))))

(ert-deftest test-lira-duplicate-line ()
  "Test lira-duplicate-line function."
  (lira-test-setup)
  (with-temp-buffer
    (insert "original line\n")
    (goto-char (point-min))
    (lira-duplicate-line)
    (should (equal (buffer-string) "original line\noriginal line\n"))))

(ert-deftest test-lira-toggle-buffer ()
  "Test lira-toggle-buffer function."
  (lira-test-setup)
  (let ((test-buffer "*test-buffer*"))
    ;; Test creating new buffer
    (lira-toggle-buffer test-buffer)
    (should (get-buffer test-buffer))
    ;; Test toggling existing buffer
    (lira-toggle-buffer test-buffer)
    ;; Clean up
    (kill-buffer test-buffer)))

;; === ERROR HANDLING TESTS ======================
(ert-deftest test-missing-module-handling ()
  "Test that missing modules are handled gracefully."
  (let ((original-os-file "~/.emacs.d/modules/os.el")
        (backup-file "~/.emacs.d/modules/os.el.backup"))
    ;; Backup original file
    (when (file-exists-p original-os-file)
      (copy-file original-os-file backup-file t))
    ;; Remove the file temporarily
    (when (file-exists-p original-os-file)
      (delete-file original-os-file))
    
    ;; Test that configuration still loads
    (should (not (condition-case err
                     (progn
                       (load-file "~/.emacs.d/init.el")
                       nil)
                   (error err))))
    
    ;; Restore original file
    (when (file-exists-p backup-file)
      (copy-file backup-file original-os-file t)
      (delete-file backup-file))))

(ert-deftest test-missing-user-config ()
  "Test that missing user config is handled gracefully."
  (let ((original-init-file "~/.config/lira/init.el")
        (backup-file "~/.config/lira/init.el.backup"))
    ;; Backup original file
    (when (file-exists-p original-init-file)
      (copy-file original-init-file backup-file t))
    ;; Remove the file temporarily
    (when (file-exists-p original-init-file)
      (delete-file original-init-file))
    
    ;; Test that configuration still loads
    (should (not (condition-case err
                     (progn
                       (load-file "~/.emacs.d/init.el")
                       nil)
                   (error err))))
    
    ;; Restore original file
    (when (file-exists-p backup-file)
      (copy-file backup-file original-init-file t)
      (delete-file backup-file))))

;; === THEME LOADING TESTS =======================
(ert-deftest test-theme-loading-functions ()
  "Test theme loading functions."
  (lira-test-setup)
  ;; Test that theme functions exist
  (should (fboundp 'lira-load-theme))
  (should (fboundp 'lira-load-theme-safe))
  
  ;; Test safe theme loading with invalid theme
  (should (not (condition-case err
                   (lira-load-theme-safe 'non-existent-theme)
                   nil
                 (error t)))))

;; === COMPLETION MODE TESTS =====================
(ert-deftest test-completion-mode-toggle ()
  "Test completion mode toggle function."
  (lira-test-setup)
  (should (fboundp 'lira-toggle-mini-buffer-mode))
  
  ;; Test that the function can be called without error
  (should (not (condition-case err
                   (lira-toggle-mini-buffer-mode)
                   nil
                 (error t)))))

;; === FONT FUNCTION TESTS =======================
(ert-deftest test-font-functions ()
  "Test font-related functions."
  (lira-test-setup)
  (should (fboundp 'lira-font-increase))
  (should (fboundp 'lira-font-decrease))
  (should (fboundp 'lira-enable-custom-font))
  (should (fboundp 'lira-disable-custom-font))
  
  ;; Test that font functions can be called
  (should (not (condition-case err
                   (lira-font-increase)
                   nil
                 (error t))))
  (should (not (condition-case err
                   (lira-font-decrease)
                   nil
                 (error t)))))

;; === INTEGRATION TESTS =========================
(ert-deftest test-full-configuration-load ()
  "Test that the full configuration loads correctly."
  (should (not (condition-case err
                   (progn
                     (load-file "~/.emacs.d/init.el")
                     ;; Verify key components are available
                     (should (fboundp 'lira-create-keymap))
                     (should (fboundp 'lira-load-theme))
                     (should (fboundp 'nikos))
                     (should (lookup-key global-map (kbd "<f1>")))
                     nil)
                 (error err)))))

;; === PERFORMANCE TESTS =========================
(ert-deftest test-loading-performance ()
  "Test that configuration loads within reasonable time."
  (let ((start-time (current-time)))
    (load-file "~/.emacs.d/init.el")
    (let ((load-time (float-time (time-subtract (current-time) start-time))))
      (message "Configuration loaded in %.2f seconds" load-time)
      ;; Should load in under 5 seconds
      (should (< load-time 5.0)))))

;; === RUN ALL TESTS =============================
(defun lira-run-all-tests ()
  "Run all Lira configuration tests."
  (interactive)
  (ert-run-tests-interactively "test-.*")) 