;;; harpoon-emacs.el --- A small Harpoon-inspired quick-file nav for Emacs
;; Author: ChatGPT (adapted)
;; Version: 0.2
;; Keywords: convenience, files, navigation
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/you/harpoon-emacs

;;; Commentary:
;; A lightweight Emacs port of "Harpoon" (the Neovim plugin) for quickly
;; marking files and jumping between them.

;;; Code:

(defgroup harpoon-emacs nil
  "Quick file marks and navigation inspired by Harpoon (neovim)."
  :group 'convenience)

(defcustom harpoon-emacs-file (expand-file-name "harpoon-emacs-list.el" user-emacs-directory)
  "File where harpoon marks are persisted." :type 'file :group 'harpoon-emacs)

(defvar harpoon-emacs--marks nil
  "List of file paths marked by harpoon-emacs.")

(defun harpoon-emacs--save ()
  (with-temp-file harpoon-emacs-file
    (insert ";; harpoon-emacs persisted marks\n")
    (prin1 harpoon-emacs--marks (current-buffer))))

(defun harpoon-emacs--load ()
  (when (file-exists-p harpoon-emacs-file)
    (with-temp-buffer
      (insert-file-contents harpoon-emacs-file)
      (let ((data (read (current-buffer))))
        (when (listp data) (setq harpoon-emacs--marks data))))))

(defun harpoon-emacs--normalize (path)
  (expand-file-name path))

;;;###autoload
(defun harpoon-emacs-add-current-file ()
  (interactive)
  (let ((file (or (buffer-file-name) (user-error "Buffer has no file"))))
    (setq file (harpoon-emacs--normalize file))
    (setq harpoon-emacs--marks (delete file harpoon-emacs--marks))
    (push file harpoon-emacs--marks)
    (setq harpoon-emacs--marks (nreverse harpoon-emacs--marks))
    (harpoon-emacs--save)
    (message "Harpoon: added %s" file)))

(defun harpoon-emacs--goto-file (file)
  (find-file file))

;;;###autoload
(defun harpoon-emacs-go-to (index)
  (interactive "nHarpoon index (1-based): ")
  (harpoon-emacs--load)
  (let* ((len (length harpoon-emacs--marks))
         (idx (1- index)))
    (unless (and (>= idx 0) (< idx len))
      (user-error "No harpoon mark %d (have %d)" index len))
    (harpoon-emacs--goto-file (nth idx harpoon-emacs--marks))))

;;;###autoload
(defun harpoon-emacs-next ()
  (interactive)
  (harpoon-emacs--load)
  (if (null harpoon-emacs--marks) (user-error "No harpoon marks"))
  (let ((current (buffer-file-name)))
    (if (not current)
        (harpoon-emacs--goto-file (car harpoon-emacs--marks))
      (let* ((norm (harpoon-emacs--normalize current))
             (idx (cl-position norm harpoon-emacs--marks :test #'string=)))
        (if idx
            (let ((next (nth (mod (1+ idx) (length harpoon-emacs--marks)) harpoon-emacs--marks)))
              (harpoon-emacs--goto-file next))
          (harpoon-emacs--goto-file (car harpoon-emacs--marks)))))))

;;;###autoload
(defun harpoon-emacs-previous ()
  (interactive)
  (harpoon-emacs--load)
  (if (null harpoon-emacs--marks) (user-error "No harpoon marks"))
  (let ((current (buffer-file-name)))
    (if (not current)
        (harpoon-emacs--goto-file (car harpoon-emacs--marks))
      (let* ((norm (harpoon-emacs--normalize current))
             (idx (cl-position norm harpoon-emacs--marks :test #'string=)))
        (if idx
            (let ((prev (nth (mod (1- idx) (length harpoon-emacs--marks)) harpoon-emacs--marks)))
              (harpoon-emacs--goto-file prev))
          (harpoon-emacs--goto-file (car harpoon-emacs--marks)))))))

;;; Menu buffer
(defvar harpoon-emacs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'harpoon-emacs-open-at-point)
    (define-key map (kbd "d") #'harpoon-emacs-remove-at-point)
    map)
  "Keymap for `harpoon-emacs-list-mode'.")

(define-derived-mode harpoon-emacs-list-mode special-mode "Harpoon"
  "Major mode to list harpoon marks.")

(defun harpoon-emacs--render-menu ()
  (let ((i 0) (lines '()))
    (dolist (f harpoon-emacs--marks)
      (setq i (1+ i))
      (push (format "%2d: %s" i f) lines))
    (string-join (nreverse lines) "\n")))

;;;###autoload
(defun harpoon-emacs-toggle-menu ()
  (interactive)
  (harpoon-emacs--load)
  (let ((buf (get-buffer-create "*Harpoon*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if harpoon-emacs--marks
                    (harpoon-emacs--render-menu)
                  "(no harpoon marks)"))
        (goto-char (point-min))
        (harpoon-emacs-list-mode)))
    (pop-to-buffer buf)))

(defun harpoon-emacs--line-file-at-point ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[[:space:]]*\([0-9]+\):[[:space:]]*\(.*\)")
      (match-string 2))))

;;;###autoload
(defun harpoon-emacs-open-at-point ()
  (interactive)
  (let ((f (harpoon-emacs--line-file-at-point)))
    (if f (find-file f) (message "No file at point"))))

;;;###autoload
(defun harpoon-emacs-remove-at-point ()
  (interactive)
  (let ((f (harpoon-emacs--line-file-at-point)))
    (if (not f) (message "No mark at point")
      (setq harpoon-emacs--marks (delete f harpoon-emacs--marks))
      (harpoon-emacs--save)
      (harpoon-emacs-toggle-menu)
      (message "Removed %s" f))))

;;;###autoload
(defun harpoon-emacs-clear-all ()
  (interactive)
  (when (yes-or-no-p "Clear all harpoon marks? ")
    (setq harpoon-emacs--marks nil)
    (when (file-exists-p harpoon-emacs-file) (delete-file harpoon-emacs-file))
    (message "Harpoon: cleared all marks")))

;;;###autoload
(defun harpoon-emacs-remove-by-index (index)
  (interactive "Remove harpoon index: ")
  (harpoon-emacs--load)
  (let ((idx (1- index)))
    (if (or (< idx 0) (>= idx (length harpoon-emacs--marks)))
        (user-error "No such mark %d" index)
      (let ((f (nth idx harpoon-emacs--marks)))
        (setq harpoon-emacs--marks (delete f harpoon-emacs--marks))
        (harpoon-emacs--save)
        (message "Removed %s" f)))))

(add-hook 'emacs-startup-hook #'harpoon-emacs--load)

(provide 'harpoon-emacs)
;;; harpoon-emacs.el ends here
