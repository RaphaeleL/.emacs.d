; ==================================================
; ===== HOOKS ======================================
; ==================================================

;; (add-hook 'c-mode-hook             'eglot-ensure)
;; (add-hook 'c++-mode-hook           'eglot-ensure)
;; (add-hook 'python-mode-hook        'eglot-ensure)

(add-hook 'after-init-hook         'global-company-mode)

(add-hook 'text-mode-hook          'rc/turn-on-paredit)

(add-hook 'c-mode-hook             'rc/on_save)
(add-hook 'c++-mode-hook           'rc/on_save)
(add-hook 'simpc-mode-hook         'rc/on_save)
(add-hook 'python-mode-hook        'rc/on_save)
(add-hook 'markdown-mode-hook      'rc/on_save)
(add-hook 'text-mode-hook          'rc/on_save)
(add-hook 'emacs-lisp-mode-hook    'rc/on_save)
(add-hook 'jenkinsfile-mode-hook   'rc/on_save)
(add-hook 'dockerfile-mode-hook    'rc/on_save)
(add-hook 'makefile-mode-hook      'rc/on_save)
(add-hook 'rpm-spec-mode-hook      'rc/on_save)
