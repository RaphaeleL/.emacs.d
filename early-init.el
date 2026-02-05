;; Disable UI noise early
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Disable implicit package loading
(setq package-enable-at-startup nil)

;; GC sanity during startup
(setq gc-cons-threshold (* 50 1024 1024))

