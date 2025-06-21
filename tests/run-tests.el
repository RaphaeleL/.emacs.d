;;; run-tests.el -*- lexical-binding: t; -*-
;; Test runner for Lira Emacs configuration

;; Add tests directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load test file
(load "test-configuration.el")

;; Run all tests
(ert-run-tests-batch-and-exit) 