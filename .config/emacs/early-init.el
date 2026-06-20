;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Startup -------------------------------------------------------------------

;; Don't let package.el initialize itself. Elpaca handles packages.
(setq package-enable-at-startup nil)

;; Speed up startup by delaying GC.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Restore sane GC values once Emacs has started.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 64 1024 1024)
         gc-cons-percentage 0.1)))

;;; Native compilation --------------------------------------------------------

(setq native-comp-async-report-warnings-errors nil)

;;; Environment ---------------------------------------------------------------

;; Faster plist decoding for LSP servers.
(setenv "LSP_USE_PLISTS" "true")

;;; UI ------------------------------------------------------------------------

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)

(setq ring-bell-function #'ignore)

(add-to-list 'default-frame-alist
           '(internal-border-width . 14))

;; Avoid expensive frame resizing during startup.
(setq frame-inhibit-implied-resize t)

;; Don't initialize package.el.
(setq package-quickstart nil)

(provide 'early-init)
