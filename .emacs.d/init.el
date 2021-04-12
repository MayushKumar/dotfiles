
;; ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;; █████╗  ██╔████╔██║███████║██║     ███████╗
;; ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;; ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;; ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

(setq gc-cons-threshold 100000000)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes :no-require t
  ;; :config (setq doom-gruvbox-dark-variant "hard")
  )

(use-package magit)
(use-package glsl-mode)
(use-package lua-mode)
 
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  )
(use-package rainbow-delimiters)
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(use-package tree-sitter)
(use-package tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package treemacs
  :config
  (treemacs-resize-icons 18)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  )

(use-package treemacs-evil
  :after treemacs evil
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)
        ("C-x t C-t" . treemacs-find-file))
  )

(use-package helpful
  :config
  ;; (global-set-key (kbd "C-h f") #'helpful-callable)
  ;; (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

;; ivy
(use-package counsel)
(use-package swiper)
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-h o") 'counsel-describe-symbol)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  )

(use-package evil
  :config
  (evil-mode 1)
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-function 'undo-redo)
  (setq evil-default-cursor 'box)
  (setq evil-insert-state-cursor 'box)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  )

;; company mode
(use-package company
  :after lsp-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  )

;; flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  )

;; lsp-mode
(use-package lsp-mode
  :hook
  (c++-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-symbol-highlighting nil)

  :commands
  (lsp lsp-deferred)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor nil)
  )

(use-package lsp-ivy)

;; Language Server specific
(setq lsp-clients-clangd-args '("--header-insertion=never"))

(use-package which-key
    :config
    (which-key-mode))

;; dap-mode
(use-package dap-mode
  :config
  (require 'dap-cpptools)
  (require 'dap-lldb)
  )

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-enable-caching t)
  )
(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  )

;; doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; cmake-mode
(use-package cmake-mode)

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode 0)))

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)
;;(global-hl-line-mode t)

;; Code related settings
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(electric-pair-mode)
(setq scroll-margin 10)
(setq scroll-conservatively 101)

(set-face-attribute 'default nil :family "Jet Brains Mono Slashed" :height 90)
(set-face-attribute 'variable-pitch nil :family "Roboto" :height 90)
(add-hook 'prog-mode-hook (lambda() (toggle-truncate-lines)))
;; Pruning away some stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell -1)
;; (setq inhibit-startup-message t)
;; (setq initial-scratch-message nil)

(defun my/resize-margins ()
  (let ((margin-size (/ (- (window-width) 150) 2)))
    (set-window-margins nil (max 0 margin-size) (max 0 margin-size))))

(add-hook 'window-state-change-hook (lambda ()
										   (when (derived-mode-p 'prog-mode) (my/resize-margins))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#2a2426" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#cc241d" "#8ec07c" "#ebdbb2"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-enabled-themes '(doom-gruvbox))
 '(custom-safe-themes
   '("dc4f9ecd3b83846f4ca356cbb14cca211d2e23beee9fc2b4cadff83ea347c27e" "0caad7549e17e2a0e32301ba7aba91b570a2b4154f57e23cef73d1c18a7251c5" "18e1bb35a0cabdd523dd555bb3905a128fb488decfdcf0a969bbc530f3d0ba1a" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" default))
 '(dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
 '(fci-rule-color "#7c6f64")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
	 ("TODO" . "#feacd0")
	 ("NEXT" . "#b6a0ff")
	 ("THEM" . "#f78fe7")
	 ("PROG" . "#00d3d0")
	 ("OKAY" . "#4ae8fc")
	 ("DONT" . "#80d200")
	 ("FAIL" . "#ff8059")
	 ("BUG" . "#ff8059")
	 ("DONE" . "#44bc44")
	 ("NOTE" . "#f0ce43")
	 ("KLUDGE" . "#eecc00")
	 ("HACK" . "#eecc00")
	 ("TEMP" . "#ffcccc")
	 ("FIXME" . "#ff9977")
	 ("XXX+" . "#f4923b")
	 ("REVIEW" . "#6ae4b9")
	 ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#0d1011" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d1011" "#b8bb26"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d1011" "#928374"))
 '(lsp-ui-doc-position 'bottom nil nil "Customized with use-package lsp-ui")
 '(lsp-ui-doc-show-with-cursor nil nil nil "Customized with use-package lsp-ui")
 '(objed-cursor-color "#fb4934")
 '(package-selected-packages
   '(doom-themes rainbow-mode evil-commentary helpful 2048-game cmake-mode doom-modeline counsel-projectile projectile dap-mode which-key lsp-ivy lsp-ui lsp-mode flycheck company evil-collection ivy-rich counsel treemacs-evil treemacs multiple-cursors rainbow-delimiters drag-stuff lua-mode glsl-mode magit use-package))
 '(pdf-view-midnight-colors (cons "#ebdbb2" "#1d2021"))
 '(rustic-ansi-faces
   ["#1d2021" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#cc241d" "#8ec07c" "#ebdbb2"])
 '(vc-annotate-background "#1d2021")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
	(cons 20 "#b8bb26")
	(cons 40 "#cebb29")
	(cons 60 "#e3bc2c")
	(cons 80 "#fabd2f")
	(cons 100 "#fba827")
	(cons 120 "#fc9420")
	(cons 140 "#fe8019")
	(cons 160 "#ed611a")
	(cons 180 "#dc421b")
	(cons 200 "#cc241d")
	(cons 220 "#db3024")
	(cons 240 "#eb3c2c")
	(cons 260 "#fb4934")
	(cons 280 "#e05744")
	(cons 300 "#c66554")
	(cons 320 "#ac7464")
	(cons 340 "#7c6f64")
	(cons 360 "#7c6f64")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "#ffffff"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "#fb4934"))))
 '(flycheck-info ((t (:underline "#8ec07c"))))
 '(flycheck-warning ((t (:underline "#fabd2f")))))
