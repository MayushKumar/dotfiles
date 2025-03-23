(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (package-initialize)

;; (require 'use-package)
;; (setq use-package-always-ensure t)

(setq backup-directory-alist '(("." . "~/.cache/emacs-backup")))
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)

(defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))


(add-hook 'after-load-theme-hook
		  (lambda ()
			(let ((error-underline (face-attribute 'flymake-error :underline nil t))
				  (warning-underline (face-attribute 'flymake-warning :underline nil t)))
			  (when (listp error-underline)
				(setq error-underline (plist-get error-underline :color)))
			  (when (listp warning-underline)
				(setq warning-underline (plist-get warning-underline :color)))
			  (set-face-attribute 'flymake-error nil :underline `(:color ,error-underline :style dashes))
			  (set-face-attribute 'flymake-warning nil :underline `(:color ,warning-underline :style dashes)))))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(set-default-coding-systems 'utf-8)
(prefer-coding-system       'utf-8)
(set-terminal-coding-system 'utf-8)

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 3)

(setq default-directory "~/")
(save-place-mode 1)
(electric-pair-mode 1)

(add-hook 'Man-mode-hook (lambda() (display-line-numbers-mode 0)))

(setq-default tab-width 4)

;; (setq scroll-margin 4)
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 6)))

;; (setq pixel-scroll-precision-mode t)

;; (global-hl-line-mode)
(setq-default truncate-lines t)
(blink-cursor-mode 0)

(setq frame-resize-pixelwise t)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-alh")

;; (set-face-attribute 'default nil :family "0xProto Nerd Font" :weight 'medium :height 110)
;; (set-face-attribute 'fixed-pitch nil :family "0xProto Nerd Font" :weight 'medium :height 110)
(set-face-attribute 'default nil :family "CommitMono Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :family "CommitMono Nerd Font" :height 120)
(set-face-attribute 'variable-pitch nil :family "Inter" :height 170)

(defun mk/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha-background value))

(mk/transparency 80)

;; (add-hook 'server-after-make-frame-hook (lambda () (mk/transparency 97)))

(defun mk/set-line-spacing (value)
  "Sets the line spacing"
  (interactive "nValue: ")
  (setq-default line-spacing value))

(use-package ultra-scroll
  :ensure `(ultra-scroll
			:host github 
			:repo "jdtsmith/ultra-scroll")
  :config
  (ultra-scroll-mode 1))

(use-package all-the-icons)
(elpaca-wait)

(use-package all-the-icons-dired
	:config
	(add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package nerd-icons)

;; (use-package dashboard
  ;;   :config
  ;;   (setq dashboard-startup-banner "~/.config/emacs/cat.png")
  ;;   (setq dashboard-set-heading-icons t)
  ;;   (setq dashboard-set-file-icons t)
  ;;   (setq dashboard-items '((recents  . 5)
  ;;                           (projects . 5)
  ;;                           (registers . 5)))
  ;;   (setq dashboard-center-content t)
  ;;   (setq dashboard-set-footer nil)
  ;;   (set-face-attribute 'dashboard-items-face nil :weight 'normal)

  ;;   (setq initial-buffer-choice (lambda () (dashboard-refresh-buffer)(get-buffer "*dashboard*")))
  ;;   (dashboard-setup-startup-hook))


(use-package dashboard
  :init
  (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
  (setq dashboard-startup-banner "~/.config/emacs/cat.webp")
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-vertically-center-content t)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-flat)
;;   (setq telephone-line-primary-right-separator 'telephone-line-flat)
;;   (setq telephone-line-secondary-left-separator 'telephone-line-flat)
;;   (setq telephone-line-secondary-right-separator 'telephone-line-flat)
;;   (telephone-line-mode 1)
;;   )

;; (setq-default mode-line-format
;;   '("%e" mode-line-front-space (:eval                                
;;     (moody-ribbon evil-mode-line-tag 0 'up))
;;    (:propertize
;;     (" " mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;     display
;;     (min-width
;;      (5.0)))
;;    mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;    "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(column-number-mode)
(setq-default mode-line-percent-position nil)

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function)
;;   (setq moody-mode-line-height 22))

;; (use-package minions
;;   :config
;;   (minions-mode))

(setq custom-safe-themes t)

(add-to-list 'load-path "~/.config/emacs/themes/")
(load "gruvbox-material-hard-theme")
(load "everforest-dark-hard-theme")

(use-package doom-themes :no-require t
  :config
  (setq doom-themes-enable-bold nil)
  (setq doom-themes-enable-italic nil))

(use-package kaolin-themes :no-require t)

(use-package uwu-theme :no-require t
  :config
  (setq uwu-distinct-line-numbers nil))

(use-package base16-theme :no-require t
  :custom
  (base16-distinct-fringe-background nil))

(use-package ef-themes)

(use-package apropospriate-theme)

;; (elpaca-wait)

;; (use-package mini-ontop
;;   :ensure t
;;   :config (mini-ontop-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        ;; evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-undo-system 'undo-redo
        evil-insert-state-cursor 'box
        evil-visual-state-cursor 'hollow
        evil-respect-visual-line-mode t
        evil-want-minibuffer t
        evil-mode-line-format nil

        evil-normal-state-tag   (propertize " ⏺ " 'face '((:foreground "MediumTurquoise")))
        evil-emacs-state-tag    (propertize " ⏺ " 'face '((:foreground "BlueViolet")))
        evil-insert-state-tag   (propertize " ⏺ " 'face '((:foreground "Orchid")))
        evil-replace-state-tag  (propertize " ⏺ " 'face '((:foreground "Red3")))
        evil-motion-state-tag   (propertize " ⏺ " 'face '((:foreground "OrangeRed3")))
        evil-visual-state-tag   (propertize " ⏺ " 'face '((:foreground "Gold2")))
        evil-operator-state-tag (propertize " ⏺ " 'face '((:foreground "RoyalBlue"))))
  :config
  (evil-mode 1)
  (evil-global-set-key 'normal (kbd "U") 'evil-redo))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-snipe
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; (use-package meow
;;   :config
;;   (defun meow-setup ()
;; 	(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;; 	(meow-motion-overwrite-define-key
;; 	 '("j" . meow-next)
;; 	 '("k" . meow-prev)
;; 	 '("<escape>" . ignore))
;; 	(meow-leader-define-key
;; 	 ;; SPC j/k will run the original command in MOTION state.
;; 	 '("j" . "H-j")
;; 	 '("k" . "H-k")
;; 	 ;; Use SPC (0-9) for digit arguments.
;; 	 '("1" . meow-digit-argument)
;; 	 '("2" . meow-digit-argument)
;; 	 '("3" . meow-digit-argument)
;; 	 '("4" . meow-digit-argument)
;; 	 '("5" . meow-digit-argument)
;; 	 '("6" . meow-digit-argument)
;; 	 '("7" . meow-digit-argument)
;; 	 '("8" . meow-digit-argument)
;; 	 '("9" . meow-digit-argument)
;; 	 '("0" . meow-digit-argument)
;; 	 '("/" . meow-keypad-describe-key)
;; 	 '("?" . meow-cheatsheet))
;; 	(meow-normal-define-key
;; 	 '("0" . meow-expand-0)
;; 	 '("9" . meow-expand-9)
;; 	 '("8" . meow-expand-8)
;; 	 '("7" . meow-expand-7)
;; 	 '("6" . meow-expand-6)
;; 	 '("5" . meow-expand-5)
;; 	 '("4" . meow-expand-4)
;; 	 '("3" . meow-expand-3)
;; 	 '("2" . meow-expand-2)
;; 	 '("1" . meow-expand-1)
;; 	 '("-" . negative-argument)
;; 	 '(";" . meow-reverse)
;; 	 '("," . meow-inner-of-thing)
;; 	 '("." . meow-bounds-of-thing)
;; 	 '("[" . meow-beginning-of-thing)
;; 	 '("]" . meow-end-of-thing)
;; 	 '("a" . meow-append)
;; 	 '("A" . meow-open-below)
;; 	 '("b" . meow-back-word)
;; 	 '("B" . meow-back-symbol)
;; 	 '("c" . meow-change)
;; 	 '("d" . meow-delete)
;; 	 '("D" . meow-backward-delete)
;; 	 '("e" . meow-next-word)
;; 	 '("E" . meow-next-symbol)
;; 	 '("f" . meow-find)
;; 	 '("g" . meow-cancel-selection)
;; 	 '("G" . meow-grab)
;; 	 '("h" . meow-left)
;; 	 '("H" . meow-left-expand)
;; 	 '("i" . meow-insert)
;; 	 '("I" . meow-open-above)
;; 	 '("j" . meow-next)
;; 	 '("J" . meow-next-expand)
;; 	 '("k" . meow-prev)
;; 	 '("K" . meow-prev-expand)
;; 	 '("l" . meow-right)
;; 	 '("L" . meow-right-expand)
;; 	 '("m" . meow-join)
;; 	 '("n" . meow-search)
;; 	 '("o" . meow-block)
;; 	 '("O" . meow-to-block)
;; 	 '("p" . meow-yank)
;; 	 '("q" . meow-quit)
;; 	 '("Q" . meow-goto-line)
;; 	 '("r" . meow-replace)
;; 	 '("R" . meow-swap-grab)
;; 	 '("s" . meow-kill)
;; 	 '("t" . meow-till)
;; 	 '("u" . meow-undo)
;; 	 '("U" . meow-undo-in-selection)
;; 	 '("v" . meow-visit)
;; 	 '("w" . meow-mark-word)
;; 	 '("W" . meow-mark-symbol)
;; 	 '("x" . meow-line)
;; 	 '("X" . meow-goto-line)
;; 	 '("y" . meow-save)
;; 	 '("Y" . meow-sync-grab)
;; 	 '("z" . meow-pop-selection)
;; 	 '("'" . repeat)
;; 	 '("<escape>" . ignore)))
;;   (meow-setup)
;;   (meow-global-mode 1))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 10)
  (setq evil-complete-next-minibuffer-func 'vertico-next
        evil-complete-previous-minibuffer-func 'vertico-previous))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  (set-face-attribute 'orderless-match-face-0 nil :weight 'normal)
  (set-face-attribute 'orderless-match-face-1 nil :weight 'normal)
  (set-face-attribute 'orderless-match-face-2 nil :weight 'normal)
  (set-face-attribute 'orderless-match-face-3 nil :weight 'normal))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
    ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
    ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package vterm :commands (vterm)
  :config
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0))))

(use-package ace-window
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 1.0)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)
  (setq aw-ignore-on nil))

(use-package avy)

;; (use-package persp-mode
;;   :config
;;   (with-eval-after-load "persp-mode-autoloads"
;; 	(setq persp-autokill-buffer-on-remove 'kill-weak)
;; 	(add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
;;   )

(use-package transient)
(use-package magit :commands (magit magit-status))

(use-package treemacs
	:config
	(treemacs-resize-icons 16)
	(treemacs-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode t)
	;; (treemacs-git-mode 'deferred)
	;; (setq doom-themes-treemacs-theme "doom-atom")
	;; (doom-themes-treemacs-config)
	(setq treemacs-width-is-initially-locked nil)
	(load "treemacs-theme.el")
	(treemacs-load-theme 'mk/treemacs-theme))

(use-package treemacs-evil
	:after (treemacs evil))

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode 0)))

(use-package rg)

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-function)
  (global-set-key (kbd "C-h c") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h o") #'helpful-symbol)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package which-key
  :config
  (which-key-mode))

(use-package rainbow-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(defun mk/launch-emacs-daemon-and-client ()
  (call-process "sh" nil nil nil "-c" "emacs --daemon && emacsclient -c &"))

(defun mk/restart-emacs-daemon ()
  "Restart Emacs daemon and launch a new client."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list 'mk/launch-emacs-daemon-and-client))))
    (save-buffers-kill-emacs)))

(global-set-key (kbd "<f12>") 'mk/restart-emacs-daemon)
(global-set-key (kbd "<f11>") 'save-buffers-kill-emacs)

(defun mk-indent-buffer ()
  "Indent the buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun mk-divider (str len)
  "Insert a divider on the current line"
  (interactive "sEnter the string for the divider: \nnEnter the length for the divider: ")
  (dotimes (_ len)
    (insert str))
  (insert "  ")
  (dotimes (_ len)
    (insert str))
  (backward-char (1+ (* len (length str))))
  (evil-insert-state))

(setq-default c-basic-offset 4)
(setq-default c-default-style "bsd")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

(setq c-ts-mode-indent-offset 4)
(setq c-ts-mode-indent-style 'bsd)

(use-package lua-mode)

(use-package rust-mode)

(use-package cmake-font-lock)

(use-package glsl-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode))))

(use-package yaml-mode)

(use-package latex
  :ensure (auctex :pre-build (("./autogen.sh")
							  ("./configure"
							   "--without-texmf-dir")
							  ("make")))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package kotlin-mode)

;; (use-package company
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1)
;;   ;; (add-hook 'after-init-hook 'global-company-mode)
;;   )

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)
  :config
  (keymap-unset corfu-map "RET"))

;; A few more useful configurations...
;; (use-package emacs
;;   :init
;; TAB cycle if there are only few candidates
;; (setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;; (setq read-extended-command-predicate
;;       #'command-completion-default-include-p)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
;; (setq tab-always-indent 'complete))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (plist-put kind-icon-default-style :height 0.9))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line) ;; NOTE: The order matters!
  )


(use-package nova
  :ensure (:host github :repo "thisisran/nova"))

;; (use-package flycheck)

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :config
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;   (set-face-attribute 'tree-sitter-hl-face:property 'nil :slant 'normal)
;;   (set-face-attribute 'tree-sitter-hl-face:function.call 'nil :inherit '(default)))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :custom
  (projectile-enable-caching t)
  (projectile-track-known-projects-automatically nil))

(use-package yasnippet
  :config
  (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :hook
;;   ;;  (c++-mode . lsp-deferred)
;;   (rust-mode . lsp-deferred)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-completion-mode . my/lsp-mode-setup-completion)

;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))) ;; Configure orderless

;;   :custom
;;   (lsp-completion-provider :none)

;;   :config
;;   (lsp-enable-which-key-integration)

;;   (setq lsp-headerline-breadcrumb-enable nil
;;         lsp-enable-symbol-highlighting nil
;;         lsp-enable-links nil
;;         lsp-modeline-code-actions-enable nil
;;         lsp-log-io nil
;;         lsp-enable-folding nil
;;         lsp-enable-imenu nil
;;         lsp-eldoc-enable-hover nil)

;;   ;; LSP Booster
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;; 	"Try to parse bytecode instead of json."
;; 	(or
;; 	 (when (equal (following-char) ?#)
;;        (let ((bytecode (read (current-buffer))))
;; 		 (when (byte-code-function-p bytecode)
;;            (funcall bytecode))))
;; 	 (apply old-fn args)))
;;   (advice-add (if (progn (require 'json)
;; 						 (fboundp 'json-parse-buffer))
;;                   'json-parse-buffer
;; 				'json-read)
;;               :around
;;               #'lsp-booster--advice-json-parse)

;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;; 	"Prepend emacs-lsp-booster command to lsp CMD."
;; 	(let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?)                             ;; for check lsp-server-present?
;;                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;                lsp-use-plists
;;                (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;                (executable-find "emacs-lsp-booster"))
;;           (progn
;; 			(message "Using emacs-lsp-booster for %s!" orig-result)
;; 			(cons "emacs-lsp-booster" orig-result))
;; 		orig-result)))
;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;;   :commands
;;   (lsp lsp-deferred))

;; (use-package lsp-treemacs :after lsp)

;; (use-package lsp-ui :after lsp
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-show-with-cursor nil)
;;   (lsp-ui-doc-show-with-mouse nil)
;;   (lsp-ui-sideline-enable nil))

;; (use-package consult-lsp)


;;;;;;;;;;;;;;;;;;;;;;;;;;; eglot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot-booster :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :inlayHintProvider :documentHighlightProvider))
  (eglot-booster-mode))

(use-package consult-eglot :after eglot)

;; (use-package eglot)

;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-bridge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-bridge
;;   :config
;;   (add-to-list 'load-path "~/.config/emacs/straight/repos/lsp-bridge/")
;;   (global-lsp-bridge-mode))

;; (add-to-list 'load-path "~/dev/lsp-bridge/")
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

(setq lsp-clients-clangd-args '("--header-insertion=never" "--completion-style=detailed"))

(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(locals controls tooltip))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (require 'dap-codelldb)
  (require 'dap-lldb))


(use-package dape :ensure (:host github :repo "svaante/dape") :commands (dape)
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  ;; :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

;; (use-package visual-fill-column)
(use-package mixed-pitch
  :config
  (setq mixed-pitch-set-height t))

(use-package org :commands (org-mode)
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  ;; (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-image-actual-width nil)
  (setq org-pretty-entities t)
  (add-to-list 'org-latex-packages-alist
               '("" "chemfig" t))
  (setq org-preview-latex-default-process 'dvisvgm)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; (mixed-pitch-mode)
              (visual-line-mode)
              (setq visual-fill-column-center-text t)
              (setq fill-column 140)
              (display-line-numbers-mode 0)
              ;; (visual-fill-column-mode)
              ;; (company-mode 0)
              ;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
              ;; (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
              ;; (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
              ;; (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
              (setq-local evil-normal-state-cursor '(bar . 1))
              (setq-local evil-insert-state-cursor '(bar . 1)))))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t))

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;   (setq org-bullets-bullet-list '("•")))
(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package ox-gfm)

(use-package general
  :config
  ;; (general-evil-setup)

  (general-define-key
   :states '(normal visual)
   :prefix "SPC"

   "p p" 'projectile-switch-project
   "p f" 'projectile-find-file
   "p s" 'projectile-save-project-buffers
   "p a" 'projectile-find-other-file
   "p e" 'projectile-find-other-file-other-window
   "p i" 'projectile-invalidate-cache
   "p k" 'projectile-kill-buffers)

  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"

   "x"   'execute-extended-command

   "f f" 'find-file
   "f s" 'save-buffer
   "f r" 'rename-visited-file

   "c b" 'consult-bookmark

   "b"   'consult-buffer

   "k" 'kill-current-buffer
   "K" 'kill-buffer

   "s"   'consult-line
   "S r" 'rg

   "w"   'ace-window
   "o"   'other-window
   "0"   'delete-window

   "h v" 'helpful-variable
   "h f" 'helpful-function
   "h k" 'helpful-key
   "h o" 'helpful-symbol
   "h p" 'helpful-at-point
   "h F" 'describe-face

   "t t" 'treemacs)

  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'dap-mode-map

   "l d d" 'dap-debug
   "l d b" 'dap-breakpoint-toggle
   "l d h" 'dap-hydra)

  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'lsp-mode-map

   "l d"   'lsp-find-declaration
   "l g"   'lsp-find-definition
   "l i"   'lsp-find-implementation
   "l r"   'lsp-find-references
   "l R"   'lsp-rename
   "l s"   'consult-lsp-symbols
   "l q"   'lsp-workspace-shutdown)

  (general-define-key
   :prefix "SPC"
   :states '(normal visual)
   :keymaps 'eglot-mode-map

   "l d"   'eglot-find-declaration
   "l g"   'eglot-find-typeDefinition
   "l i"   'eglot-find-implementation
   "l r"   'xref-find-references
   "l R"   'eglot-rename
   "l s"   'consult-eglot-symbols
   "l q"   'eglot-shutdown)

  (general-define-key
   :prefix ","
   :states '(normal visual)
   :keymaps 'org-mode-map

   "t" 'org-babel-tangle)

  (general-define-key
   :prefix ","
   :states '(normal)
   :keymaps '(lisp-mode-map lisp-interaction-mode-map emacs-lisp-mode-map)

   "e e" 'eval-last-sexp
   "e b" 'eval-buffer)

  (general-define-key
   :prefix ","
   :states '(visual)
   :keymaps '(lisp-mode-map lisp-interaction-mode-map emacs-lisp-mode-map)

   "e" 'eval-region)


  (general-define-key
   :prefix ","
   :states '(normal)
   :keymaps '(LaTeX-mode-map)

   "c" 'TeX-command-master)

  (general-define-key
   :states '(normal visual)

   "C-=" 'mk-indent-buffer))
