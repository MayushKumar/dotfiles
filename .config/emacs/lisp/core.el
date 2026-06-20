;;; core.el --- Core -*- lexical-binding: t; -*-
(eval-when-compile (require 'bootstrap))

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

(setq-default truncate-lines t)
(blink-cursor-mode 0)

(setq frame-resize-pixelwise t)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-alh")


(defun my/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha-background value))

;; (mk/transparency 95)

;; (add-hook 'server-after-make-frame-hook (lambda () (mk/transparency 97)))

(defun my/set-line-spacing (value)
  "Sets the line spacing"
  (interactive "nValue: ")
  (setq-default line-spacing value))


(set-face-attribute 'default nil :family "CommitMono Nerd Font" :height 130)
(set-face-attribute 'fixed-pitch nil :family "CommitMono Nerd Font" :height 130)
(setq nerd-icons-font-family "CommitMono Nerd Font")
(set-face-attribute 'variable-pitch nil :family "Inter" :height 170)

;; (setq scroll-margin 4)
(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . 6)))

(use-package ultra-scroll
  :ensure `(ultra-scroll
			:host github 
			:repo "jdtsmith/ultra-scroll")
  :config
  (ultra-scroll-mode 1))

(use-package transient
  :commands (my-scrolling-zone)
  :config
  (transient-define-prefix my-scrolling-zone ()
	"A modifier-free zone triggered by SPC v using h j k l for viewport scrolling."
	[:description "Viewport Navigation"
				  ["Full Page"
				   ("l" "Page Down" scroll-up-command :transient t)
				   ("h" "Page Up"   scroll-down-command :transient t)]
				  ["Half Page"
				   ("j" "Half Page Down" (lambda () (interactive) (scroll-up (/ (window-body-height) 2))) :transient t)
				   ("k" "Half Page Up"   (lambda () (interactive) (scroll-down (/ (window-body-height) 2))) :transient t)]
				  ["Recenter"
				   ("c" "Center View" recenter-top-bottom :transient t)]
				  ["Quit"
				   ("<escape>" "Exit Zone" transient-quit-one)]]))

;; (use-package all-the-icons :defer t)

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons)

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  ; This is directly from the package. needed more space after the icon
  (defun my/nerd-icons-corfu-formatter (_)
  (and-let* ((kindfunc (plist-get completion-extra-properties :company-kind)))
    (lambda (cand)
      (let* ((kind (funcall kindfunc cand))
             (glyph (nerd-icons-corfu--get-by-kind kind cand)))
        (concat
         (and (display-graphic-p) (propertize " " 'display '(space :width 0.5)))
         glyph
         (propertize " " 'display '(space :width 1.0)))))))
  ;; (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (add-to-list 'corfu-margin-formatters #'my/nerd-icons-corfu-formatter))

(use-package dashboard
  :init
  ;; (setq dashboard-icon-type 'all-the-icons)
  ;; (setq dashboard-startup-banner "~/.config/emacs/cat.webp")
  (setq dashboard-startup-banner 'logo-braille)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-vertically-center-content t)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)
  (setq initial-buffer-choice 'dashboard-open)
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(column-number-mode)
(setq-default mode-line-percent-position nil)

  ;;; Themes

(setq custom-safe-themes t)

(add-to-list
 'custom-theme-load-path
 (expand-file-name "themes" user-emacs-directory))

(use-package doom-themes
  :defer t
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

(use-package kaolin-themes
  :defer t)

(use-package uwu-theme
  :defer t
  :custom
  (uwu-distinct-line-numbers nil))

(use-package base16-theme
  :defer t
  :custom
  (base16-distinct-fringe-background nil))

(use-package ef-themes
  :defer t)

(use-package apropospriate-theme
  :defer t)

(use-package kanagawa-themes
  :defer t)

(use-package nordic-night-theme
  :defer t)


;;; Theme helpers

(defvar my/saved-bg nil
  "Stores the original background of the `default' face.")

(defvar my/bg-is-black nil
  "Non-nil when the background is currently forced to pure black.")

(defun my/toggle-black-background ()
  "Toggle the default face background between pure black and the theme background."
  (interactive)
  (if my/bg-is-black
      (progn
        (set-face-attribute 'default nil :background my/saved-bg)
        (setq my/bg-is-black nil))
    (setq my/saved-bg (face-attribute 'default :background))
    (set-face-attribute 'default nil :background "#000000")
    (setq my/bg-is-black t)))

;; Reset the toggle whenever a new theme is loaded.
(add-hook
 'after-load-theme-hook
 (lambda ()
   (setq my/bg-is-black nil)))

(global-set-key
 (kbd "C-c b")
 #'my/toggle-black-background)

(use-package meow
  :config
  (defun my/meow-toggle-state ()
  (interactive)
  (if (meow-normal-mode-p)
      (meow-motion-mode)
    (meow-normal-mode)))
  
  ;; (setq meow-mode-state-list
  ;;     '((conf-mode . normal)
  ;;       (fundamental-mode . normal)
  ;;       (help-mode . normal)
  ;;       (prog-mode . normal)
  ;;       (text-mode . normal)
  ;; 		(special-mode . normal)))
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
	 '("h" . meow-left)
     '("j" . meow-next)
     '("k" . meow-prev)
	 '("l" . meow-right)
	 '("F" . flash-jump)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)

	 '("SPC" . execute-extended-command)
	 
	 '("v" . my-scrolling-zone)

	 '("f f" . find-file)
	 '("f s" . save-buffer)
	 '("f r" . rename-visited-file)

	 '("b"   . consult-buffer)
	 '("'"   . consult-bookmark)
	 '("k"   . kill-current-buffer) 

	 '("s"   . consult-line)

	 '("w"   . ace-window)
	 '("o"   . other-window)
	 '("0"   . delete-window)

	 ;; '("h v" . helpful-variable)
	 ;; '("h f" . helpful-function)
	 ;; '("h k" . helpful-key)
	 ;; '("h o" . helpful-symbol)
	 ;; '("h p" . helpful-at-point)
	 ;; '("h F" . describe-face)

	 '("p p" . project-switch-project)
	 '("p f" . project-find-file)
	 '("p b" . consult-project-buffer)
	 '("t b" . my/toggle-black-background)
	 '("t m" . my/meow-toggle-state))
	
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
	 
	 '("F" . flash-jump)))

  (keymap-set meow-insert-state-keymap
            "C-SPC"
            #'my/corfu-refresh)
  
  (meow-setup)
  (meow-global-mode 1))

(use-package project)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 10)
  (setq evil-complete-next-minibuffer-func 'vertico-next
        evil-complete-previous-minibuffer-func 'vertico-previous))

;; (use-package orderless
;;   :config
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion)))))

;;   (set-face-attribute 'orderless-match-face-0 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-1 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-2 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-3 nil :weight 'normal))


;; (use-package orderless
;;   :ensure t
;;   :custom
;;   ;; 🔴 IMPORTANT: basic first → fixes CAPF replacement issues
;;   (completion-styles '(basic orderless))

;;   ;; Disable default overrides
;;   (completion-category-defaults nil)

;;   ;; Per-category behavior
;;   (completion-category-overrides
;;    '((file (styles partial-completion))   ;; better file path completion
;;      (eglot (styles basic orderless))))  ;; 🔴 critical for LSP/CAPF

;;   :config
;;   ;; Optional: make matches less visually noisy
;;   (set-face-attribute 'orderless-match-face-0 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-1 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-2 nil :weight 'normal)
;;   (set-face-attribute 'orderless-match-face-3 nil :weight 'normal))

(use-package fzf-native)
(use-package fussy
  :config
  (fussy-setup)
  (fussy-setup-fzf)
  (fussy-eglot-setup)
  (fussy-corfu-setup)
  (defun fussy-try-completions (string table pred point)
	(let ((res (completion-flex-try-completion
				string table pred point)))
      (if (consp res)
          (cons string (length string))
		res))))

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
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file)


  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-recent-file
  ;;  consult--source-project-recent-file
  ;;  ;; :preview-key (kbd "M-.")
  ;;  :preview-key '(:debounce 0.4 any))

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
  (setq consult-project-function #'consult--default-project-function)
    ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package ace-window
  :commands ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-ignore-on nil)
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 1.0))

(use-package avy
  :commands
  (avy-goto-char
   avy-goto-char-2
   avy-goto-word-1
   avy-goto-line))

(use-package flash
  :commands (flash-jump flash-jump-continue
             flash-treesitter)
  :custom
  (flash-multi-window t))

(use-package magit
  :commands
  (magit
   magit-status
   magit-file-dispatch))

(use-package rg
  :commands
  (rg
   rg-project
   rg-menu))

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h c" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h o" . helpful-symbol)
   ("C-h k" . helpful-key))
  :commands
  (helpful-function
   helpful-callable
   helpful-variable
   helpful-symbol
   helpful-key))

(use-package which-key
  :defer t
  :config
  (which-key-mode))

(use-package rainbow-mode
  :commands rainbow-mode)

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

(provide 'core)
