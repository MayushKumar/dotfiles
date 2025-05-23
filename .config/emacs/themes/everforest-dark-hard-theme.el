;; -*- lexical-binding: t; -*-
(deftheme everforest-dark-hard "Everforest Dark Hard Theme.")

(let ((efdh-fg0          "#DDC7A1")
	  (efdh-fg           "#D3C6AA")
	  (efdh-fg1          "#C5B18D")
	  (efdh-red          "#E67E80")
	  (efdh-orange       "#E69875")
	  (efdh-yellow       "#DBBC7F")
	  (efdh-green        "#A7C080")
	  (efdh-aqua         "#83C092")
	  (efdh-blue         "#7FBBB3")
	  (efdh-purple       "#D699B6")
	  (efdh-dimRed       "#DA6362")
	  (efdh-dimOrange    "#D77F48")
	  (efdh-dimYellow    "#BF983D")
	  (efdh-dimGreen     "#899C40")
	  (efdh-dimAqua      "#569D79")
	  (efdh-dimBlue      "#5A93A2")
	  (efdh-dimPurple    "#B87B9D")

	  (efdh-bg0          "#1E2327")
	  (efdh-bg1          "#232A2F")
	  (efdh-bg           "#2B3339")
	  (efdh-bg2          "#323C41")
	  (efdh-bg3          "#3A454A")
	  (efdh-bg4          "#445055")
	  (efdh-bg5          "#4C555B")
	  (efdh-grey0        "#7F897D")
	  (efdh-grey1        "#859289")
	  (efdh-grey2        "#9AA79D"))

  (custom-theme-set-faces 'everforest-dark-hard
                          '(button ((t (:box (:line-width 1 :color ,efdh-yellow) ))))
                          `(default ((t (:background ,efdh-bg :foreground ,efdh-fg))))
                          `(cursor ((t (:background ,efdh-fg :foreground ,efdh-bg))))
                          `(link ((t (:underline t :foreground ,efdh-blue))))
                          `(link-visited ((t (:underline t :foreground ,efdh-blue))))
                          `(underline ((t (:underline t :foreground ,efdh-yellow))))
                          `(font-lock-keyword-face ((t (:foreground ,efdh-red))))
                          `(font-lock-function-name-face ((t (:foreground ,efdh-green))))
                          `(font-lock-string-face ((t (:foreground ,efdh-green))))
                          `(font-lock-warning-face ((t (:inverse-video t :background ,efdh-bg :foreground ,efdh-red))))
                          `(font-lock-type-face ((t (:foreground ,efdh-yellow))))
                          `(font-lock-preprocessor-face ((t (:foreground ,efdh-red))))
                          `(font-lock-builtin-face ((t (:foreground ,efdh-yellow))))
                          `(font-lock-variable-name-face ((t (:foreground ,efdh-yellow))))
                          `(font-lock-constant-face ((t (:foreground ,efdh-purple))))
                          `(font-lock-doc-face ((t (:slant italic :foreground ,efdh-fg0))))
                          `(font-lock-comment-face ((t (:foreground ,efdh-grey1))))
                          `(shadow ((t (:foreground ,efdh-fg0))))
                          `(Info-quoted ((t (:inherit font-lock-constant-face))))
                          `(show-paren-match ((t (:background ,efdh-grey0))))
                          `(highline-face ((t (:background ,efdh-bg))))
                          `(ac-selection-face ((t (:background ,efdh-purple :foreground ,efdh-bg4))))
                          `(ac-candidate-face ((t (:background ,efdh-bg :foreground ,efdh-fg))))
                          `(flymake-errline
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-red)
                                          :inherit unspecified :foreground unspecified :background unspecified))
                             (t (:foreground ,efdh-red :weight bold :underline t))))
                          `(flymake-warnline
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-yellow)
                                          :inherit unspecified :foreground unspecified :background unspecified))
                             (t (:foreground ,efdh-yellow :weight bold :underline t))))
                          `(flymake-infoline
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-green)
                                          :inherit unspecified :foreground unspecified :background unspecified))
                             (t (:foreground ,efdh-green :weight bold :underline t))))
                          `(flyspell-duplicate
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-yellow) :inherit unspecified))
                             (t (:foreground ,efdh-yellow :weight bold :underline t))))
                          `(flyspell-incorrect
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-red) :inherit unspecified))
                             (t (:foreground ,efdh-red :weight bold :underline t))))
                          `(minibuffer-prompt ((t (:foreground ,efdh-yellow))))
                          `(menu ((t (:foreground ,efdh-fg :background ,efdh-bg))))
                          `(highlight ((t (:background ,efdh-bg4))))
                          ;; `(hl-line-face ((t (:background ,efdh-bg2))
                          ;;                 (t :weight bold)))
                          `(hl-line ((t (:background ,efdh-bg2 :extend t))))
                          `(success ((t (:foreground ,efdh-green :weight bold))))
                          `(warning ((t (:foreground ,efdh-yellow :weight bold))))
                          `(error ((t  (:foreground ,efdh-red))))
                          `(tooltip ((t (:foreground ,efdh-fg :background ,efdh-bg))))
                          `(region ((t (:background ,efdh-bg4))))
                          `(secondary-selection ((t (:background ,efdh-bg))))
                          `(trailing-fgspace ((t (:background ,efdh-red))))
                          `(border ((t (:background ,efdh-bg :foreground ,efdh-fg))))
                          `(vertical-border ((t (:foreground ,efdh-grey0))))
                          `(mode-line ((t (:foreground ,efdh-fg :background ,efdh-bg1))))
                          `(mode-line-inactive ((t (:foreground ,efdh-fg0 :background ,efdh-bg2))))
                          `(mode-line-buffer-id ((t (:background ,efdh-bg :foreground ,efdh-fg))))
                          `(mode-line-emphasis ((t (:foreground ,efdh-fg0))))
                          `(mode-line-highlight ((t (:foreground ,efdh-purple :box nil :weight bold))))
                          `(fringe ((t (:underline t :background ,efdh-bg :foreground ,efdh-bg4))))
                          ;; `(fill-column-indicator ((,class :foreground ,efdh-bg4 :weight semilight)))
                          `(linum ((t (:background ,efdh-bg :foreground ,efdh-fg))))
                          `(line-number ((t (:foreground ,efdh-bg5
                                                         (list :background ,efdh-bg)))))
                          `(line-number-current-line ((t (:inherit line-number :foreground ,efdh-fg
                                                                   (list :background ,efdh-bg4)))))
                          `(header-line ((t (:foreground ,efdh-yellow
                                                         :background ,efdh-bg
                                                         :box (:line-width -1 :style released-button)
                                                         :extend t))))
                          `(widget-field ((t (:foreground ,efdh-fg :background ,efdh-bg3
														  :box (:line-width -1 :color ,efdh-grey0)))))
                          ;; `(widget-button ((t (:underline t))))
                          `(escape-glyph ((t (:foreground ,efdh-yellow :weight bold))))
						  `(help-key-binding ((t (:foreground ,efdh-blue :background ,efdh-bg0))))
                          `(dired-directory ((t (:weight bold :foreground ,efdh-blue))))
                          `(lazy-highlight ((t (:foreground ,efdh-blue :background ,efdh-bg :inverse-video t))))
                          `(isearch ((t (:inverse-video t :background ,efdh-bg4 :foreground ,efdh-blue))))
                          `(isearch-fail ((t (:background ,efdh-bg :inherit font-lock-warning-face :inverse-video t))))
                          `(isearch-lazy-highlight-face ((t (:inverse-video t :foreground ,efdh-yellow))))
                          `(grep-context-face ((t (:foreground ,efdh-fg))))
                          `(grep-error-face ((t (:foreground ,efdh-red :weight bold :underline t))))
                          `(grep-hit-face ((t (:foreground ,efdh-blue))))
                          `(grep-match-face ((t (:foreground ,efdh-blue :weight bold))))
                          `(match ((t (:background ,efdh-bg :foreground ,efdh-blue :weight bold))))
                          `(completions-annotations ((t (:foreground ,efdh-fg))))
                          `(completions-common-part ((t (:foreground ,efdh-blue))))
                          `(completions-first-difference ((t (:foreground ,efdh-fg))))
                          `(ido-first-match ((t (:foreground ,efdh-blue :weight bold))))
                          `(ido-only-match ((t (:foreground ,efdh-blue :weight bold))))
                          `(ido-subdir ((t (:foreground ,efdh-yellow))))
                          `(ido-indicator ((t (:foreground ,efdh-yellow :background ,efdh-red))))
                           ;;;;; org-mode
                          `(org-agenda-date-today
                            ((t (:foreground ,efdh-fg :slant italic :weight bold))) t)
                          `(org-agenda-structure
                            ((t (:inherit font-lock-fg0-face))))
                          `(org-archived ((t (:foreground ,efdh-fg :weight bold))))
                          `(org-block ((t (:background ,efdh-bg2 :foreground ,efdh-fg :extend t))))
                          `(org-block-begin-line ((t (:foreground ,efdh-fg0 :background ,efdh-bg :extend t))))
                          `(org-code ((t (:foreground ,efdh-yellow ))))
                          `(org-checkbox ((t (:background ,efdh-bg :foreground ,efdh-fg
                                                          :box (:line-width 1 :style released-button)))))
                          `(org-date ((t (:foreground ,efdh-blue :underline t))))
                          `(org-deadline-announce ((t (:foreground ,efdh-red))))
                          `(org-done ((t (:weight bold :weight bold :foreground ,efdh-green))))
                          `(org-formula ((t (:foreground ,efdh-yellow))))
                          `(org-headline-done ((t (:foreground ,efdh-green))))
                          `(org-hide ((t (:background ,efdh-bg :foreground ,efdh-bg))))
                          `(org-verbatim ((t (:foreground ,efdh-yellow))))
                          `(org-meta-line ((t (:foreground ,efdh-fg0))))
                          `(org-indent ((t (:background ,efdh-bg :foreground ,efdh-bg))))
                          ;; `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,efdh-blue))))
                          ;; `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,efdh-green))))
                          ;; `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,efdh-purple))))
                          ;; `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,efdh-red))))
                          ;; `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,efdh-blue))))
                          ;; `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,efdh-green))))
                          ;; `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,efdh-purple))))
                          ;; `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,efdh-red))))
                          `(org-link ((t (:foreground ,efdh-blue :underline t))))
                          `(org-scheduled ((t (:foreground ,efdh-green))))
                          `(org-scheduled-previously ((t (:foreground ,efdh-red))))
                          `(org-scheduled-today ((t (:foreground ,efdh-blue))))
                          `(org-sexp-date ((t (:foreground ,efdh-blue :underline t))))
                          `(org-special-keyword ((t (:inherit font-lock-fg0-face))))
                          `(org-table ((t (:foreground ,efdh-blue))))
                          `(org-tag ((t (:weight bold :weight bold))))
                          `(org-time-grid ((t (:foreground ,efdh-yellow))))
                          `(org-todo ((t (:weight bold :foreground ,efdh-red :weight bold))))
                          `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
                          `(org-warning ((t (:weight bold :foreground ,efdh-red :weight bold :underline nil))))
                          `(org-column ((t (:background ,efdh-bg))))
                          `(org-column-title ((t (:background ,efdh-bg :underline t :weight bold))))
                          `(org-mode-line-clock ((t (:foreground ,efdh-fg :background ,efdh-bg))))
                          `(org-mode-line-clock-overrun ((t (:foreground ,efdh-bg :background ,efdh-red))))
                          `(org-ellipsis ((t (:foreground ,efdh-yellow :underline t))))
                          `(org-footnote ((t (:foreground ,efdh-aqua :underline t))))
                          ;; `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,efdh-blue
                          ;;                                    :weight bold
                          ;;                                    ,@(when efdh-scale-org-headlines
                          ;;                                        (list :height efdh-height-plus-4))))))
                          `(org-document-info ((t (:foreground ,efdh-purple))))
                          `(org-document-info-keyword ((t (:foreground ,efdh-fg0))))
                          `(org-habit-ready-face ((t :background ,efdh-green)))
                          `(org-habit-alert-face ((t :background ,efdh-yellow :foreground ,efdh-bg)))
                          `(org-habit-clear-face ((t :background ,efdh-blue)))
                          `(org-habit-overdue-face ((t :background ,efdh-red)))
                          `(org-habit-clear-future-face ((t :background ,efdh-blue)))
                          `(org-habit-ready-future-face ((t :background ,efdh-green)))
                          `(org-habit-alert-future-face ((t :background ,efdh-yellow :foreground ,efdh-bg)))
                          `(org-habit-overdue-future-face ((t :background ,efdh-red)))
                          ;;;;; org-ref
                          `(org-ref-ref-face ((t :underline t)))
                          `(org-ref-label-face ((t :underline t)))
                          `(org-ref-cite-face ((t :underline t)))
                          `(org-ref-glossary-face ((t :underline t)))
                          `(org-ref-acronym-face ((t :underline t)))
                          ;;;;; flycheck
                          `(flycheck-error
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-red) :inherit unspecified))
                             (t (:foreground ,efdh-red :weight bold :underline t))))
                          `(flycheck-warning
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-yellow) :inherit unspecified))
                             (t (:foreground ,efdh-yellow :weight bold :underline t))))
                          `(flycheck-info
                            ((((supports :underline (:style wave)))
                              (:underline (:style line :color ,efdh-aqua) :inherit unspecified))
                             (t (:foreground ,efdh-aqua :weight bold :underline t))))
                          `(flycheck-fringe-error ((t (:foreground ,efdh-red :weight bold))))
                          `(flycheck-fringe-warning ((t (:foreground ,efdh-yellow :weight bold))))
                          `(flycheck-fringe-info ((t (:foreground ,efdh-aqua :weight bold))))
                          ;;;;; company-mode
                          `(company-tooltip ((t (:foreground ,efdh-fg :background ,efdh-bg1))))
                          `(company-tooltip-annotation ((t (:foreground ,efdh-blue))))
                          `(company-tooltip-annotation-selection ((t (:foreground ,efdh-blue))))
                          `(company-tooltip-selection ((t (:foreground ,efdh-fg :background ,efdh-bg3))))
                          `(company-tooltip-mouse ((t (:background ,efdh-bg))))
                          `(company-tooltip-common ((t (:foreground ,efdh-green))))
                          `(company-tooltip-common-selection ((t (:foreground ,efdh-green))))
                          `(company-scrollbar-fg ((t (:background ,efdh-bg5))))
                          `(company-scrollbar-bg ((t (:background ,efdh-bg1))))
                          `(company-preview ((t (:background ,efdh-green))))
                          `(company-preview-common ((t (:foreground ,efdh-green :background ,efdh-bg))))
                           ;;;;; term, ansi-term, vterm
                          `(term-color-bg ((t (:foreground ,efdh-bg
                                                           :background , efdh-bg))))
                          `(term-color-red ((t (:foreground ,efdh-red
                                                            :background ,efdh-red))))
                          `(term-color-green ((t (:foreground ,efdh-green
                                                              :background ,efdh-green))))
                          `(term-color-yellow ((t (:foreground ,efdh-yellow
                                                               :background ,efdh-yellow))))
                          `(term-color-blue ((t (:foreground ,efdh-blue
                                                             :background ,efdh-blue))))
                          `(term-color-purple ((t (:foreground ,efdh-purple
                                                               :background ,efdh-purple))))
                          `(term-color-aqua ((t (:foreground ,efdh-aqua
                                                             :background ,efdh-aqua))))
                          `(term-color-fg ((t (:foreground ,efdh-fg
                                                           :background ,efdh-fg))))
                          '(term-default-fg-color ((t (:inherit efdh-fg))))
                          '(term-default-bg-color ((t (:inherit efdh-bg))))
                          ;; diff-mode
                          `(diff-added ((t (:foreground ,efdh-green :background: ,efdh-bg :extend t))))
                          `(diff-changed ((t  (:foreground ,efdh-yellow :background: ,efdh-bg :extend t))))
                          `(diff-removed ((t (:foreground ,efdh-red :background: ,efdh-bg :extend t))))
                          `(diff-indicator-added ((t (:inherit diff-added))))
                          `(diff-indicator-changed ((t (:inherit diff-changed))))
                          `(diff-indicator-removed ((t (:inherit diff-removed))))
                          `(diff-refine-added   ((t (:background ,efdh-green :foreground ,efdh-bg))))
                          `(diff-refine-changed ((t (:background ,efdh-yellow :foreground ,efdh-bg))))
                          `(diff-refine-removed ((t (:background ,efdh-red :foreground ,efdh-bg))))
                          ;; `(diff-header ((,class (:background ,efdh-bg))
                          ;; (t (:background ,efdh-fg :foreground ,efdh-bg))))
                          ;; `(diff-file-header
                          ;;   ((,class (:background ,efdh-bg :foreground ,efdh-fg :weight bold))
                          ;;    (t (:background ,efdh-fg :foreground ,efdh-bg :weight bold))))
                          ;;;;; diff-hl
                          ;; `(diff-hl-change ((,class (:inverse-video t :foreground ,efdh-yellow :background ,efdh-bg))))
                          ;; `(diff-hl-delete ((,class (:inverse-video t :foreground ,efdh-red :background ,efdh-bg))))
                          ;; `(diff-hl-insert ((,class (:inverse-video t :foreground ,efdh-green :background ,efdh-bg))))
                          ;; tab-bar
                          `(tab-bar ((t (:height 1.1 :foreground ,efdh-fg :background ,efdh-bg))))
                          `(tab-bar-tab ((t (:background ,efdh-bg
                                                         :foreground ,efdh-purple
                                                         :box (:line-width 1 :style released-button)))))
                          `(tab-bar-tab-inactive ((t (:inherit tab-bar-tab
                                                               :background ,efdh-bg
                                                               :foreground ,efdh-fg0))))

                          ;; tab-line
                          `(tab-line ((t (:foreground ,efdh-fg :background ,efdh-bg))))
                          `(tab-line-close-highlight ((t (:foreground ,efdh-red))))
                          `(tab-line-tab ((t (:background ,efdh-bg
                                                          :foreground ,efdh-purple
                                                          :box (:line-width 1 :style released-button)))))
                          `(tab-line-tab-inactive ((t (:inherit tab-line-tab
                                                                :background ,efdh-bg
                                                                :foreground ,efdh-fg0))))
                          ;;;;; vertico
                          `(vertico-current ((t (:background ,efdh-bg3 :foreground ,efdh-yellow))))
                          `(vertico-multiline ((t (:foreground ,efdh-green))))
                          `(vertico-group-title ((t (:foreground ,efdh-green :weight bold))))
                          `(vertico-group-separator ((t (:foreground ,efdh-green :weight bold))))
                          ;;;;; selectrum
                          `(selectrum-current-candidate ((t (:background ,efdh-bg :foreground ,efdh-yellow))))
                          `(selectrum-primary-highlight ((t (:background ,efdh-green))))
                          `(selectrum-secondary-highlight ((t (:background ,efdh-green))))
                          ;;;;; orderless
                          `(orderless-match-face-0 ((t (:foreground ,efdh-green))))
                          `(orderless-match-face-1 ((t (:foreground ,efdh-purple))))
                          `(orderless-match-face-2 ((t (:foreground ,efdh-blue))))
                          `(orderless-match-face-3 ((t (:foreground ,efdh-yellow))))
                          ;;;;; helpful
                          `(helpful-heading ((t (:foreground ,efdh-green :height 1.2))))
                          ;;;;; rainbow-delimiters
                          `(rainbow-delimiters-depth-1-face ((t (:foreground ,efdh-blue))))
                          `(rainbow-delimiters-depth-2-face ((t (:foreground ,efdh-green))))
                          `(rainbow-delimiters-depth-3-face ((t (:foreground ,efdh-purple))))
                          `(rainbow-delimiters-depth-4-face ((t (:foreground ,efdh-yellow))))
                          `(rainbow-delimiters-depth-5-face ((t (:foreground ,efdh-red))))
                          `(rainbow-delimiters-depth-6-face ((t (:foreground ,efdh-aqua))))
                          `(rainbow-delimiters-depth-7-face ((t (:foreground ,efdh-blue))))
                          `(rainbow-delimiters-depth-8-face ((t (:foreground ,efdh-green))))
                          `(rainbow-delimiters-depth-9-face ((t (:foreground ,efdh-purple))))
                          `(rainbow-delimiters-depth-10-face ((t (:foreground ,efdh-yellow))))
                          `(rainbow-delimiters-depth-11-face ((t (:foreground ,efdh-red))))
                          `(rainbow-delimiters-depth-12-face ((t (:foreground ,efdh-aqua))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'everforest-dark-hard)

(provide 'everforest-dark-hard-theme)
