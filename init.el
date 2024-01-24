;;; init.el --- Stuff all around -*- lexical-binding: t; -*-

;;; Commentary:
;; Modified version of
;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el

;;; Code:

(require 'use-package)

(use-package use-package
  :custom
  (use-package-hook-name-suffix nil)
  (use-package-compute-statistics t))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(use-package diminish :ensure t :after use-package) ;; in order to use :diminish
(use-package bind-key :ensure t :after use-package) ;; in order to use :bind

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


;;;========================================
;;; Useful defaults
;;;========================================

(use-package emacs
  :custom
  ;; (browse-url-browser-function 'browse-url-chrome)
  ;; (browse-url-new-window-flag  t)
  ;; (browse-url-chrome-new-window-is-tab t)
  (package-install-upgrade-built-in t)
  :init
  (set-face-attribute 'default nil :family "Intel One Mono" :height 130 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 130 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family "Avenir Next" :height 130 :weight 'regular)
  (set-fontset-font "fontset-default" 'han (font-spec :family "PingFang SC"))
  (set-fontset-font "fontset-default" 'cjk-misc (font-spec :family "PingFang SC"))
  (set-fontset-font "fontset-default" 'devanagari (font-spec :family "Noto Sans Devanagari"))
  (setq initial-major-mode 'fundamental-mode)   ; No need to have an Elisp buffer when starting up
  (setq-default cursor-type 'bar)               ; Line-style cursor similar to other text editors
  (setq initial-scratch-message
	"Welcome to Emacs!")	                ; Make *scratch* buffer have a welcome message
  (setq-default frame-title-format '("%b"))     ; Make window title the buffer name
  (setq-default fill-column 80)		        ; Set fill column to 80 rather than 70, in all cases.
  (setq inhibit-startup-screen t)               ; Disable startup screen
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq confirm-kill-processes nil)		; Stop confirming the killing of processes
  (setq use-short-answers t)                    ; y-or-n-p makes answering questions faster
  (show-paren-mode t)                           ; Visually indicates pair of matching parentheses
  (delete-selection-mode t)                     ; Start writing straight after deletion
  (put 'narrow-to-region 'disabled nil)	        ; Allows narrowing bound to C-x n n (region) and C-x n w (widen)
  (setq read-process-output-max (* 1024 1024))  ; Increase the amount of data which Emacs reads from the process
  (global-hl-line-mode 1)			; Highlight the current line to make it more visible
  (setq create-lockfiles nil)                   ; lock files kill `npm start'
  (pixel-scroll-precision-mode 1)	        ; Precision scrolling

  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups"))))
  (dolist (mapping '((python-mode . python-ts-mode)
		     (sh-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :hook (prog-mode-hook . display-fill-column-indicator-mode))


;; macOS system
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5)
				      ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (with-eval-after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others))


;; Adopt a sneaky garbage collection strategy of waiting until idle
;; time to collect; staving off the collector while the user is working.
(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gc-cons-percentage 0.8))

;; Automatically pair delimiters
(use-package elec-pair
  :ensure nil
  :defer t
  :config
  (defun nf-electric-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode."
    (add-function :before-until electric-pair-inhibit-predicate
		  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  :hook ((prog-mode-hook . electric-pair-local-mode)
	 (text-mode-hook . nf-electric-pair-local-text-mode)))

;; Set paths properly.
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :hook (after-init-hook . exec-path-from-shell-initialize))

;; Show function arglist or variable docstring in echo area.
(use-package eldoc
  :diminish eldoc-mode)

;; Automatically revert buffer when file changes in disk.
(use-package autorevert
  :defer 2
  :diminish auto-revert-mode)

;; Recent files.
(use-package recentf
  :defer 2)

;; Better shell

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-$" . vterm))

(use-package vertico
  :ensure t
  :pin elpa
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 20))))


;;;========================================
;;; Themes
;;;========================================

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-common-palette-overrides '((bg-mode-line-active bg-main)
						(bg-mode-line-inactive bg-dim)
						(border-mode-line-inactive bg-inactive)
						(fringe subtle)
						(bg-paren-match bg-yellow-intense)
						(custom-set-faces
						 '(mode-line ((t :family "Iosevka Etoile" :height 100 :weight 'regular))))))
  (setq modus-themes-headings
        (quote ((1 . (overline variable-pitch 1.4))
                (2 . (overline variable-pitch 1.25))
                (3 . (overline 1.1))
                (t . (monochrome)))))
  :config
  (load-theme 'modus-operandi :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Column and line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		pdf-view-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;;========================================
;;; Completion & Navigation
;;;========================================

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("C-c k"   . avy-goto-char-timer)))

;; ivy + counsel + swiper
(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

;; record history of command, showing the most common one in the beginninn
(use-package amx
  :ensure t
  :init (amx-mode))

;; enhance C-a C-e
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; Make help more helpful
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :pin melpa
  :ensure t
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :init
  (marginalia-mode))

(use-package which-key
  :ensure t
  :defer 4
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; Popup completion-at-point
(use-package corfu
  :pin elpa
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line))
  )

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 24)
  :bind ("C-c t" . treemacs))

(use-package deadgrep
  :ensure t
  :defer t
  :bind ("M-s o" . deadgrep))


;;;========================================
;;; Windows & movement
;;;========================================

(use-package windmove
  :ensure nil
  :defer t
  :config
  (setq windmove-create-window nil)     ; Emacs 27.1
  :bind (("C-c <up>" . windmove-up)
         ("C-c <right>" . windmove-right)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)))

(use-package transpose-frame
  :ensure t
  :defer t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (("C-c f" . flop-frame)
         ("C-c r" . rotate-frame-clockwise)))

;; Switch between multiple windows
(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window))


;;;========================================
;;; Spell checking
;;;========================================

;; Syntax checking for GNU Emacs
(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save)) ; Check on save instead of running constantly
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))

(use-package flymake
  :ensure t
  :defer t)


;;;========================================
;;; Org-mode
;;;========================================

(use-package org
  :ensure nil
  :diminish "Org"
  :custom
  (org-imenu-depth 7)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line nil)
  (org-fontify-whole-block-delimiter-line t)

  (org-confirm-babel-evaluate nil)         ; Don't prompt before running code in org
  (org-src-fontify-natively t)             ; Use syntax highlighting in source blocks while editing
  (org-src-tab-acts-natively t)            ; Tabs act as 4 spaces in source blocks
  (org-src-preserve-indentation t)         ; Preserving indentation in source blocks
  (org-highlight-latex-and-related '(latex))    ; Coloring latex code in mode
  (org-latex-prefer-user-labels t)         ; Prefer user names and labels for references
  (org-cite-csl-styles-dir "~/Zotero/styles") ; Use Zotero styles for CSL exports (bibliography management)
  (citar-citeproc-csl-styles-dir (expand-file-name "~/Zotero/styles"))
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  (org-latex-packages-alist
   (list (list ""
	       (concat (expand-file-name "~/Dropbox/assets/latex-sty/math"))
	       t)))
  (org-id-link-to-org-use-id 'create-if-interactive) ; Use id to insert link
  :config
  ;; Set :scale to 2 instead of 1 when org mode renders LaTeX
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))   ; Open PDF's with Emacs
  :hook (org-mode-hook . (lambda ()
			   (variable-pitch-mode t)
			   (setq-local fill-column 100))))

;; Use AUCTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-gs-command "/opt/homebrew/bin/gs")
  :hook
  (LaTeX-mode-hook . turn-on-auto-fill))

(use-package reftex
  :ensure t
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography (list (expand-file-name "~/Dropbox/refs.bib")))
  :hook (LaTeX-mode-hook . turn-on-reftex))

(require 'org-auctex)

;; Citations
(use-package org-ref
  :ensure t
  :defer t
  :config
  (setq org-export-before-parsing-functions '(org-ref-glossary-before-parsing
					      org-ref-acronyms-before-parsing)))

(use-package citeproc
  :ensure t
  :defer t
  :after org-ref)

(use-package citar
  :ensure t
  :defer t
  :custom
  (org-cite-global-bibliography (list (expand-file-name "~/Dropbox/refs.bib")))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

;; Ensure PDF view opens on the right.
(use-package shackle
  :ensure t
  :defer t
  :hook (org-mode-hook . shackle-mode)
  :config
  (setq shackle-rules
	'((pdf-view-mode :align right))))

;; Note taking
(defun open-svg-on-mac (path)
  "Use Safari to open SVG file that lies in PATH."
  (shell-command (concat "open -a Safari " path)))

(use-package org-roam
  :ensure t
  :defer t
  :diminish "-Ω-"
  :defines (org-roam-capture-templates org-roam-mode-map)
  :custom
  (org-roam-directory (file-truename (expand-file-name "~/Dropbox/org-roam")))
  (org-roam-db-location (expand-file-name "~/.org/org-roam.db"))
  (org-roam-graph-viewer (if (eq system-type 'darwin) 'open-svg-on-mac "/usr/bin/google-chrome-stable"))
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+created: %U \n#+last_modified: %U\n\n")
           :unnarrowed t)
	  ("c" "concept" plain "%?"
	   :if-new (file+head "concepts/${slug}.org"
	   "#+title: ${title}\n#+author: Ziqin Gong\n#+filetags:\n#+created: %U\n#+last_modified: %U\n\n")
	   :unnarrowed t)
	  ("l" "literature" plain "%?"
	   :if-new (file+head "literature/${slug}.org"
	   "#+title: ${title}\n#+author: Ziqin Gong\n#+filetags:\n#+created: %U\n#+last_modified: %U\n\n")
	   :unnarrowed t)))
  (org-roam-db-autosync-enable)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)))

;; Visualize nodes
(use-package org-roam-ui
  :ensure t
  :after org-roam
;;       normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;       a hookable mode anymore, you're advised to pick something yourself
;;       if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; Note searching
(use-package xeft
  :pin elpa
  :ensure t
  :defer t
  :custom
  (xeft-directory (expand-file-name "~/projects/notes"))
  (xeft-default-extension "org")
  (xeft-ignore-extension '("iimg" "md~" "tex" "tex~" "log" "gls" "glo" "glg" "org~"
			   "odt" "bbl" "ist" "qexams" "resums" "pdf" "class" "java"
			   "docx" "mw" "png" "jpg" "defs" "fls" "toc" "out" "fdb_latexmk"
			   "aux" "" "#" "pyg" "brf" "dvi" "html" "css" "js"))
  :commands xeft)

;; Show definitions
(use-package imenu-list
  :ensure t
  :defer t
  :bind ("C-'" . imenu-list-smart-toggle))

;; Modern org
(use-package org-modern
  :ensure t
  :defer t
  :pin melpa
  :custom
  (org-modern-table nil)
  :hook ((org-mode-hook . org-modern-mode)
	 (org-agenda-finalize-hook . org-modern-agenda)))


;;;========================================
;;; Focus
;;;========================================

(use-package olivetti
  :ensure t
  :defer t
  :diminish
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))


;;;========================================
;;; Reading
;;;========================================

(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :hook (TeX-after-compilation-finished-hook . TeX-revert-document-buffer)
  :defines pdf-annot-activate-created-annotations
  :custom
  (pdf-view-display-size 'fit-page)
  ;; more fine-grained zooming
  (pdf-view-resize-factor 1.05)
  ;; create annotation on highlight
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward)))

;; merriam-webster dictionary
(use-package mw-thesaurus
  :ensure t
  :defer t)


;;;========================================
;;; Version control
;;;========================================

;; Git integration for Emacs
;; Requires git

(use-package magit
  :ensure t
  :defer t
  :pin melpa
  :bind ("C-x g" . magit-status))


;;;========================================
;;; Editing
;;;========================================

;; Clean extra whitespace
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish
  :config (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)


;;;========================================
;;; Development
;;;========================================

(use-package eglot
  :ensure t
  :defer t
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown t)
  :hook (((python-base-mode-hook rust-mode) . eglot-ensure))
  :config
  ;; massive perf boost--don't log every event
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
	       `(rust-mode . ("rust-analyzer" :initializationOptions
			      ( :procMacro (:enbale t)
				:cargo ( :buildScripts (:enable t)
					 :features "all")))))
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))


(use-package wakatime-mode
  :ensure t
  :diminish
  :init (global-wakatime-mode)
  :config
  (setq wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli"))
  (setq wakatime-api-key "***REMOVED***"))


;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package elisp-mode
  :config
  :diminish "EL")

(use-package buttercup
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

;; Converts regex to `rx' syntax (very useful)
;; https://github.com/mattiase/xr
(use-package xr
  :ensure t
  :defer t)


;;;========================================
;;; OCaml development
;;;========================================

;; Major mode for OCaml programming
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune
  :ensure t)

;; Merlin provides advanced IDE features
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

;;;========================================
;;; Python development
;;;========================================

(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 2)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;; For virtual env management
(require 'pyrightconfig)

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniforge3"))
  (setq conda-env-home-directory (expand-file-name "~/miniforge3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; integration with org-mode
(use-package ob-python
  :defer t
  :commands (org-babel-execute:python))


;;;========================================
;;; Rust
;;;========================================

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  :ensure t
  :defer t)

(use-package cargo
  :ensure t
  :defer t
  :hook ((rust-ts-mode-hook rustic-mode-hook) . cargo-minor-mode))


;;;========================================
;;; YAML
;;;========================================

(use-package yaml-mode
  :ensure t
  :defer t
  :diminish
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


;;;========================================
;;; QoL
;;;========================================

;;; Self-defined utilities
(require 'utils)
(require 'commandline)

;;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (("C-<" . mc/mark-all-like-this-dwim)
   ("C->" . mc/mark-all-dwim)))

;;; Language parsing

(use-package tree-sitter
  :ensure t
  :defer t
  :diminish " tree")

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package cmake-ts-mode
  :ensure t
  :defer t)

(use-package combobulate
  :load-path "site-lisp/combobulate"
  :defer t
  :hook ((python-ts-mode-hook . combobulate-mode)
         (yaml-ts-mode-hook . combobulate-mode)))

;;; Colors
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;; CSV
(use-package csv-mode
  :ensure t
  :defer t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3e2039156049bd0661317137a3761d4c2ff43e8a2aa423f6db0c0e8df0197492" default))
 '(package-selected-packages
   '(amx auctex auto-package-update buttercup cape cargo citar conda corfu-terminal
	 counsel csv-mode deadgrep diminish dune elisp-lint embark-consult
	 exec-path-from-shell flycheck-ocaml gcmh helpful imenu-list jupyter
	 kind-icon magit marginalia merlin-eldoc modus-themes multiple-cursors
	 mw-thesaurus mwim ob-ipython olivetti orderless org-modern org-ref
	 org-roam-ui pdf-tools rainbow-delimiters rustic shackle transpose-frame
	 tree-sitter-langs treemacs tuareg vertico vterm wakatime-mode wgrep
	 which-key whitespace-cleanup-mode xeft xr yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
