;;; init.el --- Stuff all around -*- lexical-binding: t; -*-

;;; Commentary:
;; Modified version of
;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el

;;; Code:

;; Elpaca installer
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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

;; install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; block until current queue processed.
(elpaca-wait)

;; use `:delight'
(use-package delight
  :ensure t
  :after use-package)

;; use `:bind'
;; (use-package bind-key
;;   :ensure t
;;   :after use-package)

;; set paths correctly
;; unnecessary since Emacs+ inject `PATH' by default
(use-package exec-path-from-shell
  :ensure t
  :demand t ; help emacs find libgccjit.so installed by homebrew
  :config
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;; block until current queue processed.
;; necessary to use these keywords at the top-level.
(elpaca-wait)



;;;========================
;;; basic features
;;;========================

(use-package emacs
  :ensure nil
  :init
  ;; utf-8 encoding
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  ;; precision scrolling
  (pixel-scroll-precision-mode 1)
  ;; no need to have an elisp buffer when starting up
  (setq initial-major-mode 'fundamental-mode)
  ;; line-stype cursor
  (setq-default cursor-type 'bar)
  ;; welcome message in *scratch*
  (setq initial-scratch-message nil)
  ;; make the window title the buffer name
  (setq-default frame-title-format '("%b"))
  ;; set fill column to 100 rather than 70 in all cases
  (setq-default fill-column 100)
  ;; disable startup screen
  (setq inhibit-startup-screen t)
  ;; stop confirming the killing of processes
  (setq confirm-kill-processes nil)
  ;; short answers (y/n/p)
  (setq use-short-answers t)
  ;; visually indicates pair of matching parentheses
  (show-paren-mode t)
  ;; delete selection when start typing
  (delete-selection-mode t)
  ;; increase the amount of data which emacs read from processes
  (setq read-process-output-max (* 1024 1024))
  ;; highlight the current line
  ;; (global-hl-line-mode 1)  ; use beacon-mode
  :config
  ;; fonts
  (set-face-attribute 'default nil :family "SF Mono" :height 120 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 120 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 120 :weight 'regular)
  (set-fontset-font "fontset-default" 'han "PingFang SC")
  (set-fontset-font "fontset-default" 'cjk-misc "PingFang SC")
  (set-fontset-font "fontset-default" 'devanagari "Lava Devanagari")
  ;; no ringing
  (setq ring-bell-function #'ignore)
  ;; use xwidget-webkit to open link
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  :custom
  (line-spacing 0.2)
  :hook
  ;; show the fill column when programming
  (prog-mode . display-fill-column-indicator-mode))

;; macos
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

;; automatically pair delimiters
(use-package elec-pair
  :ensure nil
  :defer t
  :config
  (defun elec-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode."
    (add-function :before-until electric-pair-inhibit-predicate
                  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  :hook
  ((prog-mode . electric-pair-local-mode)
   (text-mode . elec-pair-local-text-mode)))


;; backup in the same location
(setq backup-directory-alist '(("." . "/private/tmp/emacs-backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 7
      kept-old-versions 3)



;;;========================
;;; appearance
;;;========================

;; column and line number
(column-number-mode)
(global-display-line-numbers-mode t)

;; never use tabs for indentation
(setq-default indent-tabs-mode nil)

;; highlights the cursor when it takes a large leap
(use-package beacon
  :ensure t
  :config
  (beacon-mode))

;; colorful delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; disable line numbers for some modes
(defun disable-line-numbers ()
  "Disable line numbers.  To be added in hooks."
  (display-line-numbers-mode 0))
(dolist (mode '(org-mode-hook
                xwidget-webkit-mode-hook
                pdf-view-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode #'disable-line-numbers))

;; show function arglist or variable docstring in echo area
(use-package eldoc
  :ensure nil
  :delight)

;; displays key bindings following currently entered incomplete command
;; (a prefix) in a popup
(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode))


;; modus-theme

;; use emacs+'s `ns-system-appearance-change-functions' to toggle light/dark
(defun zq/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi :no-confirm))
    ('dark (load-theme 'modus-vivendi :no-confirm)))
  (message "Theme toggled!"))

(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-org-blocks 'tinted-background
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-common-palette-overrides
        '((bg-mode-line-active bg-main)
          (bg-mode-line-inactive bg-dim)
          (border-mode-line-inactive bg-inactive)
          (fringe subtle)
          (bg-paren-match bg-yellow-intense)
          (custom-set-faces
           '(mode-line ((t :family "SF Mono" :height 100 :weight 'regular))))))
  (setq modus-themes-headings
        (quote ((1 . (overline variable-pitch 1.4))
                (2 . (overline variable-pitch 1.25))
                (3 . (overline 1.1))
                (t . (monochrome)))))
  :config
  (add-hook 'ns-system-appearance-change-functions #'zq/apply-theme)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))



;;;========================
;;; completion at point
;;;========================

(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; complete words from current buffer
  (add-to-list 'completion-at-point-functions #'cape-file) ; complete file names
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

;; popup completion-at-point
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)         ;; Delay for auto completion. Default: 0.2
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)     ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer." ; for `eval-expression' and `shell-command'
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))



;;;========================
;;; minibuffer completion
;;;========================

;; vertical completion UI based on the default completion system
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; adds marginalia to the minibuffer completions
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind
  ;; bind `marginalia-cycle' locally in the minibuffer
  ;; in *Completion* buffer: add to `completion-list-mode-map'
  (:map minibuffer-local-map ("M-A" . marginalia-cycle)))

;; completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))



;;;========================
;;; enhancements
;;;========================

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)                    ;; combined with orderless to replace isearch
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
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
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; quick select and copy
(use-package easy-kill
  :ensure t
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp] . easy-mark)))

;; quick jump and act
(defun avy-action-helpful (pt)
  "Look up a symbol at PT via avy."
  (save-excursion (goto-char pt)
                  (helpful-at-point))
  (select-window (cdr (ring-ref avy-ring 0)))
  t)

(use-package avy
  :ensure t
  :demand t
  :bind
  (("C-c j" . avy-goto-line)
   ("C-c k" . avy-goto-char-timer))
  :config
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
  :custom
  (avy-timeout-seconds 0.3))

;; save command history
;; show the most common one first
(use-package amx
  :ensure t
  :init
  (amx-mode))

;; smarter C-a C-e
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; Make help more helpful
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; switch between multiple windows
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))

;; remove extra whitespace on save
(use-package whitespace-cleanup-mode
  :ensure t
  :demand t
  :delight
  :config
  (add-hook 'elpaca-after-init-hook
            (lambda () (global-whitespace-cleanup-mode t))))



;;;========================
;;; development
;;;========================

;; remote
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)
  :custom
  (tramp-own-remote-path '("~/.local/bin" "~/.cargo/bin")))

;; docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; eshell
(defun fancy-shell ()
  "A pretty eshell with git status.  Reference:
https://lambdaland.org/posts/2024-08-19_fancy_eshell_prompt/#eshell-prompt."
  (let* ((cwd (abbreviate-file-name (eshell/pwd)))
         (ref (if (boundp 'magit-get-shortname)
                  (magit-get-shortname "HEAD") nil))
         (stat (if (boundp 'magit-file-status)
                   (magit-file-status) nil))
         (x-stat eshell-last-command-status)
         (git-chunk
          (if ref
              (format "%s%s%s "
                      (propertize (if stat "[" "(")
                                  'font-lock-face (if stat 'error 'success))
                      (propertize ref 'font-lock-face 'warning)
                      (propertize (if stat "]" ")")
                                  'font-lock-face (if stat 'error 'success)))
            "")))
    (propertize
     (format "%s %s %s$ "
             (if (< 0 x-stat)
                 (format (propertize "!%s" 'font-lock-face 'error) x-stat)
               (propertize "➤"
                           'font-lock-face (if (< 0 x-stat) 'error 'success)))
             (propertize cwd 'font-lock-face 'term-bold)
             git-chunk)
     'read-only nil ;; unable to insert any character if true
     'front-sticky   '(font-lock-face read-only)
     'rear-nonsticky '(font-lock-face read-only))))

(use-package eshell
  :ensure nil
  :custom
  (eshell-prompt-function 'fancy-shell)
  (eshell-prompt-regexp "^[^#$\n]* [$#] ")
  (eshell-highlight-prompt nil)
  :bind
  ("C-c e" . eshell))

;; magit
(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-x g" . magit-status))

(use-package transient
  :ensure t
  :defer t)

;; projectile for projects
(use-package projectile
  :ensure t
  :delight
  :init
  (projectile-mode +1)
  :custom
  (projectile-cache-file (expand-file-name "~/.cache/emacs/projectile.cache"))
  (projectile-known-projects-file
   (expand-file-name "~/.cache/emacs/projectile-bookmarks.eld"))
  (projectile-project-search-path '(("~/Developer/" . 2)
                                    ("~/Research/" . 2)))
  (projectile-jj-command "jj file list --no-pager . | tr '\\n' '\\0'")
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

;; wakatime
(use-package wakatime-mode
  :ensure t
  :delight
  :init (global-wakatime-mode)
  :config
  (setq wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli")))

;; syntax check
(defun check-with-c++17-std ()
  "Make flycheck use c++17 std."
  (setq-local flycheck-clang-language-standard "c++17"))

(use-package flycheck
  :ensure t
  :defer t
  :delight
  :custom
  ;; check on save instead of running constantly
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-disabled-checkers '(python-mypy))  ;; pylsp already has mypy plugin
  :hook
  ((prog-mode text-mode) . flycheck-mode)
  (c++-mode . check-with-c++17-std))

;; parse
(use-package tree-sitter
  :ensure t
  :defer t
  :delight " tree")

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package treesit
  :ensure nil
  :commands
  (treesit-install-language-grammar nf/treesit-install-all-languages)
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
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (typst . ("https://github.com/uben0/tree-sitter-typst"))))
  (dolist (mapping '((python-mode . python-ts-mode)
                     (sh-mode . bash-ts-mode)
                     (rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75)))))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate"
                 :depth 1 :main "combobulate.el")
  :after treesit
  :defer t
  :init
  (setq combobulate-key-prefix "C-c o")
  :hook
  (python-ts-mode . combobulate-mode))

;; eglot
(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eglot-autoshutdown t)
  :hook
  ((rust-mode c-mode c++-mode) . eglot-ensure)
  :config
  ;; don't log every event--boost perf
  (fset #'jsonrpc--log-event #'ignore)
  ;; pylsp config
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins ( :black (:enabled :json-false)
                                     :autopep8 (:enabled :json-false)
                                     :yapf ( :enabled :json-false)
                                     :ruff ( :enabled t
                                             :formatEnabled t
                                             :lineLength 88)
                                     :jedi_completion
                                     ( :enabled t
                                       :include_params t
                                       :include_class_objects t
                                       :include_function_objects t
                                       :fuzzy t
                                       :eager t
                                       :cache_for ["matplotlib" "numpy" "torch"])
                                     :flake8 (:enabled :json-false)
                                     :pycodestyle (:enabled :json-false)))))
  ;; rust
  (add-to-list 'eglot-server-programs
               `(rust-mode . ("rust-analyzer" :initializationOptions
                              ( :procMacro (:enable t)
                                :cargo ( :buildScripts (:enable t)
                                         :features "all")))))
  :bind
  (("C-c l c" . eglot-reconnect)
   ("C-c l d" . flymake-show-buffer-diagnostics)
   ("C-c l f f" . eglot-format)
   ("C-c l f b" . eglot-format-buffer)
   ("C-c l l" . eglot)
   ("C-c l r n" . eglot-rename)
   ("C-c l s" . eglot-shutdown)))



;; C/C++
(use-package clang-format
  :ensure t
  :custom
  (clang-format-style "Google"))



;; python
(use-package python
  :ensure nil
  :defer t
  :config
  ;; remove guess indent message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4))


;; use pet to correctly set up venv
(defun setup-python-venv ()
  "Setup correct python executable and env."
  (setq-local python-shell-interpreter (pet-executable-find "ipython")
              python-shell-interpreter-args "-i --simple-prompt"
              python-shell-virtualenv-root (pet-virtualenv-root)
              python-shell-extra-pythonpaths (list (pet-project-root)))
  (pet-eglot-setup)
  (eglot-ensure))

(use-package pet
  :ensure t
  :hook
  (python-base-mode . setup-python-venv))

;; python in org
(use-package ob-python
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:python))



;; rust
(use-package rustic
  :ensure t
  :defer t
  :custom
  (rustic-lsp-client 'eglot))

(use-package cargo
  :ensure t
  :defer t
  :hook
  ((rust-ts-mode rustic-mode) . cargo-minor-mode))



;; yaml
(defun zq/yaml-map ()
  "Define map for yaml-mode."
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

(use-package yaml-mode
  :ensure t
  :delight
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :hook
  (yaml-mode . zq/yaml-map))



;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . zq/prettify-org))



;; latex
;; use AUCTeX
(defun zq/org-latex-preview-scale-fn ()
  "Org LaTeX preview scale function."
  (* (/ 10.0
        (preview-document-pt))
     preview-scale))

(use-package auctex
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git"
                  :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'tex-site) AUCTeX-version)))

(use-package latex
  :ensure nil
  :init
  (setq-default preview-scale 1.2
                preview-scale-function #'zq/org-latex-preview-scale-fn)
  ;; :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-auto-cache-preamble nil)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :bind
  (:map LaTeX-mode-map
        ("<M-return>" . LaTeX-insert-item)
        ("C-c t 0" . insert-left-right-parentheses)
        ("C-c t [" . insert-left-right-brackets)
        ("C-c t ]" . insert-left-right-braces))
  :custom
  (TeX-auto-local ".tex_auto")
  :hook
  (LaTeX-mode . turn-on-auto-fill)
  (LaTeX-mode . prettify-symbols-mode))

(use-package reftex
  :ensure nil
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-default-bibliography
   (list (expand-file-name "~/OneDrive/zot-references.bib")))
  :hook
  (LaTeX-mode . turn-on-reftex))

;; better pdf reader
(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)  ;; build necessary tool if missing
  :custom
  (pdf-view-display-size 'fit-page))



;; SICP
(use-package sicp
  :ensure t)

;; Racket
(use-package racket-mode
  :ensure t)

;; racket in org
(use-package ob-racket
  :ensure (:type git :host github :repo "hasu/emacs-ob-racket"))



;;;========================
;;; org-mode
;;;========================

(defun zq/prettify-org ()
  "Use `variable-pitch-mode'."
  (variable-pitch-mode t)
  (setq-local line-spacing 0.3))

(defun insert-delimiter (type)
  "Insert delimiters based on TYPE with \\left \\right for LaTeX equations.
\() -- 0, [] -- 1, {} -- 2."
  (defun insert-delimiter-and-stay-in-between (delimiters)
    (insert (car delimiters))
    (save-excursion
      (insert (car (cdr delimiters)))))
  (insert-delimiter-and-stay-in-between
   (cond ((= type 0) (list "\\left(" "\\right)"))
         ((= type 1) (list "\\left[" "\\right]"))
         ((= type 2) (list "\\left\\{" "\\right\\}"))
         (t (error "Unsupported delimiter type!")))))

(defun insert-left-right-parentheses ()
  "Insert \\left( and \\right)."
  (interactive)
  (insert-delimiter 0))

(defun insert-left-right-brackets ()
  "Insert \\left[ and \\right]."
  (interactive)
  (insert-delimiter 1))

(defun insert-left-right-braces ()
  "Insert \\left\\{ and \\right\\}."
  (interactive)
  (insert-delimiter 2))

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-imenu-depth 7)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line nil)
  (org-fontify-whole-block-delimiter-line t)
  (org-special-ctrl-a/e t)
  ;; move attachment files by default
  (org-attach-method 'mv)
  ;; allow attachment inheritance
  (org-attach-use-inheritance t)
  ;; allow alphabetical list
  (org-list-allow-alphabetical t)
  ;; Block changing TODO to DONE when dependency not satisfied
  (org-enforce-todo-dependencies t)
  ;; skip scheduled if deadline is shown unless scheduled today
  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
  ;; no deadline warning before scheduled date
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  ;; hide */~+ markers
  (org-hide-emphasis-markers t)
  ;; don't prompt before running code in org
  (org-confirm-babel-evaluate nil)
  ;; use syntax highlighting in source blocks when editing
  (org-src-fontify-natively t)
  ;; tab acts like its major mode
  (org-src-tab-acts-natively t)
  ;; preserve indentation in source blocks
  (org-src-preserve-indentation t)
  ;; color latex code in org
  (org-highlight-latex-and-related '(latex))
  ;; latex pdf
  (org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
  ;; latex packages
  (org-latex-packages-alist
   (list (list "" "bm" t)
         (list "" (expand-file-name "~/OneDrive/assets/templates/dl-math") t)))
  ;; export to latex with smart quotes
  (org-export-with-smart-quotes t)
  ;; use zotero styles for csl exports
  (org-cite-csl-styles-dir (expand-file-name "~/Zotero/styles/"))
  ;; use id to insert link
  (org-id-link-to-org-use-id 'create-if-interactive)
  :config
  ;; open pdf with emacs
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-agenda-files (list (expand-file-name
                                "~/OneDrive/org-life/agenda.org")))
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c M-L" . org-store-link)
         ("C-c t m" . TeX-insert-macro)
         ("C-c t e" . LaTeX-environment)
         ("C-c t 0" . insert-left-right-parentheses)
         ("C-c t [" . insert-left-right-brackets)
         ("C-c t ]" . insert-left-right-braces))
  :hook
  (org-mode . zq/prettify-org))

;; modern look
(use-package org-modern
  :ensure t
  :defer t
  :config
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka" :height 120 :weight 'regular)
  :custom
  (org-modern-table nil)
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

;; org-roam
(defun update-last-modified-field ()
  "Update `last_modified' at save."
  (setq-local time-stamp-active t
              time-stamp-start "#\\+last_modified:[ \t]+\\\\?\\[+"
              time-stamp-end "\\\\?\\]"
              time-stamp-format "%Y-%02m-%02d %3a %02H:%02M")
  (add-hook 'before-save-hook 'time-stamp nil 'local))

(defun org-roam-ref-add-from-key (key)
  "Add reference whose citation key is KEY to the node at point."
  (interactive "sCitation key: ")
  (org-roam-ref-add (format "[cite:@%s]" key)))

(use-package org-roam
  :ensure t
  :defer t
  :delight "-Ω-"
  :defines (org-roam-capture-templates org-roam-mode-map)
  :custom
  (org-roam-directory (file-truename (expand-file-name "~/OneDrive/org-roam")))
  (org-roam-db-location (expand-file-name "~/.cache/emacs/org-roam.db"))
  (org-roam-graph-viewer (if (eq system-type 'darwin) 'open-svg-on-mac "/usr/bin/google-chrome-stable"))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U \n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("c" "concept" plain "%?"
           :if-new (file+head "concepts/${slug}.org"
           "#+title: ${title}\n#+author: Ziqin Gong\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("l" "literature" plain "%?"
           :if-new (file+head "literature/${slug}.org"
           "#+title: ${title}\n#+author: Ziqin Gong\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)
          ("C" "collection" plain "%?"
           :if-new (file+head "collections/${slug}.org"
           "#+title: ${title}\n#+author: Ziqin Gong\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))
  (org-roam-db-autosync-enable)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n r" . org-roam-ref-add-from-key)
         ("C-c n a" . org-roam-alias-add))
  :hook
  ;; enable last_modified
  (org-mode . update-last-modified-field))

;; Visualize nodes
(use-package org-roam-ui
  :ensure t
  :delight
  ;; :after org-roam
  ;; normally we'd recommend hooking orui after org-roam, but since org-roam
  ;; does not have a hookable mode anymore, you're advised to pick something
  ;; yourself if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :bind (("C-c n g" . org-roam-ui-mode)))

;; org to epub
(use-package ox-epub
  :ensure t
  :after org)

;; use auctex preview
(use-package org-auctex
  :ensure nil
  :load-path "lisp"
  :hook
  (org-mode . org-auctex-mode))

;; citation
(use-package citar
  :ensure t
  :defer t
  :custom
  (org-cite-global-bibliography
   (list (expand-file-name "~/OneDrive/zot-references.bib")))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-citeproc-csl-styles-dir (expand-file-name "~/Zotero/styles"))
  (citar-library-paths
   (list (expand-file-name "~/OneDrive/zotero-attachments/")))
  (citar-notes-paths
   (list (expand-file-name "~/OneDrive/org-roam/literature/")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))



;;;========================
;;; everyday use
;;;========================

;; RSS feed reader
(use-package elfeed
  :ensure t
  :custom
  (elfeed-feeds '("https://pypi.org/rss/project/jax-metal/releases.xml")))


(defun open-my-config ()
  "Open my init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


(defun open-my-agenda ()
  "Open agenda file."
  (interactive)
  (find-file (expand-file-name "~/OneDrive/org-life/agenda.org")))


;; use MPV to stream URL
(defun mpv-url (url)
  "Use `mpv' to stream URL."
  (interactive "sURL: ")
  (let ((mpv-buf (get-buffer-create "*mpv*"))
        (ytdl-url (format "ytdl%s" (substring url 5 nil))))
    (start-process "mpv" mpv-buf "mpv" "--quiet" ytdl-url)
    (with-current-buffer mpv-buf
      (insert (format "URL: %s\n" url))
      (insert "\n=============\n\n"))))


;; use MPV to play local media
(defun mpv-media (file)
  "Use `mpv' to play FILE."
  (interactive "fMedia: ")
  (let ((mpv-buf (get-buffer-create "*mpv*"))
        (fpath (expand-file-name file)))
    (start-process "mpv" mpv-buf "mpv" "--quiet" fpath)
    (with-current-buffer mpv-buf
      (insert "\n=============\n\n"))))


;; play white noise with MPV
(defun white-noise (file)
  "Play white noise from FILE."
  (interactive "fWhite noise: ")
  (let ((mpv-proc (start-process "mpv" nil
                                 "mpv" "--quiet" (expand-file-name file))))
    (when mpv-proc
      (set-process-sentinel
       mpv-proc
       (lambda (proc status)
         (if (string= status "finished\n")
             (message "White noise ended. Take a break!")))))))


(use-package utils
  :ensure nil
  :load-path "lisp"
  :bind
  (("C-c z d" . delete-file-and-buffer)
   ("C-c z c" . open-my-config)
   ("C-c z a" . open-my-agenda)
   ("C-c z m u" . mpv-url)
   ("C-c z m m" . mpv-media)
   ("C-c z w" . white-noise)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
