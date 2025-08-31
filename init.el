;;; init.el --- Stuff all around -*- lexical-binding: t; -*-

;;; Commentary:
;; Modified version of
;; https://gitlab.com/nathanfurnal/dotemacs/-/blob/master/init.el

;;; Code:

;; Elpaca installer
(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
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
  ;; make the window title the buffer name
  (setq-default frame-title-format '("%b"))
  ;; visually indicates pair of matching parentheses
  (show-paren-mode t)
  ;; delete selection when start typing
  (delete-selection-mode t)
  ;; increase the amount of data which emacs read from processes
  (setq read-process-output-max (* 1024 1024))
  :config
  ;; default fonts
  (set-face-attribute 'default nil :family "SF Mono" :height 120 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka Slab" :height 120 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 120 :weight 'regular)
  :custom
  ;; disable startup screen
  (inhibit-startup-screen t)
  ;; no need to have an elisp buffer when starting up
  (initial-major-mode 'fundamental-mode)
  ;; welcome message in *scratch*
  (initial-scratch-message nil)
  ;; line-stype cursor
  (cursor-type 'bar)
  ;; no ringing
  (ring-bell-function #'ignore)
  ;; set fill column to 100 rather than 70 in all cases
  (fill-column 100)
  ;; never use tabs for indentation
  (indent-tabs-mode nil)
  ;; a littler looser layout
  (line-spacing 0.2)
  ;; stop confirming the killing of processes
  (confirm-kill-processes nil)
  ;; short answers (y/n/p)
  (use-short-answers t)
  ;; backup in the same location
  (backup-directory-alist '(("." . "/tmp/emacs-backup")))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 7)
  (kept-old-versions 3)
  :hook
  ;; show the fill column when programming
  (prog-mode . display-fill-column-indicator-mode))

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



;;;========================
;;; appearance
;;;========================


;; fonts
(defun zq/set-fonts ()
  "Set default fonts.  Since daemon-mode doesn't respect these settings, manually set them."
  (interactive)
  (set-fontset-font "fontset-default" 'han "Noto Sans CJK SC")
  (set-fontset-font "fontset-default" 'cjk-misc "Noto Sans CJK SC")
  (set-fontset-font "fontset-default" 'devanagari "Noto Sans Devanagari")
  (message "Set fonts for 'han 'cjk-misc and 'devanagari. Done!"))

;; column and line number
(column-number-mode)
(global-display-line-numbers-mode t)

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
(use-package modus-themes
  :ensure t
  :demand t  ;; load immediately
  :config
  (load-theme 'modus-operandi-tinted :no-confirm)
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-common-palette-overrides
   '((bg-mode-line-active bg-main)
     (bg-mode-line-inactive bg-dim)
     (border-mode-line-inactive bg-inactive)
     (fringe subtle)
     (bg-paren-match bg-yellow-intense)))
  (modus-themes-headings '((1 . (variable-pitch 1.5))
                           (2 . (variable-pitch 1.3))
                           (3 . (variable-pitch 1.2))
                           (t . (1.1))))
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :bind ([f5] . modus-themes-toggle))

;; indentation guide bars
(use-package indent-bars
  :ensure t
  :custom
  (indent-bars-no-descend-lists t)  ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; scope focus for python
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  :hook ((python-ts-mode yaml-mode) . indent-bars-mode))

(defun zq/org-simple-python-mode ()
  "Inhibit hooks from running for `org' src blocks for Python."
  (if (string-prefix-p " *org-src-fontification:" (buffer-name))
      (delay-mode-hooks (python-mode))
    (python-mode)))


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
  (global-corfu-mode))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



;;;========================
;;; minibuffer completion
;;;========================


;; vertical completion UI based on the default completion system
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

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

(use-package ellama
  :ensure t
  :bind ("C-c z l" . ellama-transient-main-menu)
  :init
  (require 'llm-ollama)
  (setopt ellama-provider (make-llm-ollama :chat-model "llama3.1:8b"))
  (setopt ellama-coding-provider (make-llm-ollama :chat-model "qwen2.5-coder:7b"
                                                  :embedding-model "nomic-embed-text"))
  (setopt ellama-naming-provider (make-llm-ollama :chat-model "llama3.1:8b"
                                                  :embedding-model "nomic-embed-text"))
  (setopt ellama-summarization-provider (make-llm-ollama :chat-model "llama3.1:8b"
                                                         :embedding-model "nomic-embed-text"))
  (setopt ellama-translation-provider (make-llm-ollama :chat-model "qwen2.5:7b"
                                                       :embedding-model "nomic-embed-text"))
  :custom
  (ellama-language "Chinese"))



;;;========================
;;; development
;;;========================


;; remote
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)
  :custom
  (tramp-own-remote-path '("~/.local/bin" "~/.cargo/bin"))
  ;; disable file lock for faster tramp
  (remote-file-name-inhibit-locks t)
  ;; don't find vc on remote, as I won't use emacs to vc
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

;; smart dash, becomes _ automatically
(use-package smart-dash
  :ensure (:host github :repo "malsyned/smart-dash")
  :hook (python-base-mode . smart-dash-mode))

;; docker
(use-package docker
  :ensure t
  :mode ("Dockerfile" . dockerfile-ts-mode)
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
  ;; ignore git repos installed by package managers
  (projectile-ignored-project-function 'start-with-package-manager-prefix-p)
  (projectile-jj-command "jj file list --no-pager . | tr '\\n' '\\0'")
  ;; reduce the range for projectile to search for project root for faster tramp
  (projectile-project-root-functions '(projectile-root-local
                                       projectile-root-marked
                                       projectile-root-bottom-up))
  (projectile-project-root-files-bottom-up '(".jj" ".git"))
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

(defun start-with-package-manager-prefix-p (string)
  "Return t if STRING start with locations where package managers store files."
  (or (string-prefix-p "/opt/homebrew" string)
      (string-prefix-p "~/.emacs.d/elpaca" string)))

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
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.3"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          ;; (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  (dolist (mapping '((python-mode . python-ts-mode)
                     (sh-mode . bash-ts-mode)
                     (rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

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
  (eglot-send-changes-idle-time 0.1)
  :hook
  ((rust-mode c-mode c++-mode) . eglot-ensure)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; pylsp config
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins ( :black (:enabled :json-false)
                                     :autopep8 (:enabled :json-false)
                                     :yapf ( :enabled :json-false)
                                     :ruff ( :enabled t
                                             :formatEnabled t
                                             :lineLength 100)
                                     :jedi_completion
                                     ( :enabled t
                                       :include_params t
                                       :include_class_objects t
                                       :include_function_objects t
                                       :fuzzy t
                                       :eager t
                                       :cache_for ["matplotlib" "numpy" "torch" "jax" "flax"])
                                     :rope_autoimport ( :enabled :json-false)
                                     ;; :pylsp_mypy ( :live_mode :json-false :dmypy t)
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

;; boost eglot -- requires "cargo install emacs-lsp-booster"
(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

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
  (setq python-indent-offset 4)
  :hook
  (python-base-mode . eglot-ensure))

;; manually set venv
(defun zq/setup-python-project ()
  "Interactively create a .dir-locals.el file for the current Python project."
  (interactive)
  (let* ((project-root (or (locate-dominating-file default-directory "pyproject.toml")
                           (locate-dominating-file default-directory ".jj")
                           (locate-dominating-file default-directory ".git")
                           (read-directory-name "Enter project root: ")))
         (dir-locals-file (concat (file-name-as-directory project-root) ".dir-locals.el")))

    (when (or (not (file-exists-p dir-locals-file))
              (y-or-n-p (format "File %s already exists.  Overwrite it? " dir-locals-file)))

      (let* (;; Use read-directory-name for venv path with completion
             (venv-root (condition-case nil
                            (read-directory-name "Enter virtualenv path (C-g for global):" project-root)
                          (quit nil)))
             (venv-root (if venv-root (expand-file-name venv-root) nil))
             ;; Use read-file-name for python executable with completion
             (base-python (if venv-root
                              (concat venv-root "bin/python")
                            (read-file-name "Enter global Python executable path: " nil nil t)))
             (env-bin-dir (file-name-directory base-python))
             (user-bin-dir (expand-file-name "~/.local/bin/"))

             ;; Helper to find an executable or return "nil" for the template
             (find-exec (lambda (name)
                          (let ((env-path (concat env-bin-dir name))
                                (user-path (concat user-bin-dir name)))
                            (cond ((and env-path (file-exists-p env-path)) (format "\"%s\"" env-path))
                                  ((and user-path (file-exists-p user-path)) (format "\"%s\"" user-path))
                                  (t "nil")))))
             ;; Helper for the pylsp command itself
             (pylsp-cmd (let ((path (concat user-bin-dir "pylsp")))
                          (if (and path (file-exists-p path)) path "pylsp")))

             ;; Generate content
             (content
              (format
               "((python-mode
  . ((eval . (with-eval-after-load 'eglot
               (progn
                 ;; Safely modify eglot-server-programs for the local buffer
                 (setq-local eglot-server-programs (copy-alist eglot-server-programs))
                 (assq-delete-all 'python-mode eglot-server-programs)
                 (add-to-list 'eglot-server-programs
                              '(python-mode . (\"%s\" :initializationOptions
                                               (:pylsp
                                                (:plugins
                                                 ( :jedi (:environment %s)
                                                   :ruff (:executable %s)
                                                   :pylsp_mypy (:overrides [\"--python-executable\"
                                                                            \"%s\"
                                                                            t])
                                                   :flake8 (:executable %s)
                                                   :pylint (:executable %s))))))))))

     (python-shell-interpreter . %s)
     (python-shell-virtualenv-root . %s)

     (flycheck-python-pylint-executable . %s)
     (flycheck-python-mypy-executable . %s)
     (flycheck-python-mypy-python-executable . \"%s\")
     (flycheck-python-ruff-executable . %s)

     (dap-python-executable . \"%s\")

     (python-black-command . %s)
     (python-isort-command . %s)
     (ruff-format-command . %s))))"
               ;; Values for format string
               pylsp-cmd
               (if venv-root (format "\"%s\"" venv-root) "nil") ; for jedi
               (funcall find-exec "ruff")
               base-python
               (funcall find-exec "flake8")
               (funcall find-exec "pylint")
               (funcall find-exec "ipython")
               (if venv-root (format "\"%s\"" venv-root) "nil") ; for shell
               (funcall find-exec "pylint")
               (funcall find-exec "mypy")
               base-python
               (funcall find-exec "ruff")
               base-python
               (funcall find-exec "black")
               (funcall find-exec "isort")
               (funcall find-exec "ruff"))))

        (with-temp-file dir-locals-file
          (insert content))
        (message "Created %s" dir-locals-file)))))

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

;; lua
(use-package lua-mode
  :ensure t)

;; yaml
(use-package yaml-mode
  :ensure t
  :delight
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :bind
  (:map yaml-mode-map ("C-m" . newline-and-indent)))

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
(use-package auctex
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git"
                  :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'auctex) AUCTeX-version)))

;; shortcuts for math templates or symbols
;; https://github.com/cdominik/cdlatex TODO: org-roam note
;; or `cdlatex-command-help' to see a full list of abbr.
(use-package cdlatex
  :ensure t
  :delight
  :custom
  (cdlatex-command-alist
   '(("bb" "Insert \\mathbb{}" "\\mathbb{?}" cdlatex-position-cursor nil nil t)
     ("kl" "Insert D_{\\textrm{KL}}" "D_{\\textrm{KL}} " nil nil nil t)
     ("bm" "Insert \\bm{}" "\\bm{?}" cdlatex-position-cursor nil nil t)))
  :hook
  (LaTeX-mode . turn-on-cdlatex))

;; async and automatic render for LaTeX images and equations
;; broken in org files with src blocks
(use-package xenops
  :ensure t
  :delight
  :hook
  (LaTeX-mode . xenops-mode))

(use-package latex
  :ensure nil
  :init
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
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . prettify-symbols-mode)
  (LaTeX-mode . turn-on-auto-fill))

(use-package reftex
  :ensure nil
  :delight
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
              time-stamp-line-limit 9
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

;; org to markdown (github flavored)
(use-package ox-gfm
  :ensure t
  :after org)

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

;; tighter Citar and Org-roam integration
(use-package citar-org-roam
  :ensure t
  :delight
  :after (org-roam citar)
  :config
  (citar-org-roam-mode))



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

(defun translate-markdown-buffer-to-org-via-ellama ()
  "Use `ellama--translate-markdown-to-org-filter' to translate markdown to org in current buffer."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (buf (get-buffer-create (format "md2org %s..." (substring text 0 13)))))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (org-mode)
      (insert (ellama--translate-markdown-to-org-filter text)))
    (switch-to-buffer buf)))

(use-package utils
  :ensure nil
  :load-path "lisp"
  :bind
  (("C-c z d" . delete-file-and-buffer)
   ("C-c z c" . open-my-config)
   ("C-c z a" . open-my-agenda)
   ("C-c z m u" . mpv-url)
   ("C-c z m m" . mpv-media)))


;; custom file
(load "~/.config/emacs-custom.el")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
