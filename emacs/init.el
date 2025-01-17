;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package jsonrpc :ensure t)

(use-package emacs
  :custom
  (backup-directory-alist '(("" . "~/.bak")))
  (auto-save-file-name-transforms '((".*" "~/.bak/" t)))
  (split-height-threshold nil)
  (split-width-threshold nil)
  (display-line-numbers-type 'relative)
  (require-final-newline 'visit)
  (inhibit-startup-screen t)
  (initial-major-mode 'lisp-interaction-mode)
  (global-auto-revert-mode t)
  (select-enable-primary t)
  (comint-scroll-show-maximum-output t) ;for exec in shell
  (warning-suppress-types '((comp) comp))
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defvar crm-separator)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq gc-cons-threshold 100000000)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (setq frame-title-format
        '("emacs@" system-name ":"
          (:eval (or (buffer-file-name)
                     default-directory))))
  ;;# 右から左に読む言語に対応させないことで描画高速化
  (setq-default bidi-display-reordering nil)
  (setq-default indicate-empty-lines t)
  ;;# Edit
  (show-paren-mode 1)
  (which-function-mode 1)
  (global-auto-revert-mode 1)
  ;;# Scroll
  (setq scroll-conservatively 35
        scroll-margin 0
        scroll-step 1)
  ;;# fold always
  (setq truncate-lines nil)
  (setq truncate-partial-width-windows nil)
  (setq ring-bell-function 'ignore) ; No Beeps
  ;; like "mkdir -p"
  (add-hook 'find-file-not-found-functions
            #'(lambda () (make-directory (file-name-directory buffer-file-name) t)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; 自動分割を抑制
  (global-display-line-numbers-mode)
  (prefer-coding-system 'utf-8-unix))

(use-package ffap
  :bind (:map text-mode-map
         ([remap find-file] . find-file-at-point)))

(use-package diminish :ensure t)

(use-package recentf
  :init
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))
  (recentf-mode 1)
  :config
  ;; recentf の メッセージをエコーエリア(ミニバッファ)に表示しない
  (defun recentf-save-list-inhibit-message:around (orig-func &rest args)
    (setq inhibit-message t)
    (apply orig-func args)
    (setq inhibit-message nil)
    'around)
  (advice-add 'recentf-cleanup   :around 'recentf-save-list-inhibit-message:around)
  (advice-add 'recentf-save-list :around 'recentf-save-list-inhibit-message:around)
  :custom
  (recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (recentf-max-saved-items 2000)
  (recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/elpa"))
  (recentf-save-file (expand-file-name "~/.bak/emacs/recentf")))

(use-package recentf-ext :ensure t)

(use-package savehist
  :init
  (savehist-mode 1)
  :custom
  (savehist-file (expand-file-name "~/.bak/emacs/history")))

(use-package undohist :ensure t
  :config
  (undohist-initialize)
  :custom
  (undohist-ignored-files '("^/tmp" "COMMIT_EDITMSG" "EDITMSG" "/elpa"))
  (undohist-directory (expand-file-name "~/.bak/emacs/undohist")))

(use-package tramp
  :custom
  (auto-revert-remote-files 't)
  :config
  (setq tramp-auto-save-directory (expand-file-name "~/.bak/emacs/tramp")))

;;# key binding
(defun event-apply-meta-control-modifier (_)
  "\\Add the Meta-Control modifier to the following event.
For example, type \\[event-apply-meta-control-modifier] % to enter Meta-Control-%."
  (vector (event-apply-modifier
           (event-apply-modifier (read-event) 'control 26 "C-")
           'meta 27 "M-")))

(bind-keys* :map global-map
            ("C-x h" . nil) ; delete help
            ("<C-tab>" . other-window)
            ("<C-S-tab>" . (lambda () (interactive) (other-window -1)))
            ;:filter window-system ;; for tiling window manager
            ;:filter (equal system-type 'gnu/linux)
            ;("C-x 3" . make-frame-command)
            ;("C-x 2" . make-frame-command)
            :map function-key-map
            ("C-x @ M" . event-apply-meta-control-modifier)
            :map isearch-mode-map
            ("C-b" . isearch-delete-char)
            :map minibuffer-local-map
            ("C-d" . left-char)
            ("C-n" . right-char)
            ("C-b" . backward-delete-char-untabify)
            ("C-h" . next-line-or-history-element)
            ("C-t" . previous-line-or-history-element))

;;# evil
(defvar last-h-inserted-time (current-time) "The last inserted time")
(defun hh-normal ()
    (interactive)
    ;; (message "%f" (abs (float-time (time-subtract last-h-inserted-time (current-time) ))))
    (if (< (abs (float-time (time-subtract (current-time) last-h-inserted-time))) 0.40)
          (progn
            (if (char-equal (string-to-char "h") (char-before)) (delete-char -1) nil)
            (evil-normal-state))
    (progn
      (setq last-h-inserted-time (current-time))
      (if (and (boundp 'skk-j-mode) skk-j-mode)
          (skk-insert)
        (insert-char (string-to-char "h"))))))
(use-package evil
  :ensure t
  :after (consult)
  :bind (:map evil-ex-search-keymap
         ("C-b" . backward-delete-char-untabify)
         :map evil-normal-state-map
         ("<leader>q" . kill-this-buffer)
         ("<leader>w" . save-buffer)
         ("<leader>W" . save-buffer)
         ("<leader>SPC" . consult-buffer)
         :map evil-insert-state-map
         ("h" . hh-normal)
         ("C-d" . backward-char)
         ("C-n" . forward-char)
         ("C-t" . previous-line)
         ("C-h" . next-line)
         ("C-b" . backward-delete-char-untabify))
  :init
  (setq evil-search-module 'evil-search
        evil-ex-substitute-case 'smart
        evil-want-keybinding nil
        evil-want-abbrev-expand-on-insert-exit nil
        evil-shift-width 2)
  (evil-mode 1)
  :config
  (let* ((common-evil-keys
          `(("h" . evil-next-visual-line)
            ("t" . evil-previous-visual-line)
            ("n" . evil-forward-char)
            ("d" . evil-backward-char)
            ("k" . evil-delete)
            ("K" . evil-delete-line)
            ("M" . evil-ex-search-previous)
            ("N" . evil-ex-search-previous)
            ("m" . evil-ex-search-next)
            (,(kbd "C-w") . comment-or-uncomment-region)))
         (evil-modes '(visual motion normal)))
    (mapc (lambda (mode)
            (mapc (lambda (key)
                    (evil-define-key mode 'global (car key) (cdr key)))
                  common-evil-keys))
          evil-modes))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-initial-state 'tabulated-list-mode 'emacs)
  (evil-define-key 'emacs tabulated-list-mode-map
    "t" 'tablist-previous-line
    "h" 'tablist-next-line))

(use-package evil-numbers :ensure t
  :after (evil)
  :config
  (bind-key "+" 'evil-numbers/inc-at-pt evil-normal-state-map)
  (bind-key "-" 'evil-numbers/dec-at-pt evil-normal-state-map))

(use-package evil-collection :ensure t
  :after (evil)
  :init
  (setq evil-want-keybinding nil)
  (evil-collection-init))

(use-package yasnippet :ensure t :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs '("~/.config/emacs/snippets"))
  :config
  (yas-global-mode 1))

;;# Code Jump
(use-package dumb-jump :ensure t
  :custom
  (dumb-jump-default-project (expand-file-name "~"))
  (dumb-jump-force-searcher 'ag)
  :bind (:map dumb-jump-mode-map
         ("C-M-g" . xref-find-definitions)
         :map emacs-lisp-mode-map
         ("C-M-g" . xref-find-definitions)))

;;# completion
(use-package vertico :ensure t
  :init
  (require 'consult)
  (vertico-mode)
  :custom
  (consult-preview-key nil)
  :bind (:map vertico-map
         ("C-t" . 'vertico-previous)
         ("C-h" . 'vertico-next)))

(use-package consult :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function #'(lambda (_) (vc-root-dir)))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-ag :ensure t
  :after (consult)
  :bind (("C-M-f" . consult-ag)))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  (setq completion-category-defaults nil))

(use-package corfu :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-cycle t)
  :init
  (global-corfu-mode))
(use-package corfu-terminal :ensure t
  :after (corfu)
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

;;# skk
(use-package ddskk
  :ensure t
  :bind (("C-x C-j" . skk-mode))
  :after (evil)
  :init
  (setq skk-kakutei-when-unique-candidate t)
  (setq skk-egg-like-newline t)
  (setq skk-kuten-touten-alist
        '((jp . ("." . "," ))
          (en . ("." . ","))))
  (setq-default skk-kutouten-type 'en)
  (setq skk-user-directory "~/skk"))

(use-package dired
  :after (evil evil-collection)
  :commands (dired-mode)
  :bind (:map dired-mode-map ("SPC" . nil))
  :config
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-define-key 'normal dired-mode-map
    (kbd "SPC") nil
    "t" 'dired-previous-line
    "h" 'dired-next-line
    "d" 'dired-up-directory
    "n" 'dired-find-alternate-file))

(use-package ibuffer
  :commands (ibuffer-mode)
  :after (evil evil-collection)
  :bind (:map ibuffer-mode-map ("SPC" . nil))
  :config
  (evil-make-overriding-map ibuffer-mode-map 'normal)
  (evil-define-key 'normal ibuffer-mode-map
    (kbd "SPC") nil
    "t" 'evil-previous-visual-line
    "h" 'evil-next-visual-line
    "d" 'evil-backward-char
    "n" 'evil-forward-char))

(use-package arc-mode
  :bind (:map archive-mode-map
         ("t" . archive-previous-line)
         ("h" . archive-next-line))
  :commands (archive-mode)
  :after (evil))

(use-package tar-mode
  :bind (:map tar-mode-map
         ("t" . tar-previous-line)
         ("h" . tar-next-line))
  :commands (tar-mode)
  :after (evil))

;;# parens
;; http://d.hatena.ne.jp/murase_syuka/20140815/1408061850
(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;# git
(use-package magit :ensure t
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :after (evil evil-collection)
  :bind (("C-c C-g" . magit-status)
         :map magit-mode-map
         ("t" . evil-previous-visual-line)
         ("h" . evil-next-visual-line)
         ("T" . magit-section-backward-sibling)
         ("H" . magit-section-forward-sibling))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(use-package gcmh
  :ensure t
  :demand t
  :diminish ""
  :config
  (gcmh-mode 1))

(use-package eglot
  :ensure t
  :bind
  (:map eglot-mode-map
        ("C-c h" . eldoc)
        ("C-M-g" . xref-find-definitions)))
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :after eglot
  :config
  (eglot-booster-mode))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package shell
  :config
  (setenv "EMACS" "1"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;# Theme
(use-package modus-themes :ensure t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-region '(bg-only no-extend))
  :config
  ;; Load the theme of your choice:
  ;; (load-theme 'modus-vivendi t)
  (load-theme 'modus-operandi t))

;;# fonts
(when (equal system-type 'darwin)
  (let* ((size 14)
         (asciifont "Droid Sans Mono Dotted for Powerline")
         (jpfont "Osaka")
         (h (* size 10))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil '(#x0080 . #x024F) fontspec)
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))

(when (equal system-type 'gnu/linux)
  (progn
    (set-face-attribute 'default nil :family "Source Han Code JP N" :height 140)
    (set-frame-font "Source Han Code JP N" nil t)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md" . markdown-mode)
         ("\\.md.erb\\'" . markdown-mode)
         ("\\.howm\\'" . markdown-mode))
  ;; :init
  ;; (setq markdown-command "redcarpet")
  :config
  (defun outline-imenu-create-index ()
    (let (index)
      (goto-char (point-min))
      (while (re-search-forward "^\*\s*\\(.+\\)" (point-max) t)
        (push (cons (match-string 1) (match-beginning 1)) index))
      (nreverse index)))
  (add-hook 'markdown-mode #'(lambda ()
                              (setq imenu-create-index-function 'outline-imenu-create-index)
                              (auto-fill-mode))))

(use-package cc-mode
  :commands (c++-mode)
  :mode (("\\.c\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.h\\'" . c++-mode))
  :hook (c-mode-common-hook . (lambda ()
                                (setq c-default-style "bsd")
                                (setq indent-tabs-mode nil)
                                (setq c-basic-offset 4)))
  :config
  (c-set-offset 'cpp-macro 0 nil))

(use-package yatex
  :ensure t
  :commands (yatex-mode)
  :mode ("\\.tex\\'" . yatex-mode)
  :hook ((skk-mode-hook . (lambda ()
                            (if (eq major-mode 'yatex-mode)
                                (progn
                                  (define-key skk-j-mode-map "\\" 'self-insert-command)
                                  (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)))))
         (yatex-mode-hook . (lambda ()
                              (reftex-mode 1)
                              (auto-fill-mode -2)
                              (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
                              (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))
  :init
  (setq YaTeX-kanji-code 4))

;;# OCaml
(use-package tuareg
  :ensure t
  :commands (tuareg-mode)
  :mode (("\\.sml\\'" . tuareg-mode)
         ("\\.smi\\'" . tuareg-mode)
         ("\\.ml\\'" . tuareg-mode)
         ("\\.mlg\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :config
  ;; work around
  (defun tuareg-abbrev-hook () nil))

(use-package haskell-mode :ensure t)

;;# F#
(use-package fsharp-mode :defer t :ensure t
  :config
  (setq inferior-fsharp-program (concat (getenv "DOTNET_ROOT") "/dotnet fsi")))

;;# python
(use-package blacken :ensure t)
(use-package py-isort :ensure t
  :custom
  (py-isort-options '("--multi-line=3"
                      "--line-length=88"
                      "--trailing-comma"
                      "--use-parentheses"
                      "--ensure-newline-before-comments"
                      "--force-grid-wrap=0")))
(defun format-py ()
  (when (eq major-mode 'python-mode)
    (blacken-buffer)
    (py-isort-before-save)))
(use-package python-mode :ensure t
  :mode (("\\.py\\`" . python-mode))
  :bind
  (:map python-mode-map
        ("C-c C-b" . python-shell-send-buffer))
  :hook
  (before-save . format-py)
  (python-ts-mode . (lambda ()
                   (require 'eglot)
                   (let ((command
                          (mapconcat 'identity `("cd" ,default-directory "&&" "hatch" "run" "pylsp" "--tcp" "--port" "$0") " ")))
                     (add-to-list 'eglot-server-programs
                                  `(python-ts-mode . ("bash" "-c" ,command :autoport))))
                   (eglot-ensure))))
(use-package cython-mode :ensure t)

;;# R
(use-package ess
  :ensure t
  :init (require 'ess-site))

;;# Docker
(use-package dockerfile-mode :ensure t
  :bind ("C-c C-d" . docker))

;;# Rust
(use-package rustic :ensure t
  ;https://syohex.hatenablog.com/entry/2022/11/08/000610
  :config
  (defun my/find-rust-project-root (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (list 'vc 'Git root)))
  (defun my/rust-mode-hook ()
    (setq-local project-find-functions (list #'my/find-rust-project-root)))
  (add-hook 'rust-mode-hook #'my/rust-mode-hook)
  :custom
  (rustic-lsp-client 'eglot))

;;# TypeScript
(use-package typescript-mode :ensure t
  :init
  (defun typescript-eglot ()
    (require 'eglot)
    (eglot-ensure))
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-mode . typescript-eglot)
         (tsx-ts-mode . typescript-eglot)))

;;# typst
(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode"
                 :files (:defaults "*.el")))

;;# Nix
(use-package nix-mode :ensure t :mode "\\.nix\\'")

;; sync with x clipboard
(unless window-system
  (cond
   ((getenv "WAYLAND_DISPLAY")
    (progn
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            :command '("wl-copy" "-f" "-n")
                                            :connection-type 'pipe))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil ; should return nil if we're the current paste owner
          (shell-command-to-string "wl-paste -n | tr -d \r")))
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))
   ((getenv "DISPLAY")
    (progn
      ;; Callback for when user cuts
      (defun xsel-cut-function (text &optional _)
        ;; Insert text to temp-buffer, and "send" content to xsel stdin
        (with-temp-buffer
          (insert text)
          ;; I prefer using the "clipboard" selection (the one the
          ;; typically is used by c-c/c-v) before the primary selection
          ;; (that uses mouse-select/middle-button-click)
          (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
      ;; Call back for when user pastes
      (defun xsel-paste-function()
        ;; Find out what is current selection by xsel. If it is different
        ;; from the top of the kill-ring (car kill-ring), then return
        ;; it. Else, nil is returned, so whatever is in the top of the
        ;; kill-ring will be used.
        (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
          (unless (string= (car kill-ring) xsel-output)
            xsel-output)))
      ;; Attach callbacks to hooks
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function)
      ;; Idea from
      ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
      ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
      ))))
