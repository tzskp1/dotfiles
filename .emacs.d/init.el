;; -*- lexical-binding: t -*-

;; package requirement
;; silversearcher-ag
;; opam

(defun event-apply-meta-control-modifier (ignore-prompt)
  "\\Add the Meta-Control modifier to the following event.
For example, type \\[event-apply-meta-control-modifier] % to enter Meta-Control-%."
  (vector (event-apply-modifier
           (event-apply-modifier (read-event) 'control 26 "C-")
           'meta 27 "M-")))

;; killing custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))


;; コンパイル時にパッケージをインストールする.
(eval-when-compile
  (package-initialize)
  (setq package-archives
        '(
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (unless package-archive-contents (package-refresh-contents))
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (package-install-selected-packages))

(package-initialize)

(require 'use-package)

(use-package jsonrpc :ensure t)

(use-package emacs
  :custom
  (backup-directory-alist '(("" . "~/bak")))
  (split-height-threshold nil)
  (split-width-threshold nil)
  (display-line-numbers-type 'relative)
  (require-final-newline 'visit)
  (inhibit-startup-screen t)
  (initial-major-mode 'lisp-interaction-mode)
  (select-enable-primary t)
  (comint-scroll-show-maximum-output t) ;for exec in shell
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
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
  (setq enable-recursive-minibuffers t)
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
            '(lambda () (make-directory (file-name-directory buffer-file-name) t)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; 自動分割を抑制
  (global-display-line-numbers-mode)
  (prefer-coding-system 'utf-8-unix)
)

(use-package diminish :ensure t)

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))
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
  (recentf-max-saved-items 2000)
  (recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/elpa"))
  (recentf-save-file (expand-file-name "~/.bak/emacs/recentf")))

(use-package recentf-ext :ensure t)

(use-package savehist :ensure t
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
(use-package undo-tree :diminish "")

(use-package tramp
  :config
  (setq tramp-auto-save-directory (expand-file-name "~/.bak/emacs/tramp")))

;;# key binding
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
            ("C-m" . ret)
            :map minibuffer-local-map
            ("C-d" . left-char)
            ("C-n" . right-char)
            ("C-b" . backward-delete-char-untabify)
            ("C-h" . next-line-or-history-element)
            ("C-t" . previous-line-or-history-element))

;;# evil
(use-package evil-leader :ensure t
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "q" 'kill-this-buffer
    "w" 'save-buffer
    "<SPC>" 'consult-buffer)
  (kill-buffer (messages-buffer)))

(eval
`(use-package evil :ensure t
  :init
  (evil-mode 1)
  :custom
  (evil-ex-substitute-case 'smart)
  (evil-want-abbrev-expand-on-insert-exit 'nil)
  (evil-search-module 'evil-search)
  (evil-shift-width 2)
  :bind (:map evil-ex-search-keymap
         ("C-b" . backward-delete-char-untabify)
         :map evil-insert-state-map
         ("h" . hh-normal)
         ("C-d" . backward-char)
         ("C-n" . forward-char)
         ("C-t" . previous-line)
         ("C-b" . backward-delete-char-untabify)
         ("C-h" . next-line)
         ,@(mapcan (lambda (x)
                     (list ':map x
                           '("h" . evil-next-visual-line)
                           '("t" . evil-previous-visual-line)
                           '("n" . evil-forward-char)
                           '("d" . evil-backward-char)
                           '("k" . evil-delete)
                           '("K" . evil-delete-line)
                           '("M" . evil-ex-search-previous)
                           '("N" . evil-ex-search-previous)
                           '("m" . evil-ex-search-next)
                           '("C-w" . comment-or-uncomment-region)))
                   '(evil-visual-state-map evil-motion-state-map evil-normal-state-map)))
  :config
  (evil-set-initial-state 'tabulated-list-mode 'emacs)
  (evil-define-key 'emacs tabulated-list-mode-map
    "t" 'tablist-previous-line
    "h" 'tablist-next-line)))

(use-package evil-numbers :ensure t
  :after (evil)
  :config
  (bind-key "+" 'evil-numbers/inc-at-pt evil-normal-state-map)
  (bind-key "-" 'evil-numbers/dec-at-pt evil-normal-state-map))

(use-package evil-collection
  :ensure t
  :after (evil)
  :init (evil-collection-init))

(defvar last-h-inserted-time (current-time) "The last inserted time")
(defun hh-normal ()
    (interactive)
    ;; (message "%f" (abs (float-time (time-subtract last-h-inserted-time (current-time) ))))
    (if (< (abs (float-time (time-subtract (current-time) last-h-inserted-time))) 0.20)
          (progn
            (if (char-equal (string-to-char "h") (char-before)) (delete-backward-char 1) nil)
            (evil-normal-state))
    (progn
      (setq last-h-inserted-time (current-time))
      (if (and (boundp 'skk-j-mode) skk-j-mode)
          (skk-insert)
        (insert-char (string-to-char "h"))))))

(use-package yasnippet :ensure t :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets"))

;;# Code Jump
(use-package dumb-jump :ensure t
  :custom
  (dumb-jump-default-project (expand-file-name "~"))
  (dumb-jump-selector 'ivy)
  (dumb-jump-force-searcher 'ag)
  :bind (:map dumb-jump-mode-map
         ("C-M-g" . dumb-jump-go)
         :map emacs-lisp-mode-map
         ("C-M-g" . xref-find-definitions)))

(use-package smart-jump
  :ensure t
  :custom
  (smart-jump-jump-key "C-M-g")
  :config
  (smart-jump-setup-default-registers))

;;# completion
(use-package company :ensure t :diminish ""
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous)
         :map company-search-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 4)
  (company-selection-wrap-around t)
  :init
  (global-company-mode))
(use-package company-flx :ensure t
  :after (company)
  :init
  (company-flx-mode +1))

;;# skk
(use-package ddskk
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
  :commands (dired-mode)
  :after (evil evil-collection)
  :config
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-define-key 'normal dired-mode-map
    "t" 'dired-previous-line
    "h" 'dired-next-line
    "d" 'dired-up-directory
    "n" 'dired-find-alternate-file))

(use-package ibuffer
  :commands (ibuffer-mode)
  :after (evil evil-collection)
  :config
  (evil-make-overriding-map ibuffer-mode-map 'normal)
  (evil-define-key 'normal ibuffer-mode-map
    "t" 'evil-previous-visual-line
    "h" 'evil-next-visual-line
    "d" 'evil-backward-char
    "n" 'evil-forward-char))

(use-package arc-mode
  :bind (:map archive-mode-map
         ("t" . 'archive-previous-line)
         ("h" . 'archive-next-line))
  :commands (archive-mode)
  :after (evil))

(use-package tar-mode
  :bind (:map tar-mode-map
         ("t" . 'tar-previous-line)
         ("h" . 'tar-next-line))
  :commands (tar-mode)
  :after (evil))

;;# parens
;; http://d.hatena.ne.jp/murase_syuka/20140815/1408061850
(use-package rainbow-delimiters :ensure t
  :config
  (use-package color
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (use-package cl-lib)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30)))))

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

(use-package lsp-mode :ensure t
  :after (company)
  :bind
  (:map lsp-mode-map
        ("C-h" . company-select-next)
        ("C-t" . company-select-previous)
        ("C-M-g" . xref-find-definitions))
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rust-mode . lsp)
         (python-mode . lsp))
  :commands lsp)

(use-package lsp-ui :ensure t :commands lsp-ui-mode)

(use-package shell
  :config
  (setenv "EMACS" "1"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;# Theme
(use-package modus-themes :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
  :config
  ;; Load the theme of your choice:
  ;; (load-theme 'modus-operandi)
  (load-theme 'modus-vivendi)
  )

;;# fonts
(when (equal system-type 'darwin)
  (let* ((size 12)
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
  (add-hook 'markdown-mode '(lambda ()
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
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code 4))

;;# OCaml
(use-package tuareg
  :commands (tuareg-mode)
  :mode (("\\.sml\\'" . tuareg-mode)
         ("\\.smi\\'" . tuareg-mode)
         ("\\.ml\\'" . tuareg-mode)
         ("\\.mlg\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :config
  ;; work around
  (defun tuareg-abbrev-hook () nil))

;; opam init
;; opam install merlin utop core
;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(when (executable-find "opam")
  (dolist
      (var (car (read-from-string
                 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  ;; Update the emacs path
  (setq exec-path (split-string (getenv "PATH") path-separator))
  ;; Update the emacs load path
  (add-to-list 'load-path (concat (getenv "OCAML_TOPLEVEL_PATH")
                                  "/../../share/emacs/site-lisp")))

(use-package merlin
  :after (tuareg company evil-collection)
  :commands (merlin-mode)
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  :config
  (evil-make-overriding-map merlin-mode-map 'normal)
  (add-to-list 'company-backends 'merlin-company-backend)
  (setq merlin-command 'opam)
  :bind
  (:map merlin-mode-map
        ("C-M-g" . merlin-locate)))

(use-package haskell-mode :ensure t)

;;# F#
(use-package fsharp-mode :defer t :ensure t
  :config
  (setq inferior-fsharp-program (concat (getenv "DOTNET_ROOT") "/dotnet fsi")))

;;# python
(use-package pipenv :ensure t
  :mode (("\\.py\\`" . python-mode))
  :bind
  (:map python-mode-map
        ("C-c C-b" . python-shell-send-buffer))
  :custom
  (python-environment-virtualenv (list "virtualenv"))
  :hook
  (python-mode . (lambda ()
                   (pipenv-mode)
                   )))
(use-package ein :ensure t)

;;# Coq
;; Open .v files with Proof General's Coq mode
(use-package proof-general :ensure t
  :hook
  (coq-mode . (lambda ()
  (font-lock-add-keywords nil
      '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))
                (dumb-jump-mode -1)
                (company-coq-mode 1)))
  :mode (("\\.v\\`" . coq-mode)))
  ;; :custom
  ;; (coq-prog-name "/Users/tk/coq/bin/coqtop"))
(use-package company-coq :ensure t
:bind (:map company-coq-map
        ("C-M-g" . company-coq-jump-to-definition)))

;;# Docker
(use-package docker :ensure t
  :config
  (use-package docker-tramp :ensure t
    :custom
    (docker-tramp-use-names t))
  (use-package dockerfile-mode :ensure t)
  :bind ("C-c C-d" . docker))

(require 'json)
(setq dropbox-path
 (cdadar (json-read-file "~/.dropbox/info.json")))
(use-package org :ensure t
  :init
  (require 'org-agenda)
  :custom
  (org-directory (concat (file-name-as-directory dropbox-path) "org"))
  (org-agenda-files
   (list (concat (file-name-as-directory dropbox-path) "org/agenda.org")))
  (org-capture-templates
   `(("T" "TODO" entry (file+headline ,(concat (file-name-as-directory dropbox-path) "org/TODO.org") "Inbox")
      "*** TODO %?\n    CAPTURED_AT: %a\n    %i")))
  :bind (:map global-map
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda))
  (:map org-agenda-mode-map
  ("t" . org-agenda-previous-line)
  ("h" . org-agenda-next-line)))

;;# Rust
(use-package rustic :ensure t
  :custom
  (rustic-lsp-client 'lsp-mode))

(use-package reason-mode :ensure t)
(use-package lean-mode :ensure t)

;;# Michelson
(load "~/.emacs.d/emacs_michelson-mode.el" nil t)
(setq michelson-client-command "~/tezos/tezos-client -A localhost -P 8732")
(setq michelson-alphanet nil)

;;# TypeScript
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
(use-package typescript-mode :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package csharp-mode :ensure t)

;; sync with x clipboard
(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    (defun xsel-cut-function (text &optional push)
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
          xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))

(use-package vertico :ensure t
  :init
  (vertico-mode)
  :custom
  (consult-preview-key nil)
  :bind (:map vertico-map
         ("C-t" . 'vertico-previous)
         ("C-h" . 'vertico-next)))

(use-package ivy :ensure t
  :bind (:map ivy-mode-map
         ("C-t" . 'ivy-previous-line)
         ("C-h" . 'ivy-next-line)))

(use-package consult :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
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
   :preview-key (kbd "M-."))

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
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package consult-ag :ensure t
  :after (consult)
  :bind (("C-M-f" . consult-ag)))

;; Optionally use the `orderless' completion style.
(use-package orderless :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
