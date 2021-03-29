;; -*- lexical-binding: t -*-

;; package requirement
;; silversearcher-ag
;; opam

;; killing custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; (setq package-check-signature nil)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; コンパイル時にパッケージをインストールする.
(eval-when-compile
  (package-initialize)
  (setq package-archives
        '(
        ;; ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
          ("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (unless package-archive-contents (package-refresh-contents))
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (package-install-selected-packages))

(package-initialize)

(require 'use-package)

(use-package jsonrpc :ensure t)

(setq gc-cons-threshold 100000000)
(setq initial-major-mode 'lisp-interaction-mode)
(setq inhibit-startup-screen t)
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
;;# クリップボード
(setq select-enable-primary t)
;;# display EOF
(setq-default indicate-empty-lines t)
;;# Edit
(show-paren-mode 1)
(which-function-mode 1)
(global-auto-revert-mode 1)
;;# Scroll
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;for exec in shell
;;# fold always
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq ring-bell-function 'ignore) ; No Beeps
;; like "mkdir -p"
(add-hook 'find-file-not-found-hooks
          '(lambda () (make-directory (file-name-directory buffer-file-name) t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; 自動分割を抑制
(setq split-height-threshold nil)
(setq split-width-threshold nil)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq require-final-newline 'visit)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(prefer-coding-system 'utf-8-unix)

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
            ("<C-iso-lefttab>" . (lambda () (interactive) (other-window -1)))
            :filter window-system ;; for tiling window manager
            :filter (equal system-type 'gnu/linux)
            ("C-x 3" . make-frame-command)
            ("C-x 2" . make-frame-command)
            :map isearch-mode-map
            ("C-b" . isearch-delete-char)
            ("C-m" . ret)
            :map minibuffer-local-map
            ("C-d" . left-char)
            ("C-n" . right-char)
            ("C-b" . backward-delete-char-untabify)
            ("C-h" . next-line-or-history-element)
            ("C-t" . previous-line-or-history-element))

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

;;# evil
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

;;# helm
(use-package helm :ensure t)
(use-package helm-config
  :bind (("M-x" . helm-M-x)
         :map helm-moccur-map
         ("C-h" . helm-next-line)
         ("C-t" . helm-previous-line)
         :map helm-map
         ("C-d" . helm-buffer-run-kill-persistent)
         ("C-n" . helm-execute-persistent-action)
         ("C-t" . helm-previous-line)
         ("C-h" . helm-next-line))
  :custom
  (helm-ff-auto-update-initial-value nil)
  (helm-input-idle-delay 0.2)
  (helm-candidate-number-limit 50)
  (helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\magit*." "\\`\\*magit*." "\\`\\*Ediff*."))
  :config
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil)))

(use-package evil-leader :ensure t
  :after (evil helm)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "q" 'kill-this-buffer
    "w" 'save-buffer
    "<SPC>" 'helm-mini)
  (kill-buffer (messages-buffer)))

(use-package helm-ag :ensure t
  :after (helm)
  :bind (("C-M-f" . helm-ag))
  :init
  (setq helm-ag-base-command "ag --nocolor --nogrou"))

(use-package yasnippet :ensure t :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets"))

;;# Code Jump
(use-package helm-xref :ensure t
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package dumb-jump :ensure t
  :custom
  (dumb-jump-default-project (expand-file-name "~"))
  (dumb-jump-selector 'helm)
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
  :after (evil-collection)
  :config
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-define-key 'normal dired-mode-map
    "t" 'dired-previous-line
    "h" 'dired-next-line
    "d" 'dired-up-directory
    "n" 'dired-find-alternate-file))

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
  :after (evil)
  :bind (("C-c C-g" . magit-status)
         :map magit-mode-map
         ("t" . evil-previous-visual-line)
         ("h" . evil-next-visual-line)
         ("T" . magit-section-backward-sibling)
         ("H" . magit-section-forward-sibling))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package evil-collection
  :ensure t
  :after (evil)
  :init (evil-collection-init))

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
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)

(use-package shell
  :config
  (setenv "EMACS" "1"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;# Theme
(use-package doom-themes :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

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
  :after (tuareg company)
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
