;; -*- lexical-binding: t -*- 

;; package requirement
;; silversearcher-ag
;; opam

;; killing custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; コンパイル時にパッケージをインストールする.
(eval-when-compile
  (package-initialize)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (unless package-archive-contents (package-refresh-contents))
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (package-install-selected-packages))

(setq gc-cons-threshold 100000000)

(package-initialize)

(require 'use-package)

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
;;# ミニバッファを複数起動
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
;; 自動分割を抑制
(setq split-height-threshold nil)
(setq split-width-threshold nil)
(custom-set-variables
 ;; collecting backups
 '(make-backup-files t)
 '(auto-save-default t)
 '(backup-directory-alist '(("." . "~/.bak/emacs")))
 '(auto-save-file-name-transforms '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(expand-file-name "~/.bak/emacs/autosave") t)))
 '(vc-follow-symlinks t)
 '(auto-revert-check-vc-info t))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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
    (set-face-attribute 'default nil :family "Source Han Code JP N" :height 120)
    (set-frame-font "Source Han Code JP N" nil t)))

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

(use-package tramp
  :config
  (setq tramp-auto-save-directory (expand-file-name "~/.bak/emacs/tramp")))

(use-package undo-tree :diminish ""
  :bind (:map undo-tree-visualizer-mode-map
              ("C-t" . undo-tree-visualize-undo)
              ("C-h" . undo-tree-visualize-redo)))

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
      (insert-char (string-to-char "h")))))

;;# evil
(use-package evil :ensure t
  :init
  (evil-mode 1)
  :custom
  (evil-ex-substitute-case 'smart)
  (evil-want-abbrev-expand-on-insert-exit 'nil)
  (evil-search-module 'evil-search)
  (evil-shift-width 2)
  :bind (:map evil-ex-search-keymap
         ("C-b" . backward-delete-char-untabify)
         :map evil-visual-state-map
         ("h" . evil-next-visual-line)
         ("t" . evil-previous-visual-line)
         ("n" . evil-forward-char)
         ("d" . evil-backward-char)
         ("k" . evil-delete)
         ("K" . evil-delete-line)
         ("M" . evil-ex-search-previous)
         ("N" . evil-ex-search-previous)
         ("m" . evil-ex-search-next)
         ("C-w" . comment-or-uncomment-region)
         :map evil-motion-state-map
         ("h" . evil-next-visual-line)
         ("t" . evil-previous-visual-line)
         ("n" . evil-forward-char)
         ("d" . evil-backward-char)
         ("k" . evil-delete)
         ("K" . evil-delete-line)
         ("M" . evil-ex-search-previous)
         ("N" . evil-ex-search-previous)
         ("m" . evil-ex-search-next)
         ("C-w" . comment-or-uncomment-region)
         :map evil-normal-state-map
         ("h" . evil-next-visual-line)
         ("t" . evil-previous-visual-line)
         ("n" . evil-forward-char)
         ("d" . evil-backward-char)
         ("k" . evil-delete)
         ("K" . evil-delete-line)
         ("M" . evil-ex-search-previous)
         ("N" . evil-ex-search-previous)
         ("m" . evil-ex-search-next)
         ("C-w" . comment-or-uncomment-region)
         :map evil-insert-state-map
         ("h" . hh-normal)
         ("C-d" . backward-char)
         ("C-n" . forward-char)
         ("C-t" . previous-line)
         ("C-b" . backward-delete-char-untabify)
         ("C-h" . next-line)))

(use-package evil-numbers :ensure t
  :after (evil)
  :config
  (bind-key "+" 'evil-numbers/inc-at-pt evil-normal-state-map)
  (bind-key "-" 'evil-numbers/dec-at-pt evil-normal-state-map))

;;# helm
(use-package helm :ensure t)
(use-package helm-config 
  :bind (("M-x" . helm-M-x)
         :map helm-buffer-map
         ("C-t" . helm-previous-line)
         ("C-h" . helm-next-line)
         :map helm-moccur-map
         ("C-h" . helm-next-line)
         ("C-t" . helm-previous-line)
         :map helm-map
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
  :bind (("C-M-g" . dumb-jump-go)
         :map emacs-lisp-mode-map
         ("C-M-g" . xref-find-definitions)))

;;# completion
(use-package auto-complete
  :commands (auto-complete-mode)
  :config
  (bind-keys :map ac-complete-mode-map
             ("C-h" . ac-next)
             ("C-t" . ac-previous))
  (global-auto-complete-mode -1))

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

;;# skk
(use-package ddskk
  :bind (("C-x C-j" . skk-mode))
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
  :after (evil)
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

(use-package magit :ensure t
  :commands (magit-status)
  :after (evil)
  :bind (("C-c C-g" . magit-status)
         :map magit-mode-map
         ("t" . evil-previous-visual-line)
         ("h" . evil-next-visual-line)
         ("T" . magit-section-backward-sibling)
         ("H" . magit-section-forward-sibling))
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (evil-make-overriding-map dired-mode-map 'normal))

(use-package evil-magit :ensure t)

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;# Theme
(use-package nord-theme :ensure t
  :config
  (load-theme 'nord t))
(add-to-list 'default-frame-alist '(alpha . 95))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md" . markdown-mode)
         ("\\.md.erb\\'" . markdown-mode)
         ("\\.howm\\'" . markdown-mode))
  :init
  (setq markdown-command "redcarpet")
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


;; (use-package utop
;;   :after (tuareg)
;;   :commands (utop-minor-mode)
;;   :init 
;;   (add-hook 'tuareg-mode-hook 'utop-minor-mode t))

(use-package fsharp-mode)

(use-package haskell-mode :ensure t)

;;# Coq
;; Open .v files with Proof General's Coq mode
(use-package proof-general :ensure t)

;;# Scala
(use-package ensime :ensure t)

;;# python
(use-package python :ensure t)
(use-package jedi :ensure t
  :after (python)
  :custom
  (jedi:complete-on-dot t)
  :hook
  (python-mode-hook . jedi:setup))
(use-package company-jedi :ensure t
  :after (jedi company)
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package ein
  :after (company-jedi)
  :config
  (add-to-list 'company-backends 'ein:company-backend))
