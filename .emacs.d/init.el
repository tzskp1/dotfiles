;; -*- lexical-binding: t -*- 

;; package requirement
;; llvm
;; llvm-dev
;; libclang-dev
;; silversearcher-ag
;; mercurial
;; git
;; ddskk
;; cmake
;; python-virtualenv
;; rtags

(package-initialize)
(require 'cl-lib)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;# installing packages
(el-get-bundle elpa:edit-server)
(el-get-bundle rainbow-delimiters)
(el-get-bundle evil)
(el-get-bundle evil-numbers)
(el-get-bundle evil-leader)
(el-get-bundle tarao/evil-plugins)
(el-get-bundle color-moccur)
(el-get-bundle async)
(el-get-bundle helm)
(el-get-bundle emacs-helm/helm-descbinds)
(el-get-bundle helm-ag)
(el-get-bundle popwin)
(el-get-bundle yatex)
(el-get-bundle ddskk)
(el-get-bundle emacsfodder/emacs-slime-theme)
(el-get-bundle markdown-mode)
(el-get-bundle jedi-core)
(el-get-bundle jedi)
(el-get-bundle haskell-mode)
(el-get-bundle yasnippet)
(el-get-bundle git-gutter)
(el-get-bundle yaml-mode)
(el-get-bundle rainbow-mode)
(el-get-bundle flycheck)
(el-get-bundle eldoc-extension)
(el-get-bundle undohist)
(el-get-bundle company-mode/company-mode)
(el-get-bundle irony-mode)
(el-get-bundle company-irony)
(el-get-bundle flycheck-irony)
(el-get-bundle company-jedi)
(el-get-bundle helm-gtags)
(el-get-bundle cmake-mode)
(el-get-bundle popup)
(el-get-bundle tuareg)
(el-get-bundle use-package)
(el-get-bundle key-chord)
(el-get-bundle linum-relative)

;;# 行の表示
(use-package linum-relative
  :config
  (setq linum-format "%5d")
  (linum-relative-on)
  (global-linum-mode 1))

(savehist-mode 1)
(setq initial-major-mode 'lisp-interaction-mode)
(setq inhibit-startup-screen t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(global-font-lock-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(setq vc-follow-symlinks t)
(setq auto-revert-check-vc-info t)
(setq gc-cons-threshold 40960000)
(setq frame-title-format
      '("emacs@" system-name ":"
        (:eval (or (buffer-file-name)
                   default-directory))
        ))
;;# ミニバッファを複数起動
(setq enable-recursive-minibuffers t)
;;# 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)
;;# クリップボード
(setq x-select-enable-primary t)
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
;;# Region
(setq transient-mark-mode t) ;highlight region
(setq highlight-nonselected-windows t)
;;# fold always
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq ring-bell-function 'ignore) ; No Beeps
;;# Don't ask
(add-hook 'find-file-not-found-hooks
		  '(lambda () (make-directory (file-name-directory buffer-file-name) t)))
;; 自動分割を抑制
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;;# バックアップファイルを~/.bakに集める
(setq make-backup-files t)
(setq auto-save-default t)
(setq backup-directory-alist
	  (cons (cons ".*" (expand-file-name "~/.bak"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.bak") t)))
;;# tramp
(setq tramp-auto-save-directory "~/.bak/emacs")

;;# font
(set-face-attribute 'default nil
					:height 140)
(if (display-graphic-p)
	(set-fontset-font (frame-parameter nil 'font)
					  'japanese-jisx0208
					  '("ricty" . "unicode-bmp")))

(add-to-list 'default-frame-alist '(alpha . 95))

;;# undohist
(use-package undohist
  :init
  (setq undohist-ignored-files
		'("/tmp/"))
  :config
  (undohist-initialize))

;;# key binding
(bind-key "C-x h" nil) ; delete help
(bind-keys :map isearch-mode-map
		   ("C-b" . isearch-delete-char)
		   ("C-m" . ret))
(bind-keys :map minibuffer-local-map
		   ("C-b" . backward-delete-char-untabify))

;; popwin.el
(require 'popwin)
(push '("^\\*helm" :regexp t :width 60 :position :right) popwin:special-display-config)

;;# evil
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  (setq-default evil-shift-width 2)
  :bind (:map evil-insert-state-map
		("C-d" . backward-char)
		("C-n" . forward-char)
		("C-t" . previous-line)
		("C-b" . backward-delete-char-untabify)
		("C-h" . next-line)
		:map evil-normal-state-map
		("h" . evil-next-visual-line)
		("t" . evil-previous-visual-line)
		("n" . evil-forward-char)
		("d" . evil-backward-char)
		("k" . evil-delete)
		("K" . evil-delete-line)
		("M" . evil-search-previous)
		("N" . evil-search-previous)
		("C-w" . comment-or-uncomment-region)
		("m" . evil-search-next)
		:map evil-motion-state-map
		("h" . evil-next-visual-line)
		("t" . evil-previous-visual-line)
		("n" . evil-forward-char)
		("d" . evil-backward-char)
		("k" . evil-delete)
		("K" . evil-delete-line)
		("M" . evil-search-previous)
		("N" . evil-search-previous)
		("C-w" . comment-or-uncomment-region)
		("m" . evil-search-next)
		:map evil-visual-state-map
		("h" . evil-next-visual-line)
		("t" . evil-previous-visual-line)
		("n" . evil-forward-char)
		("d" . evil-backward-char)
		("k" . evil-delete)
		("K" . evil-delete-line)
		("M" . evil-search-previous)
		("N" . evil-search-previous)
		("C-w" . comment-or-uncomment-region)
		("m" . evil-search-next)
		:map evil-ex-search-keymap
		("C-b" . backward-delete-char-untabify))
  :config
  (evil-mode 1)
  (use-package key-chord
	:custom (key-chord-two-keys-delay 0.01)
	:config
	(key-chord-mode t)
	(key-chord-define evil-insert-state-map "hh" 'evil-normal-state))
  (use-package evil-numbers
	:config
	(bind-key "+" 'evil-numbers/inc-at-pt evil-normal-state-map)
	(bind-key "-" 'evil-numbers/dec-at-pt evil-normal-state-map))
  (use-package evil-leader
	:config
	(global-evil-leader-mode)
	(evil-leader/set-leader "<SPC>")
	(evil-leader/set-key
	  "q" 'kill-this-buffer
	  "w" 'save-buffer
	  "b" 'helm-mini)))

;;# helm
(use-package helm-config
  :bind (("C-x C-b" . helm-mini)
		 ("M-x" . helm-M-x)
		 :map helm-buffer-map
		 ("C-t" . helm-previous-line)
		 ("C-h" . helm-next-line)
		 :map helm-moccur-map
		 ("C-h" . helm-next-line)
		 ("C-t" . helm-previous-line)
		 :map helm-command-map
		 ("C-t" . helm-previous-line)
		 ("C-h" . helm-next-line)
		 :map helm-map
		 ("C-t" . helm-previous-line)
		 ("C-h" . helm-next-line))
  :custom (helm-ff-auto-update-initial-value nil)
  :init
  (setq helm-idle-delay 0.3) 
  (setq helm-input-idle-delay 0.2) 
  (setq helm-candidate-number-limit 50)
  (setq helm-display-function (lambda (buf)
								(split-window-vertically)
								(switch-to-buffer buf)))
  :config
  (helm-descbinds-mode t)
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (use-package helm-ag
	:bind (("M-g ," . helm-ag-pop-stack)
		   ("C-M-s" . helm-ag-this-file)
		   ("C-M-f" . helm-ag))
	:init
	(setq helm-ag-base-command "ag --nocolor --nogrou")))

;;# yasnippet
(use-package yasnippet
  :config
  ;; (setq yas/trigger-key (kbd "C-c m"))
  (yas/initialize)
  (yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
  (yas/load-directory "~/.emacs.d/snippets"))

;;# auto-complete
(use-package auto-complete-config
  :commands (auto-complete-mode)
  :config
  (bind-keys :map ac-complete-mode-map
			 ("C-h" . ac-next)
			 ("C-t" . ac-previous))
  (global-auto-complete-mode -1))

;;# company
(use-package company
  :bind (:map company-active-map
			  ("C-h" . company-select-next)
			  ("C-t" . company-select-previous))
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 4)
  (setq company-selection-wrap-around t)
  :config
  (global-company-mode))

;;# skk
(use-package skk
  :bind (("C-x C-j" . skk-mode))
  :init
  ;; (setq skk-kakutei-key "C-m")
  (setq skk-kakutei-when-unique-candidate t)
  (setq skk-egg-like-newline t)
  (setq skk-kuten-touten-alist
		'(
		  (jp . ("." . "," ))
		  (en . ("." . ","))
		  ))
  (setq-default skk-kutouten-type 'en)
  (setq skk-user-directory "~/skk"))

;;# git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;;# Theme
(use-package slime-theme)

;;# flycheck
(use-package flycheck
  :commands (flycheck-mode)
  :custom
  (flycheck-display-errors-function (lambda (errors)
									  (let ((messages
											 (mapcar
											  (function flycheck-error-message)
											  errors)))
										(popup-tip
										 (mapconcat
										  (quote identity)
										  messages " ")))))
  (flycheck-display-errors-delay 0.5)
  :bind (:map flycheck-mode-map
			  ("C-M-h" . flycheck-next-error)
			  ("C-M-t" . flycheck-previous-error))
  :hook ((c-mode-common-hook . flycheck-mode)
		 (c-mode-common-hook . (lambda () (progn (flycheck-select-checker 'c/c++-clang)
												 (setq flycheck-clang-include-path
													   (list "/usr/local/include/eigen3"))
												 (setq flycheck-clang-args
													   (list "-std=c++11"))))))
  :config
	(when (locate-library "flycheck-irony")
	  (flycheck-irony-setup)))

;;# start server
(use-package server
  :config
  (unless (server-running-p)
	(server-start)))

;;# dired
(use-package dired
  :commands (dired-mode)
  ;; :after (evil)
  :config
  (evil-make-overriding-map dired-mode-map 'normal)
  (defun keu-dired-down-directory ()
	"[Dired command] Go down to the directory."
	(interactive)
	(condition-case err
		(let ((path (dired-get-file-for-visit)))
		  (if (f-directory? path)
			  (dired-find-file)
			(message "This is not directory!")))
	  (error (message "%s" (cadr err)))))
  (evil-define-key 'normal dired-mode-map
	";" (lookup-key evil-motion-state-map ";")
	"t" 'dired-previous-line                    ; 人差し指
	"h" 'dired-next-line                        ; 中指
	"d" 'dired-up-directory                     ; 人差し指の左
	"n" 'keu-dired-down-directory               ; 薬指
	"m" (lookup-key evil-normal-state-map "m")
	"w" (lookup-key evil-normal-state-map "w")
	(kbd "SPC")   (lookup-key dired-mode-map "m")
	(kbd "S-SPC") (lookup-key dired-mode-map "d")))

;;# http://d.hatena.ne.jp/murase_syuka/20140815/1408061850
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (use-package color
	:config
	(cl-loop
	 for index from 1 to rainbow-delimiters-max-face-count
	 do
	 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	   (cl-callf color-saturate-name (face-foreground face) 30)))))

;;# irony
(use-package irony
  :commands (company-irony irony-mode-hook irony-cdb-autosetup-compile-options irony-mode)
  :custom (irony-additional-clang-options '("-std=c++11"))
  :after (company)
  :hook ((irony-mode-hook . irony-cdb-autosetup-compile-options)
		 (c-mode-common-hook . irony-mode))
  :config
  (add-to-list 'company-backends 'company-irony))

;;# gtags
(use-package helm-gtags
  :commands (helm-gtags-mode)
  :after (evil)
  :bind (:map evil-normal-state-local-map
			  ("<return>" . helm-gtags-find-tag)
			  ("RET" . helm-gtags-find-tag))
  :hook ((c-mode-common-hook . helm-gtags-mode)
		 (helm-gtags-mode-hook . (lambda ()
								   (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
								   (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
								   (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
								   (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))))

;;# markdown
(use-package markdown-mode
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
  (add-hook 'markdown-mode
			(lambda ()
			  (setq imenu-create-index-function 'outline-imenu-create-index)
			  (auto-fill-mode))))

;;# c
(use-package cc-mode
  :commands (c-mode-common-hook)
  :mode (("\\.c\\'" . c++-mode)
		 ("\\.cpp\\'" . c++-mode)
		 ("\\.cc\\'" . c++-mode)
		 ("\\.h\\'" . c++-mode))
  :hook (c-mode-common-hook . (lambda ()
								(setq c-default-style "bsd")
								(setq indent-tabs-mode nil)
								(setq c-basic-offset 4)))
  :config
  (c-set-offset (quote cpp-macro) 0 nil))

;;# python
(use-package python
  :commands (python-mode-hook)
  :after (company)
  :config
  (use-package jedi-core
	:init
	(setq jedi:server-args
		  '("--sys-path" "/usr/local/lib/python3.4/dist-packages"
			"--sys-path" "/usr/local/lib/python2.7/dist-packages"))
	(setq jedi:complete-on-dot t)
	(setq jedi:use-shortcuts t)
	:config
	(add-hook 'python-mode-hook 'jedi:setup)
	(add-to-list 'company-backends 'company-jedi)))

;;# haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :custom (haskell-mode-hook 'turn-on-haskell-indentation)
  :init
  (setq haskell-program-name "/bin/ghci"))

;;# others
(use-package yaml-mode)
(use-package rainbow-mode)

;;# eldoc
(use-package eldoc-extension
  :hook ((emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook) .turn-on-eldoc-mode)
  :after (company)
  :init
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t))

;;# YaTeX
(use-package yatex-mode
  :mode "\\.tex$"
  :commands (yatex-mode)
  :hook ((skk-mode-hook . (lambda ()
							(if (eq major-mode 'yatex-mode)
								(progn
								  (define-key skk-j-mode-map "\\" 'self-insert-command)
								  (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)
								  ))))
		 (yatex-mode-hook . (lambda ()
							  (auto-fill-mode -2)))
		 (yatex-mode-hook . (lambda ()
							  (reftex-mode 1)
							  (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
							  (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))
  :init
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code 4))

;;# tuareg
(require 'tuareg)
(add-hook 'tuareg-mode-hook 
           (lambda()
             (local-unset-key (kbd "C-c"))))
(add-hook 'tuareg-mode-hook
          (lambda()
            (local-unset-key (kbd "<ESC>"))))
;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(dolist
   (var (car (read-from-string
	       (shell-command-to-string "opam config env --sexp"))))
 (setenv (car var) (cadr var)))
;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))
;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
	      "/../../share/emacs/site-lisp") load-path)
;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;;# CMake
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
		 ("\\.cmake\\'" . cmake-mode)))

;;# Arduino
(use-package arduino-mode
  :config
  (setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist)))
