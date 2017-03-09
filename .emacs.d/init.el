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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(el-get-bundle tarao/evil-plugins)
(el-get-bundle color-moccur)
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
;(el-get-bundle rtags)
(el-get-bundle helm-gtags)
(el-get-bundle cmake-mode)
(el-get-bundle popup)

;;# Theme
(require 'slime-theme)
(add-to-list 'default-frame-alist '(alpha . 95))

;;# ミニバッファを複数起動
(setq enable-recursive-minibuffers t)

;;# 環境変数をひきつぐ
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;;# 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;# クリップボード
(setq x-select-enable-primary t)

;;# start server
(require 'server)
(unless (server-running-p)
  (server-start))

;;# 行の表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anything-samewindow nil)
 '(display-buffer-function (quote popwin:display-buffer))
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (lambda
	 (errors)
	 (let
		 ((messages
		   (mapcar
			(function flycheck-error-message)
			errors)))
	   (popup-tip
		(mapconcat
		 (quote identity)
		 messages "
")))))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(helm-ff-auto-update-initial-value nil)
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages (quote (rainbow-mode edit-server)))
 '(popwin:adjust-other-windows nil)
 '(popwin:popup-window-height 15)
 '(popwin:popup-window-position (quote bottom))
 '(split-width-threshold 70))

(setq vc-follow-symlinks t)
(setq auto-revert-check-vc-info t)
;; Avoid re-building of display buffer
(setq gc-cons-threshold 40960000)        ; 40M(default: 400K)
;; todo
;;(setq-default tab-width 4 indent-tabs-mode nil)
(setq frame-title-format
      '("emacs@" system-name ":"
        (:eval (or (buffer-file-name)
                   default-directory))
        ))

;;# display EOF
(setq-default indicate-empty-lines t)

;;# Edit
(show-paren-mode 1)
(which-function-mode 1)
(global-auto-revert-mode 1) ;auto-reload the changed file

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

;;# Beeps
(setq ring-bell-function 'ignore) ; No Beeps

;;# Don't ask
(add-hook 'find-file-not-found-hooks
        '(lambda () (make-directory (file-name-directory buffer-file-name) t)))

;;# バックアップファイルを~/.bakに集める
(setq make-backup-files t)
(setq auto-save-default t)
(setq backup-directory-alist
	  (cons (cons ".*" (expand-file-name "~/.bak"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.bak") t)))

;;# Arduino mode
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;;# dired
(load "dired-x")
(autoload 'wdired "wdired" nil t)
(define-key dired-mode-map "p" 'wdired-change-to-wdired-mode)

;;# key binding
(defun interrupt-and-recompile ()
  "Interrupt old compilation, if any, and recompile."
  (interactive)
  (ignore-errors 
    (process-kill-without-query 
      (get-buffer-process
        (get-buffer "*compilation*"))))
  (ignore-errors 
    (kill-buffer "*compilation*"))
  (recompile)
)
(global-set-key (kbd "C-h C-h") 'interrupt-and-recompile)
(global-unset-key (kbd "C-x h")) ; helpうぜえ
;; (global-set-key (kbd "C-M-f") 'find-grep)
(global-set-key (kbd "C-w") 'comment-or-uncomment-region)

;;# url
;;# https://pqrs.org/emacs/doc/keyjack-mode/
(eval-after-load 'evil
  '(progn
     (setq my-keyjack-mode-map (make-sparse-keymap))
     (mapcar (lambda (x)
               (define-key my-keyjack-mode-map (car x) (cdr x))
               (global-set-key (car x) (cdr x)))
             '(
               ("\C-b" . backward-delete-char-untabify)
               ;;("\C-m" . newline-and-indent)
               ;;("\C-z" . undo)
               ;;("\M-u" . evil-ret)
               ("\C-xh" . nil)
               ("\C-xm" . browse-url-at-point)
               ("\C-x." . find-file-at-point)
               ([C-tab] . other-window)
               ;; ("\C-/" . evil-normal-state)
			   ;;("\C-c \C-b \C-b" . kill-other-buffers)
               ))
    ; (evil-define-key nil my-keyjack-mode-map (kbd "C-/") 'evil-normal-state)
     (easy-mmode-define-minor-mode my-keyjack-mode "Grab keys"
                                   t " Keyjack" my-keyjack-mode-map)
     (my-keyjack-mode t)))

(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)


;;# Mode line setup
; based from: http://amitp.blogspot.jp/2011/08/emacs-custom-mode-line.html
(setq-default
 mode-line-position
 '(
   " "
   ;; Position, including warning for 80 columns
   (:propertize "%4l" face mode-line-position-face)
   (:propertize "/" face mode-line-delim-face-1)
   (:eval
    (number-to-string (count-lines (point-min) (point-max))))
   " "
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   " "
   ))
(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   ;; emacsclient [default -- keep?]
   mode-line-client
   mode-line-remote
   ;evil-mode-line-tag
   mode-line-position
   (:eval (count-lines-and-chars))
   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "  ")))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b" face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n"
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   "  "
   (:propertize mode-line-process
                face mode-line-process-face)
   "  "
   (global-mode-string global-mode-string)
   ;; "  "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
   ))
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
(defun count-lines-and-chars ()
  (if mark-active
      (format "[%3d:%4d]"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(set-face-attribute 'mode-line nil
    :foreground "gray80" :background "gray10"
    :inverse-video nil
    :weight 'normal
    :height 120
    :box '(:line-width 2 :color "gray10" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray30"
    :inverse-video nil
    :weight 'extra-light
    :height 120
    :box '(:line-width 2 :color "gray30" :style nil))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-delim-face-1)
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :weight 'extra-light
    :height 110
    :foreground "gray90")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray60"
    :height 100)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-delim-face-1 nil
    :inherit 'mode-line-face
    :foreground "white")

;;# isearch
(define-key isearch-mode-map "\C-b" 'isearch-delete-char)
(define-key isearch-mode-map "\C-m" 'ret)

;; popwin.el
(require 'popwin)

(push '("^\\*helm" :regexp t :width 60 :position :right) popwin:special-display-config)
(push '("*Help*" :width 80 :position :right :noselect t :stick t) popwin:special-display-config)
(push '("*ri*" :width 70 :position :right :noselect t :stick t) popwin:special-display-config)
(push '("*rspec-compilation*" :height 40 :position :bottom) popwin:special-display-config)
;; Apropos
(push '("*slime-apropos*") popwin:special-display-config)
;; Macroexpand
(push '("*slime-macroexpansion*") popwin:special-display-config)
;; Help
(push '("*slime-description*") popwin:special-display-config)
;; Compilation
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;; Cross-reference
(push '("*slime-xref*") popwin:special-display-config)
;; Debugger
(push '(sldb-mode :stick t) popwin:special-display-config)
;; REPL
(push '(slime-repl-mode) popwin:special-display-config)
;; Connections
(push '(slime-connection-list-mode) popwin:special-display-config)
;; Browse-Kill-RIng
(push '("*Kill Ring*") popwin:special-display-config)
;; sdic
(push '("*sdic*") popwin:special-display-config)
;; ;; Completions
;; (push '("*Completions*") popwin:special-display-config)
;; ;; magit
;; (push '(magit-status-mode) popwin:special-display-config)

;; 自動分割を抑制
(setq split-height-threshold nil)
(setq split-width-threshold nil)


;;# tramp
(setq tramp-auto-save-directory "~/.bak/emacs")
;; root所有のファイルを一般ユーザから開くときに自動的にsudoするかどうかを尋ねる。
(defun file-root-p (filename)
 "Return t if file FILENAME created by root."
 (eq 0 (nth 2 (file-attributes filename))))
(defun th-rename-tramp-buffer ()
 (when (file-remote-p (buffer-file-name))
   (rename-buffer
    (format "%s:%s"
            (file-remote-p (buffer-file-name) 'method)
            (buffer-name)))))
(add-hook 'find-file-hook
         'th-rename-tramp-buffer)
(defadvice find-file (around th-find-file activate)
 "Open FILENAME using tramp's sudo method if it's read-only."
 (if (and (file-root-p (ad-get-arg 0))
          (not (file-writable-p (ad-get-arg 0)))
          (y-or-n-p (concat "File "
                            (ad-get-arg 0)
                            " is read-only.  Open it as root? ")))
     (th-find-file-sudo (ad-get-arg 0))
   ad-do-it))
(defun th-find-file-sudo (file)
 "Opens FILE with root privileges."
 (interactive "F")
 (set-buffer (find-file (concat "/sudo::" file))))


;;# flycheck
(when (require 'flycheck nil 'noerror)
  (custom-set-variables
   '(flycheck-display-errors-delay 0.5))
  (define-key flycheck-mode-map (kbd "C-M-h") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-t") 'flycheck-previous-error)
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'c-mode-common-hook 
			(lambda () (progn (flycheck-select-checker 'c/c++-clang)
							  (setq flycheck-clang-include-path
									(list "/usr/local/include/eigen3"))
							  (setq flycheck-clang-args
									(list "-std=c++11"))))))
(eval-after-load "irony"
  '(progn
     (when (locate-library "flycheck-irony")
       (flycheck-irony-setup))))

;;# for chrome
(require 'edit-server)
(edit-server-start)

;;# http://d.hatena.ne.jp/murase_syuka/20140815/1408061850
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;;# evil
(setq evil-search-module 'evil-search)
(require 'evil)
(setq-default evil-shift-width 2)
(evil-mode 1)

; visual line move
(defun evil-swap-key (map key1 key2)
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")

; for dvorak
; use dhtn as hjkl for dvorak keyboard
(dolist (keys-assoc '(("d" . "h") ("h" . "j") ("t" . "k") ("n" . "l")))
  (dolist (state '(normal motion visual))
    (evil-global-set-key state
                         (car keys-assoc)
                         (lookup-key
                          (symbol-value
                           (intern (concat "evil-" (symbol-name state) "-state-map")))
                          (cdr keys-assoc)))))

(evil-global-set-key 'insert "\C-d" 'backward-char)
(evil-global-set-key 'insert "\C-n" 'forward-char)
(evil-global-set-key 'insert "\C-t" 'previous-line)
(evil-global-set-key 'insert "\C-h" 'next-line)
;;(evil-global-set-key 'insert (kbd "TAB") 'indent-for-tab-command)
;;(evil-global-set-key 'insert "\C-i" 'transpose-chars)
(define-key evil-ex-search-keymap "\C-b" 'backward-delete-char-untabify)

(defmacro evil-add-dhtn-bindings (keymap &optional state &rest bindings)
  "Add \"d\", \"h\", \"t\", \"n\" bindings to KEYMAP in STATE.
Add additional BINDINGS if specified. For dvorak keyboard."
  (declare (indent defun))
  `(evil-define-key ,state ,keymap
     "d" (lookup-key evil-motion-state-map "d")
     "h" (lookup-key evil-motion-state-map "h")
     "t" (lookup-key evil-motion-state-map "t")
     "n" (lookup-key evil-motion-state-map "n")
     ":" (lookup-key evil-motion-state-map ":")
     ,@bindings))

; 'k' for deletion
(let ((map '("k" evil-delete)))
  (apply 'define-key evil-normal-state-map map)
  (apply 'define-key evil-visual-state-map map)
  (apply 'define-key evil-motion-state-map map))

; 'Q' for hide buffer
(define-key evil-normal-state-map "Q" 'quit-window)

; 'M' for search next
(define-key evil-normal-state-map "N" 'evil-search-next)
(define-key evil-normal-state-map "m" 'evil-search-next)
(define-key evil-normal-state-map "M" 'evil-search-previous)

;;(evil-ex-search-previous &optional COUNT)
;; Prevent quit command from exit Emacs
(defun my-kill-current-butffer ()
  :repeat nil
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-save-kill-current-butffer ()
  :repeat nil
  (interactive)
  (save-buffer)
  (kill-buffer (current-buffer)))

(evil-ex-define-cmd "q[uit]" 'my-kill-current-butffer)
(evil-ex-define-cmd "wq" 'my-save-kill-current-butffer)

;; dont care shift key
;; (evil-ex-define-cmd "W" 'save-buffer)
;; (evil-ex-define-cmd "Wq" 'my-save-kill-current-butffer)
;; (evil-ex-define-cmd "WQ" 'my-save-kill-current-butffer)

;; plugins
(require 'evil-numbers)
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)

;; (add-to-load-path "vendor/evil-plugins")
;; (require 'surround)
;; (global-surround-mode 1)

(require 'evil-operator-moccur)
(global-evil-operator-moccur-mode 1)

;; integration
;; Dired
(eval-after-load 'dired
  '(progn
	 (evil-add-dhtn-bindings dired-mode-map 'normal
	   "d" 'dired-up-directory
	   "n" 'dired-find-file                   ; "j"
	   ";" (lookup-key dired-mode-map ":"))
	 )) ; ":d", ":v", ":s", ":e"

;; (eval-after-load 'compilation-mode
;;   '(progn
;;      (evil-add-dhtn-bindings dired-mode-map 'normal
;;        "d" 'dired-up-directory
;;        "n" 'dired-find-file                   ; "j"
;;        ";" (lookup-key dired-mode-map ":")))) ; ":d", ":v", ":s", ":e"

;; (defadvice evil-search-forward (before search-C-b-b () activate)
;;   (evil-insert-state t))
;; (defadvice evil-search-forward (after search-C-b-a () activate)
;;   (evil-normal-state t))

;; (eval-after-load 'term
;;   '(progn
;;      ;; (evil-make-overriding-map term-mode-map 'insert t)
;;      (evil-define-key 'insert term-mode-map
;;        (kbd "C-e") 'term-send-raw)))

(eval-after-load 'compile
  '(progn
     (evil-add-dhtn-bindings compilation-mode-map 'normal)))

;(define-key evil-insert-state-key (kbd "C-d")

;;# helm
(setq helm-idle-delay 0.3) ; 候補を作って描写するまでのタイムラグ。デフォルトで 0.3
(setq helm-input-idle-delay 0.2) ; 文字列を入力しから検索するまでのタイムラグ。デフォルトで 0
(setq helm-candidate-number-limit 50)
(require 'helm)
(require 'helm-config)
(require 'helm-buffers)
(require 'helm-files)
(define-key helm-buffer-map "\C-t" 'helm-previous-line)
(define-key helm-buffer-map "\C-h" 'helm-next-line)
(define-key helm-moccur-map "\C-h" 'helm-next-line)
(define-key helm-moccur-map "\C-t" 'helm-previous-line)
(define-key helm-command-map "\C-t" 'helm-previous-line)
(define-key helm-command-map "\C-h" 'helm-next-line)
(define-key helm-map "\C-t" 'helm-previous-line)
(define-key helm-map "\C-h" 'helm-next-line)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
;; (evil-define-key nil helm-moccur-map "\C-h" 'helm-next-line)
;; (evil-define-key nil helm-moccur-map "\C-t" 'helm-previous-line)
;; ;; List files in git repos
;; (defun helm-c-sources-git-project-for (pwd)
;;   (loop for elt in
;;         '(("Modified files" . "--modified")
;;           ("Untracked files" . "--others --exclude-standard")
;;           ("All controlled files in this project" . nil))
;;         for title  = (format "%s (%s)" (car elt) pwd)
;;         for option = (cdr elt)
;;         for cmd    = (format "git ls-files %s" (or option ""))
;;         collect
;;         `((name . ,title)
;;           (init . (lambda ()
;;                     (unless (and (not ,option) (helm-candidate-buffer))
;;                       (with-current-buffer (helm-candidate-buffer 'global)
;;                         (call-process-shell-command ,cmd nil t nil)))))
;;           (candidates-in-buffer)
;;           (type . file))))

;; (defun helm-git-project-topdir ()
;;   (file-name-as-directory
;;    (replace-regexp-in-string
;;     "\n" ""
;;     (shell-command-to-string "git rev-parse --show-toplevel"))))

;; (defun helm-git-project ()
;;   (interactive)
;;   (let ((topdir (helm-git-project-topdir)))
;;     (unless (file-directory-p topdir)
;;       (error "I'm not in Git Repository!!"))
;;     (let* ((default-directory topdir)
;;            (sources (helm-c-sources-git-project-for default-directory)))
;;       (helm-other-buffer sources
;;                          (format "*helm git project in %s*" default-directory)))))

;; (define-key global-map [(super \,)] 'helm-git-project)
(setq helm-display-function (lambda (buf)
                             (split-window-vertically)
                             (switch-to-buffer buf)
                             ))
; 自動補完を無効

;; ;; C-hでバックスペースと同じように文字を削除  
;; (define-key helm-c-read-file-map (kbd "C-b") 'delete-backward-char)
;; ;; TABで任意補完。選択肢が出てきたらC-nやC-pで上下移動してから決定することも可能
;; (define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(helm-descbinds-mode t)
(helm-mode 1)
;; 処理を変更したいコマンドをリストに登録していく
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(require 'helm-ag)
(setq helm-ag-base-command "ag --nocolor --nogrou")
;; (global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "C-M-s") 'helm-ag-this-file)
(global-set-key (kbd "C-M-f") 'helm-ag)

;;# yasnippet
(require 'yasnippet) ;;# not yasnippet-bundle
;; (setq yas/trigger-key (kbd "C-c m"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")

;;# auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(define-key ac-complete-mode-map (kbd "C-h") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-t") 'ac-previous)
(global-auto-complete-mode -1)

;;# company
(require 'company)
(global-company-mode) ; 全バッファで有効にする 
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 4) ; 4 文字以上で起動
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(define-key company-active-map (kbd "C-h") 'company-select-next)
(define-key company-active-map (kbd "C-t") 'company-select-previous)

;;# irony
(eval-after-load "irony"
  '(progn
     (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
     (add-to-list 'company-backends 'company-irony)
     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
     (add-hook 'c-mode-common-hook 'irony-mode)))
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;;# rtags
;; (when (require 'rtags nil 'noerror)
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (rtags-is-indexed)
;; 				(define-key evil-normal-state-local-map (kbd "<return>") 'rtags-find-symbol-at-point)
;; 				(define-key evil-normal-state-local-map (kbd "RET") 'rtags-find-symbol-at-point)
;;                 (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
;;                 (local-set-key (kbd "M-;") 'rtags-find-symbol)
;;                 (local-set-key (kbd "M-@") 'rtags-find-references)
;;                 (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))
;; (custom-set-variables '(rtags-use-helm t))

;;# gtags
(require 'helm-config)
(require 'helm-gtags)

(add-hook 'c-mode-common-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
			 (define-key evil-normal-state-local-map (kbd "<return>") 'helm-gtags-find-tag)
			 (define-key evil-normal-state-local-map (kbd "RET") 'helm-gtags-find-tag)
			 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
			 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
			 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
			 (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))

;;# skk
(require 'skk)
(when (require 'dired-x nil t)
  (global-set-key "\C-x\C-j" 'skk-mode))
; (setq skk-kakutei-key "C-m")
;; 変換候補がひとつしかない場合は確定する
(setq skk-kakutei-when-unique-candidate t)
;;モードで RET を入力したときに確定のみ行い、改行はしない
(setq skk-egg-like-newline t)
;; (setq skk-kuten-touten-alist
;;   '(
;;     (jp . ("." . "," ))
;;     (en . ("." . ","))
;;     ))
;; (setq-default skk-kutouten-type 'en)
(setq skk-user-directory "~/skk")

;;# markdown
(require 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode)
            (cons '("\\.md.erb\\'" . markdown-mode)
            (cons '("\\.howm\\'" . markdown-mode) auto-mode-alist)
            )))
(setq markdown-command "redcarpet")
(defun outline-imenu-create-index ()
  (let (index)
    (goto-char (point-min))
    (while (re-search-forward "^\*\s*\\(.+\\)" (point-max) t)
      (push (cons (match-string 1) (match-beginning 1)) index))
    (nreverse index)))
(add-hook 'markdown-mode
          (lambda ()
            (setq imenu-create-index-function 'outline-imenu-create-index)
            (auto-fill-mode)
            ))
;; (add-hook 'markdown-mode-hook
;;           '(lambda ()
;;              (refill-mode -1)
;;              ))

;;# c
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(c-set-offset (quote cpp-macro) 0 nil)
(require 'cc-mode)
;; c-mode-common-hook は C/C++ の設定
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "bsd") ;; bsd style
            (setq indent-tabs-mode nil)  ;; タブは利用しない
            (setq c-basic-offset 4)      ;; indent は 4 スペース
            ))
;; (eval-after-load "cc-mode"
;;   '(progn
;;      (define-key c-mode-base-map (kbd "C-c C-c") 'nil)))

;;# gauche(scheme)
(put 'downcase-region 'disabled nil)
; settings for load path
;(setq load-path (cons "~/site-lisp/slime" load-path))
(setq inferior-lisp-program "ecl")
;(setq inferior-lisp-program "ccl")
;(require 'slime)
;(slime-setup)
;; Gaucheのデフォルトエンコーディングに合わせます。
;; Gaucheのデフォルトエンコーディングがeuc-jpの時はutf-8をeuc-jpに
;; してください。
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
; goshインタプリタのパスに合わせます。-iは対話モードを意味します。
(setq gosh-program-name "gosh -i")
;; schemeモードとrun-schemeモードにcmuscheme.elを使用します。
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; ウィンドウを2つに分け、一方でgoshインタプリタを実行するコマンドを定義します。
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme gosh-program-name))
; そのコマンドをCtrl-cSで呼び出します。
(define-key global-map
  "\C-cS" 'scheme-other-window)
;; 直前/直後の括弧に対応する括弧を光らせます。
(show-paren-mode)
;; 以下はインデントの定義です。
(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)
(setq geiser-racket-binary "/home/tk/racket/bin/racket")
(setq geiser-active-implementations '(racket))

;;# python
(require 'python)
(require 'jedi-core)
(setq jedi:server-args
      '("--sys-path" "/usr/local/lib/python3.4/dist-packages"
		"--sys-path" "/usr/local/lib/python2.7/dist-packages"))
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi) ; backendに追加

;;# haskell
(require 'haskell-mode)
(setq haskell-program-name "/usr/bin/ghci")

;;# git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)

;;# others
(require 'yaml-mode)
(require 'rainbow-mode)

;;# eldoc
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;# font
(set-face-attribute 'default nil
					;:family "ricty"
					:height 140)
;; (set-default-font "ricty-11:spacing=1")
;; (set-face-font 'variable-pitch "ricty-11:spacing=1")
(if (display-graphic-p)
	(set-fontset-font (frame-parameter nil 'font)
					  'japanese-jisx0208
					  '("ricty" . "unicode-bmp")))
;; (add-to-list 'face-font-rescale-alist
;;              '("ricty" . 1.2))

;;# undohist
(require 'undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("/tmp/"))

;;# YaTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code 4)
; skk 対策
(add-hook 'skk-mode-hook
	  (lambda ()
	    (if (eq major-mode 'yatex-mode)
		(progn
		  (define-key skk-j-mode-map "\\" 'self-insert-command)
		  (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar)
		  ))
	    ))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -2)))

;;# RefTeX with YaTeX
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;# CMake
(require 'cmake-mode)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
