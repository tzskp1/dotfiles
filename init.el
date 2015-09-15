;; package requirement
;; llvm
;; libclang-dev
;; silversearcher-ag
;; mercurial
;; git
;; ddskk
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;# install package
(el-get-bundle elpa:edit-server)
(el-get-bundle rainbow-delimiters)
(el-get-bundle flymake-cursor)
(el-get-bundle evil)
(el-get-bundle evil-numbers)
(el-get-bundle tarao/evil-plugins)
(el-get-bundle color-moccur)
(el-get-bundle helm)
(el-get-bundle emacs-helm/helm-descbinds)
(el-get-bundle helm-ag)
(el-get-bundle popwin)
(el-get-bundle auto-complete)
(el-get-bundle yatex)
(el-get-bundle ddskk)
(el-get-bundle markdown-mode)
(el-get-bundle jedi)
(el-get-bundle haskell-mode)
(el-get-bundle magit)
(el-get-bundle yasnippet)
(el-get-bundle dropdown-list)
(el-get-bundle git-gutter)
(el-get-bundle yaml-mode)
(el-get-bundle rainbow-mode)
(el-get-bundle flycheck)
(el-get-bundle popup)
(el-get-bundle eldoc-extension)
(el-get-bundle auto-complete-c-headers)
(el-get-bundle Golevka/emacs-clang-complete-async)

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
(setq auto-save-default t)
(global-font-lock-mode t)
(tool-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(custom-set-variables
 '(split-width-threshold 70)
)

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
(setq backup-directory "~/.bak")
(if (and (boundp 'backup-directory)
         (not (fboundp 'make-backup-file-name-original)))
    (progn
      (fset 'make-backup-file-name-original
            (symbol-function 'make-backup-file-name))
      (defun make-backup-file-name (filename)
        (if (and (file-exists-p (expand-file-name backup-directory))
                 (file-directory-p (expand-file-name backup-directory)))
            (concat (expand-file-name backup-directory) 
                    "/" (file-name-nondirectory filename))
          (make-backup-file-name-original filename)))))

;;# Arduino mode
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

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
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

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
     (evil-define-key nil my-keyjack-mode-map (kbd "C-/") 'evil-normal-state)
     (easy-mmode-define-minor-mode my-keyjack-mode "Grab keys"
                                   t " Keyjack" my-keyjack-mode-map)
     (my-keyjack-mode t)))

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
(custom-set-variables
 '(popwin:popup-window-position 'bottom)
 '(popwin:popup-window-height 15)
 '(popwin:adjust-other-windows nil)
 '(display-buffer-function 'popwin:display-buffer)
 '(anything-samewindow nil))
(push '("^\\*helm" :regexp t :width 60 :position :right) popwin:special-display-config)
(push '("*Help*" :width 80 :position :right :noselect t :stick t) popwin:special-display-config)
(define-key global-map [(super o)] 'dired-jump-other-window)
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
;; Completions
(push '("*Completions*") popwin:special-display-config)
;; ;; magit
;; (push '(magit-status-mode) popwin:special-display-config)

;;# dired
(load "dired-x")
(require 'wdired)
(define-key dired-mode-map (kbd "C-M-r") 'wdired-change-to-wdired-mode)

;;# tramp
(setq tramp-auto-save-directory "~/bak/emacs")
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

;;# flymake
(require 'flymake)
(load-library "flymake-cursor")

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

; 'L' for search next
(define-key evil-normal-state-map "N" 'evil-search-next)
(define-key evil-normal-state-map "l" 'evil-search-next)
(define-key evil-normal-state-map "L" 'evil-search-previous)

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
       ";" (lookup-key dired-mode-map ":")))) ; ":d", ":v", ":s", ":e"

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
(evil-define-key nil helm-buffer-map "\C-t" 'helm-previous-line)
(evil-define-key nil helm-buffer-map "\C-h" 'helm-next-line)
(evil-define-key nil helm-moccur-map "\C-h" 'helm-next-line)
(evil-define-key nil helm-moccur-map "\C-t" 'helm-previous-line)
(evil-define-key nil helm-command-map "\C-t" 'helm-previous-line)
(evil-define-key nil helm-command-map "\C-h" 'helm-next-line)
(evil-define-key nil helm-map "\C-t" 'helm-previous-line)
(evil-define-key nil helm-map "\C-h" 'helm-next-line)
(global-set-key (kbd "C-x C-b") 'helm-mini)
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
(custom-set-variables '(helm-ff-auto-update-initial-value nil))
;; C-hでバックスペースと同じように文字を削除  
(define-key helm-c-read-file-map (kbd "C-b") 'delete-backward-char)
;; TABで任意補完。選択肢が出てきたらC-nやC-pで上下移動してから決定することも可能
(define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
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

;(setq yas/trigger-key (kbd "C-c m"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/el-get/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets")

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))
(yas/global-mode 1)


;; from http://d.hatena.ne.jp/antipop/20080321/1206090430
;; [2008-03-17]
;; yasnippet展開中はflymakeを無効にする
(defvar flymake-is-active-flag nil)

(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))

(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on)
               (setq flymake-is-active-flag nil))))

;;# auto-complete
(require 'auto-complete-config nil 'noerror)
(eval-after-load "auto-complete-config"
  (progn
    (add-to-list 'ac-dictionary-directories
                 (concat user-emacs-directory "ac-dict"))
    (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")))
(ac-config-default)
(ac-flyspell-workaround)
(global-auto-complete-mode t)
;; 追加メジャーモード
(add-to-list 'ac-modes 'c++-mode-hook)
(add-to-list 'ac-modes 'c-mode-hook)
(add-to-list 'ac-modes 'sws-mode)
(add-to-list 'ac-modes 'markdown-mode)
(add-to-list 'ac-modes 'makefile-mode)
(add-to-list 'ac-modes 'yatex-mode)
(setq ac-source-yasnippet nil)
(setq ac-auto-start 4)                         ; 4 文字以上で起動
(setq ac-auto-show-menu 0.3)                   ; 0.8秒でメニュー表示
(setq ac-use-comphist t)                       ; 補完候補をソート
(setq ac-candidate-limit nil)                  ; 補完候補表示を無制限に
;; (setq ac-use-quick-help nil)                   ; tool tip 無し
(setq ac-use-menu-map t)                       ; キーバインド
(define-key ac-menu-map (kbd "C-h")         'ac-next)
(define-key ac-menu-map (kbd "C-t")         'ac-previous)
;; (define-key ac-completing-map (kbd "<tab>") 'nil)
(define-key ac-completing-map (kbd "<tab>") 'ac-complete)
(define-key ac-completing-map (kbd "M-/")   'ac-stop)
;; (define-key ac-completing-map (kbd "RET") nil) ; return での補完禁止
(setf (symbol-function 'yas-active-keys)
      (lambda ()
        (remove-duplicates
         (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))

;;# YaTeX
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq dvi2-command "evince")
(setq tex-pdfview-command "evince")
(setq dviprint-command-format "xdg-open `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")

(require 'dbus)
(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(defun okular-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "okular"
      nil
      "okular"
      "--unique"
      (concat (expand-file-name
               (concat (file-name-sans-extension (or YaTeX-parent-file
                                                     (save-excursion
                                                       (YaTeX-visit-main t)
                                                       buffer-file-name)))
                       ".pdf"))
              "#src:"
              (number-to-string (save-restriction
                                  (widen)
                                  (count-lines (point-min) (point))))
              (buffer-file-name))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c o") 'okular-forward-search)))

(defun zathura-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "zathura"
      nil
      "zathura"
      "--synctex-forward"
      (concat (number-to-string (save-restriction
                                  (widen)
                                  (count-lines (point-min) (point))))
              ":0:"
              (buffer-name))
      (expand-file-name
       (concat (file-name-sans-extension (or YaTeX-parent-file
                                             (save-excursion
                                               (YaTeX-visit-main t)
                                               buffer-file-name)))
               ".pdf"))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c z") 'zathura-forward-search)))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;;# RefTeX with YaTeX
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;# skk
(require 'skk)
(when (require 'dired-x nil t)
  (global-set-key "\C-x\C-j" 'skk-mode))
; (setq skk-kakutei-key "C-m")
;; 変換候補がひとつしかない場合は確定する
(setq skk-kakutei-when-unique-candidate t)
;;モードで RET を入力したときに確定のみ行い、改行はしない
(setq skk-egg-like-newline t)
(setq skk-kuten-touten-alist
  '(
    (jp . ("." . "," ))
    (en . ("." . ","))
    ))
(setq-default skk-kutouten-type 'en)

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
(require 'cc-mode)
;; c-mode-common-hook は C/C++ の設定
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "bsd") ;; bsd style
            (setq indent-tabs-mode nil)  ;; タブは利用しない
            (setq c-basic-offset 2)      ;; indent は 2 スペース
            ))
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))
;; Makefile が無くてもC/C++のチェック
(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
(push '("\\.[cC]\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.hpp\\'" flymake-master-make-header-init flymake-master-cleanup)
      flymake-allowed-file-name-masks)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (flymake-mode t)))
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "C-c C-c") 'nil)))
;;# auto-complete-c-headers
(require 'auto-complete-c-headers)
(add-hook 'c++-mode-hook '(setq ac-sources (append ac-sources '(ac-source-c-headers))))
(add-hook 'c-mode-hook '(setq ac-sources (append ac-sources '(ac-source-c-headers))))
;;# auto-complete-clang-async
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/el-get/emacs-clang-complete-async/clang-complete")
  (setq ac-sources (append ac-sources '(ac-source-clang-async)))
  (ac-clang-launch-completion-process))
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)
(add-hook 'c-mode-common-hook
		  '(lambda ()
			 ;; ac-omni-completion-sources is made buffer local so
			 ;; you need to add it to a mode hook to activate on 
			 ;; whatever buffer you want to use it with.  This
			 ;; example uses C mode (as you probably surmised).

			 ;; auto-complete.el expects ac-omni-completion-sources to be
			 ;; a list of cons cells where each cell's car is a regex
			 ;; that describes the syntactical bits you want AutoComplete
			 ;; to be aware of. The cdr of each cell is the source that will
			 ;; supply the completion data.  The following tells autocomplete
			 ;; to begin completion when you type in a . or a ->

			 (add-to-list 'ac-omni-completion-sources
						  (cons "\\." '(ac-source-semantic)))
			 (add-to-list 'ac-omni-completion-sources
						  (cons "->" '(ac-source-semantic)))

			 ;; ac-sources was also made buffer local in new versions of
			 ;; autocomplete.  In my case, I want AutoComplete to use 
			 ;; semantic and yasnippet (order matters, if reversed snippets
			 ;; will appear before semantic tag completions).

			 (setq ac-sources '(ac-source-semantic ac-source-yasnippet))))

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
;; (add-to-list 'load-path
;;              "~/.emacs.d/elisp/emacs-w3m/share/emacs/site-lisp/w3m")
;; (setq w3m-home-page "http://www.google.co.jp/")
(setq geiser-racket-binary "/home/tk/racket/bin/racket")
(setq geiser-active-implementations '(racket))

;;# python
(require 'auto-complete-config)
(require 'python)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;;(setq jedi:setup-keys t)
;; (define-key python-mode-map (kbd "<tab>") 'jedi:complete)

;;# haskell
(require 'haskell-mode)
(setq haskell-program-name "/usr/bin/ghci")
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

;;# magit
(require 'magit)
;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows)
;;   )

;; (defun magit-quit-session ()
;;   "Restores the previous window configuration and kills the magit buffer"
;;   (interactive)
;;   (kill-buffer)
;;   (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;;;;;;;;;;;;;;;;;;;
;; evil key bindings
;;;;;;;;;;;;;;;;;;;

(evil-set-initial-state 'magit-log-edit-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'magit-commit-mode 'motion)
(evil-set-initial-state 'magit-status-mode 'motion)
(evil-set-initial-state 'magit-log-mode 'motion)
(evil-set-initial-state 'magit-wassup-mode 'motion)
(evil-set-initial-state 'magit-mode 'motion)
(evil-set-initial-state 'git-rebase-mode 'motion)

(evil-define-key 'motion git-rebase-mode-map
  "c" 'git-rebase-pick
  "r" 'git-rebase-reword
  "s" 'git-rebase-squash
  "e" 'git-rebase-edit
  "f" 'git-rebase-fixup
  "y" 'git-rebase-insert
  "d" 'git-rebase-kill-line
  "u" 'git-rebase-undo
  "x" 'git-rebase-exec
  (kbd "<return>") 'git-rebase-show-commit
  "\M-n" 'git-rebase-move-line-down
  "\M-p" 'git-rebase-move-line-up)

(evil-define-key 'motion magit-commit-mode-map
  "\C-c\C-b" 'magit-show-commit-backward
  "\C-c\C-f" 'magit-show-commit-forward)

(evil-define-key 'motion magit-status-mode-map
  "\C-f" 'evil-scroll-page-down
  "\C-b" 'evil-scroll-page-up
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "C" 'magit-add-log
  "I" 'magit-ignore-item-locally
  "S" 'magit-stage-all
  "U" 'magit-unstage-all
  "W" 'magit-toggle-whitespace
  "X" 'magit-reset-working-tree
  "d" 'magit-discard-item
  "i" 'magit-ignore-item
  "s" 'magit-stage-item
  "u" 'magit-unstage-item
  "z" 'magit-key-mode-popup-stashing)

(evil-define-key 'motion magit-log-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "e" 'magit-log-show-more-entries)

(evil-define-key 'motion magit-wazzup-mode-map
  "." 'magit-mark-item
  "=" 'magit-diff-with-mark
  "i" 'magit-ignore-item)

(evil-set-initial-state 'magit-branch-manager-mode 'motion)
(evil-define-key 'motion magit-branch-manager-mode-map
  "a" 'magit-add-remote
  "c" 'magit-rename-item
  "d" 'magit-discard-item
  "o" 'magit-create-branch
  "v" 'magit-show-branches
  "T" 'magit-change-what-branch-tracks)

(evil-define-key 'motion magit-mode-map
  "\M-1" 'magit-show-level-1-all
  "\M-2" 'magit-show-level-2-all
  "\M-3" 'magit-show-level-3-all
  "\M-4" 'magit-show-level-4-all
  "\M-H" 'magit-show-only-files-all
  "\M-S" 'magit-show-level-4-all
  "\M-h" 'magit-show-only-files
  "\M-s" 'magit-show-level-4
  "!" 'magit-key-mode-popup-running
  "$" 'magit-process
  "+" 'magit-diff-larger-hunks
  "-" 'magit-diff-smaller-hunks
  "=" 'magit-diff-default-hunks
  "/" 'evil-search-forward
  ":" 'evil-ex
  ";" 'magit-git-command
  "?" 'evil-search-backward
  "<" 'magit-key-mode-popup-stashing
  "A" 'magit-cherry-pick-item
  "B" 'magit-key-mode-popup-bisecting
  "D" 'magit-revert-item
  "E" 'magit-ediff
  "F" 'magit-key-mode-popup-pulling
  "G" 'evil-goto-line
  "H" 'magit-rebase-step
  "J" 'magit-key-mode-popup-apply-mailbox
  "K" 'magit-key-mode-popup-dispatch
  "L" 'magit-add-change-log-entry
  "M" 'magit-key-mode-popup-remoting
  "N" 'evil-search-previous
  "P" 'magit-key-mode-popup-pushing
  "Q" 'magit-quit-session
  "R" 'magit-refresh-all
  "S" 'magit-stage-all
  "U" 'magit-unstage-all
  "W" 'magit-diff-working-tree
  "X" 'magit-reset-working-tree
  "Y" 'magit-interactive-rebase
  "Z" 'magit-key-mode-popup-stashing
  "a" 'magit-apply-item
  "b" 'magit-key-mode-popup-branching
  "c" 'magit-key-mode-popup-committing
  "e" 'magit-diff
  "f" 'magit-key-mode-popup-fetching
  "g?" 'magit-describe-item
  "g$" 'evil-end-of-visual-line
  "g0" 'evil-beginning-of-visual-line
  "gE" 'evil-backward-WORD-end
  "g^" 'evil-first-non-blank-of-visual-line
  "g_" 'evil-last-non-blank
  "gd" 'evil-goto-definition
  "ge" 'evil-backward-word-end
  "gg" 'evil-goto-first-line
  "gh" 'magit-goto-next-section
  "gt" 'magit-goto-previous-section
  ;; "gh" 'evil-next-visual-line
  ;; "gt" 'evil-previous-visual-line
  "gm" 'evil-middle-of-visual-line
  "k" 'magit-key-mode-popup-rewriting
  "h" 'evil-next-visual-line
  "t" 'evil-previous-visual-line
  "l" 'magit-key-mode-popup-logging
  "m" 'magit-key-mode-popup-merging
  "n" 'evil-search-next
  "o" 'magit-key-mode-popup-submodule
  "p" 'magit-cherry
  "q" 'magit-mode-quit-window
  "r" 'magit-refresh
  "j" 'magit-key-mode-popup-tagging
  "v" 'magit-revert-item
  "w" 'magit-wazzup
  "x" 'magit-reset-head
  "y" 'magit-copy-item-as-kill
  ;z  position current line
  " " 'magit-show-item-or-scroll-up
  "\d" 'magit-show-item-or-scroll-down
  "\t" 'magit-visit-item
  (kbd "<return>")   'magit-toggle-section
  (kbd "C-<return>") 'magit-dired-jump
  (kbd "<backtab>")  'magit-expand-collapse-section
  (kbd "C-x 4 a")    'magit-add-change-log-entry-other-window
  (kbd "\M-d") 'magit-copy-item-as-kill)

;; (eval-after-load 'magit-status-mode
;;   (evil-define-key 'normal magit-status-mode-map
;; 	"h" 'magit-goto-next-section
;; 	"t" 'magit-goto-previous-section))
;; (define-key global-map [(super g)] 'magit-status)
;; (add-to-load-path "vendor/magit-plugins")
;; (require 'magit-flow)
;; (add-hook 'magit-mode-hook 'turn-on-magit-flow)

;;# others
(require 'git-gutter)
(require 'yaml-mode)
(require 'rainbow-mode)
(require 'flycheck)

;;# popwin.el
(require 'popwin)
(custom-set-variables
 '(popwin:popup-window-position 'bottom)
 '(popwin:popup-window-height 15)
 '(popwin:adjust-other-windows nil)
 '(display-buffer-function 'popwin:display-buffer)
 '(anything-samewindow nil))
;;(push '("^\\*anything" :regexp t :width 40 :position :left) popwin:special-display-config)
(push '("^\\*helm" :regexp t :width 60 :position :right) popwin:special-display-config)
(push '("^\\*magit" :regexp t :width 60 :position :right) popwin:special-display-config)
(push '("*Help*" :width 80 :position :right :noselect t :stick t) popwin:special-display-config)
;; (push '("*anything imenu*" :width 40 :position :left) popwin:special-display-config)
;; (push '("*Moccur*" :height 50 :position :left) popwin:special-display-config)
(define-key global-map [(super o)] 'dired-jump-other-window)
(push '("*ri*" :width 70 :position :right :noselect t :stick t) popwin:special-display-config)
(push '("*rspec-compilation*" :height 40 :position :bottom) popwin:special-display-config)
;; (push '("^\\*SPEEDBAR" :regexp t :width 40 :position :left :noselect t :stick t) popwin:special-display-config)

;;# eldoc
(require 'eldoc)
(require 'eldoc-extension)
(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
