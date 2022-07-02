;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kazunari Tanaka"
      user-mail-address "tzskp1@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
;;
(setq doom-font (font-spec :family "Source Han Code JP N" :height 140)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
; (setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


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

(map! :leader
      :desc "" "SPC" 'consult-buffer)

;;# evil
(eval
`(use-package! evil
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

(use-package! evil-collection
  :after (evil)
  :init (evil-collection-init))

(use-package consult-ag
  :after (consult)
  :bind (("C-M-f" . consult-ag)))
  ;; :init
  ;; (setq -ag-base-command "ag --nocolor --nogrou"))



(use-package! dired
  :commands (dired-mode)
  :after (evil evil-collection)
  :config
  (evil-make-overriding-map dired-mode-map 'normal)
  (evil-define-key 'normal dired-mode-map
    "t" 'dired-previous-line
    "h" 'dired-next-line
    "d" 'dired-up-directory
    "n" 'dired-find-alternate-file))

(use-package! ibuffer
  :commands (ibuffer-mode)
  :after (evil evil-collection)
  :config
  (evil-make-overriding-map ibuffer-mode-map 'normal)
  (evil-define-key 'normal ibuffer-mode-map
    "t" 'evil-previous-visual-line
    "h" 'evil-next-visual-line
    "d" 'evil-backward-char
    "n" 'evil-forward-char))

(use-package! arc-mode
  :bind (:map archive-mode-map
         ("t" . 'archive-previous-line)
         ("h" . 'archive-next-line))
  :commands (archive-mode)
  :after (evil))

(use-package! tar-mode
  :bind (:map tar-mode-map
         ("t" . 'tar-previous-line)
         ("h" . 'tar-next-line))
  :commands (tar-mode)
  :after (evil))

;;# completion
(use-package! company
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
(use-package! ddskk
  :after (evil)
  :bind (("C-x C-j" . skk-mode)
         :map evil-insert-state-map
         ("C-j" . skk-kakutei))
  :init
  (setq skk-kakutei-when-unique-candidate t)
  (setq skk-egg-like-newline t)
  (setq skk-kuten-touten-alist
        '((jp . ("." . "," ))
          (en . ("." . ","))))
  (setq-default skk-kutouten-type 'en)
  (setq skk-user-directory "~/skk"))

(use-package! vertico
  :custom
  (consult-preview-key nil)
  :bind (:map vertico-map
         ("C-t" . 'vertico-previous)
         ("C-h" . 'vertico-next)))

(use-package! ivy
  :bind (:map ivy-mode-map
         ("C-t" . 'ivy-previous-line)
         ("C-h" . 'ivy-next-line)))

(use-package! helm-config
  :bind (:map helm-moccur-map
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

;;# git
(use-package! magit
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

(use-package! lsp-pyright
  :after python
  :init
  (defun lsp-pyright-setup-when-pipenv ()
    (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
    (lsp-restart-workspace))
  :hook
  (python-mode-hook . lsp))

(use-package! popup
  :bind (:map popup-menu-keymap
         ("C-h" . popup-next)
         ("C-t" . popup-previous)))
