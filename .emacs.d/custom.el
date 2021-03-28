(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 4)
 '(company-selection-wrap-around t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(dumb-jump-default-project "/home/tk")
 '(dumb-jump-force-searcher 'ag)
 '(dumb-jump-selector 'helm)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(evil-ex-substitute-case 'smart)
 '(evil-search-module 'evil-search)
 '(evil-shift-width 2)
 '(evil-want-abbrev-expand-on-insert-exit nil)
 '(helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\magit*." "\\`\\*magit*." "\\`\\*Ediff*."))
 '(helm-candidate-number-limit 50)
 '(helm-ff-auto-update-initial-value nil)
 '(helm-input-idle-delay 0.2)
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(package-selected-packages
   '(which-key dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode rustic csharp-mode typescript-mode tide editorconfig smart-jump yatex rust-mode tuareg lean-mode reason-mode ddskk company-coq ein fsharp-mode ess julia-mode dockerfile-mode docker proof-general flycheck-mypy jedi pipenv ensime haskell-mode nord-theme evil-magit magit rainbow-delimiters company-flx company dumb-jump helm-xref yasnippet helm-ag evil-leader helm evil-numbers evil undohist recentf-ext diminish use-package))
 '(recentf-exclude
   '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/elpa"))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "/home/tk/.bak/emacs/recentf")
 '(savehist-file "/home/tk/.bak/emacs/history")
 '(smart-jump-jump-key "C-M-g")
 '(undohist-directory "/home/tk/.bak/emacs/undohist")
 '(undohist-ignored-files '("^/tmp" "COMMIT_EDITMSG" "EDITMSG" "/elpa")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#D8DEE9" :background "#2E3440"))))
 '(doom-modeline-bar ((t (:background "#6272a4")))))
