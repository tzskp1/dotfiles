(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-check-vc-info t)
 '(auto-save-default t)
 '(auto-save-file-name-transforms
   (quote
    (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      (\,
       (expand-file-name "~/.bak/emacs/autosave"))
      t))))
 '(backup-directory-alist (quote (("." . "~/.bak/emacs"))))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 4)
 '(company-selection-wrap-around t)
 '(dumb-jump-default-project "/home/tk" t)
 '(dumb-jump-force-searcher (quote ag) t)
 '(dumb-jump-selector (quote helm) t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(evil-ex-substitute-case (quote smart))
 '(evil-search-module (quote evil-search))
 '(evil-shift-width 2)
 '(evil-want-abbrev-expand-on-insert-exit nil)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\magit*." "\\`\\*magit*." "\\`\\*Ediff*.")))
 '(helm-candidate-number-limit 50)
 '(helm-ff-auto-update-initial-value nil)
 '(helm-input-idle-delay 0.2)
 '(key-chord-two-keys-delay 0.05 t)
 '(make-backup-files t)
 '(package-selected-packages
   (quote
    (company-jedi jedi ensime proof-general htmlize fsharp-mode ddskk markdown-mode tuareg yatex haskell-mode madhat2r-theme magit rainbow-delimiters company yasnippet helm-ag evil-leader helm git-gutter linum-relative evil-numbers key-chord evil undohist recentf-ext diminish use-package)))
 '(recentf-exclude
   (quote
    ("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/elpa")))
 '(recentf-max-saved-items 2000)
 '(recentf-save-file "/home/tk/.bak/emacs/recentf")
 '(savehist-file "/home/tk/.bak/emacs/history")
 '(undohist-directory "/home/tk/.bak/emacs/undohist")
 '(undohist-ignored-files (quote ("^/tmp" "COMMIT_EDITMSG" "EDITMSG" "/elpa")))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
