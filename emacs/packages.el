(defvar package-list
  '(
    yatex
    tuareg
    markdown-mode
    ddskk
    )
  "packages to be installed")

(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
