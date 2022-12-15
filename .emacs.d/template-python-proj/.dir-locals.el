((python-mode . ((eval . (lsp-register-custom-settings
                          '(("pylsp.plugins.pycodestyle.enabled" nil t)
                            ("pylsp.plugins.flake8.enabled" t t)
                            ("pylsp.plugins.flake8.maxLineLength" 88 t)
                            ("pylsp.configurationSources" ["flake8"] t))))
                 (company-minimum-prefix-length . 1)
                 (eval . (set (make-local-variable 'my-project-path)
                              (file-name-directory
                               (let ((d (dir-locals-find-file ".")))
                                 (if (stringp d) d (car d))))))
                 (eval . (lsp-register-client
                          (make-lsp-client :new-connection (lsp-stdio-connection "pylsp")
                                           :activation-fn (lsp-activate-on "python")
                                           :server-id 'template-pylsp-server
                                           :initialized-fn (lambda (workspace)
                                                             (with-lsp-workspace workspace
                                                               (lsp--set-configuration (lsp-configuration-section "pylsp")))))))
                 (eval . (lsp-workspace-folders-add my-project-path))
                 (eval . (lsp-docker-start)))))
