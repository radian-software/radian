;;; radian-config.el --- Support for editing various config files

(require 'radian-package)

;; Provides syntax highlighting, indentation, and editing commands for
;; YAML files.
(use-package yaml-mode
  :defer-install t
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'"
  :config

  ;; Don't automatically wrap text when editing YAML files.

  (defun radian--disable-auto-fill-mode ()
    (auto-fill-mode -1))

  (add-hook 'yaml-mode-hook #'radian--disable-auto-fill-mode))

;; Editing for TOML files.
(use-package toml-mode
  :defer-install t
  :mode "\\.toml\\'"
  :config

  ;; Show `toml-mode' as "TOML" instead of "Toml" in the mode line.

  (defun radian--rename-toml-mode ()
    (setq mode-name "TOML"))

  (add-hook 'toml-mode-hook #'radian--rename-toml-mode))

;; Edit .gitignore files.
(use-package gitignore-mode
  :defer-install t
  :mode (("/git/ignore\\'" . gitignore-mode)
         ("/info/exclude\\'" . gitignore-mode)
         ("/\\.gitignore\\'" . gitignore-mode)))

;; Provides syntax highlighting for VimScript files.
(use-package vimrc-mode
  :defer-install t
  :mode (("\\.exrc\\'" . vimrc-mode)
         ("[._]?g?vimrc\\'" . vimrc-mode)
         ("\\.vim\\'" . vimrc-mode))
  :config

  ;; Indent by two spaces in `vimrc-mode' rather than eight spaces.
  ;; Based on [1].
  ;;
  ;; [1]: http://stackoverflow.com/a/1819405/3538165

  (defun radian--fix-vimrc-indentation ()
    (setq-local tab-width 2)
    (setq-local indent-line-function 'insert-tab))

  (add-hook 'vimrc-mode-hook #'radian--fix-vimrc-indentation))

;; Edit .htaccess and friends.
(use-package apache-mode
  :defer-install t
  :mode (("\\.htaccess\\'"   . apache-mode)
         ("httpd\\.conf\\'"  . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(provide 'radian-config)

;;; radian-config.el ends here
