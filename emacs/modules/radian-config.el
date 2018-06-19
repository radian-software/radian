;; -*- lexical-binding: t -*-

(require 'radian-regexp)

;; Edit .htaccess and friends.
(use-package apache-mode)

;; Edit Dockerfiles.
(use-package dockerfile-mode)

;; Edit .gitconfig and .gitmodules files.
(use-package gitconfig-mode)

;; Edit .gitignore files.
(use-package gitignore-mode)

;; Edit pip's requirements.txt files.
(use-package pip-requirements
  :config

  (defun radian--rename-pip-requirements-mode-lighter ()
    "Change the lighter for `pip-requirements-mode'."
    (setq mode-name "Requirements"))

  (add-hook 'pip-requirements-mode-hook #'radian--rename-pip-requirements-mode-lighter))

;; Package `ssh-config-mode' provides syntax highlighting and
;; indentation for files in ~/.ssh.
(use-package ssh-config-mode)

;; Edit Terraform configuration files.
(use-package terraform-mode)

;; Editing for TOML files.
(use-package toml-mode
  :config

  ;; Show `toml-mode' as "TOML" instead of "Toml" in the mode line.

  (defun radian--rename-toml-mode ()
    (setq mode-name "TOML"))

  (add-hook 'toml-mode-hook #'radian--rename-toml-mode))

;; Provides syntax highlighting for VimScript files.
(use-package vimrc-mode
  :config

  ;; Indent by two spaces in `vimrc-mode' rather than eight spaces.
  ;; Based on [1].
  ;;
  ;; [1]: http://stackoverflow.com/a/1819405/3538165

  (defun radian--fix-vimrc-indentation ()
    (setq-local tab-width 2)
    (setq-local indent-line-function 'insert-tab))

  (add-hook 'vimrc-mode-hook #'radian--fix-vimrc-indentation))

;; Provides syntax highlighting, indentation, and editing commands for
;; YAML files.
(use-package yaml-mode
  :config

  ;; Don't automatically wrap text when editing YAML files.

  (defun radian--disable-auto-fill-mode ()
    (auto-fill-mode -1))

  (add-hook 'yaml-mode-hook #'radian--disable-auto-fill-mode))

(provide 'radian-config)
