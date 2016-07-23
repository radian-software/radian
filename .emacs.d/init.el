;;; This is required to have access to some basic data manipulation
;;; functions, like cl-every. Why aren't these available by default?
(require 'cl-lib)

;;;; Packages

;;; Based on http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

;;; Initialize the package management system, before we start trying
;;; to add packages.
(require 'package)
(package-initialize)

;;; The default package repository in Emacs doesn't have a lot of the
;;; packages we need, such as Projectile. Therefore, add the MELPA Stable
;;; repository.
(add-to-list 'package-archives
	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	     t) ; this appends to the end of the list

(defvar my-packages
  '(
    ;; Enables quickly jumping to any file in a project by filename, or
    ;; jumping to files in previously visited projects.
    ;;
    ;; http://projectile.readthedocs.io/en/latest/
    projectile)
  "The packages required by radon-emacs.")

;;; Install required packages, if necessary.
(unless (cl-every 'package-installed-p my-packages)
  ;; Make sure to get the latest version of each package.
  (message "%s" "radon-emacs is refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages.
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; Make the installed packages available.
(provide 'my-packages)

;;;; Projectile

;;; Enable Projectile everywhere.
(projectile-global-mode 1)
