;;;; Appearance

;;; Disable the menu bar, as it doesn't seem very useful...
(menu-bar-mode -1)

;;;; Elisp customization

;;; This is required to have access to some basic data manipulation
;;; functions, like cl-every. Why aren't these available by default?
(require 'cl-lib)

;;;; OSX interop

;;; Add mouse support
;;; Based on http://stackoverflow.com/a/8859057/3538165
(unless (display-graphic-p)
  (xterm-mouse-mode t)
  ;; Enable scrolling.
  (global-set-key [mouse-4]
		  (lambda ()
		    (interactive)
		    (scroll-down 1)))
  (global-set-key [mouse-5]
		  (lambda ()
		    (interactive)
		    (scroll-up 1))))

;;; Add clipboard support
;;; Based on https://gist.github.com/the-kenny/267162

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;; Packages
;; Downloads any packages that are not included with Emacs 24 by default.
;; This allows radon-emacs to run on other systems without any additional
;; setup (other than Emacs 24 being installed).
;;;;

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
  '(projectile)
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

;;;; Package: IDO
;; Makes completion more intelligent (for instance, when running C-x b
;; or C-x C-f) by using fuzzy matching and better keybindings.
;;;;

;;; Turn on IDO mode everywhere.
(ido-mode 1)
(ido-everywhere 1)

;;; Use fuzzy matching.
(setq ido-enable-flex-matching 1)

;;;; Package: Projectile
;; Enables quickly jumping to any file in a project by filename, or
;; jumping to files in previously visited projects.
;;
;; http://projectile.readthedocs.io/en/latest/
;;;;

;;; Enable Projectile everywhere.
(projectile-global-mode 1)
