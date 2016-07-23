;;;; Appearance

;;; Disable the "For information about GNU Emacs..." message at startup,
;;; for *all* users.
(defun display-startup-echo-area-message ())

;;; Disable the menu bar, as it doesn't seem very useful...
(menu-bar-mode -1)

;;; Customize the mode bar to something like:
;;; [*] init.el        38% (149,30)   (Emacs-Lisp Projectile[dotfiles])

(defvar mode-line-modified-radon
  '(:eval (if (buffer-modified-p)
	      "[*]" "   "))
  "Construct for the mode line that shows [*] if the buffer
has been modified, and whitespace otherwise.")

(setq-default mode-line-format
	      (list
	       ;; Show a warning if Emacs is low on memory.
	       "%e"
	       ;; Show [*] if the buffer is modified.
	       mode-line-modified-radon
	       " "
	       ;; Show the name of the current buffer.
	       mode-line-buffer-identification
	       "   "
	       ;; Show the row and column of point.
	       mode-line-position
	       "  "
	       ;; Show the active major and minor modes.
	       mode-line-modes))

(column-number-mode 1) ; makes mode-line-position show the column

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

;;;; Backup and autosave configuration

;;; Put backup files in $HOME/.emacs-backups, rather than in the current
;;; directory.
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;;; Always use copying to make backup files. This prevents links from
;;; being made to point at the backup file rather than the original.
(setq backup-by-copying t)

;;; Keep multiple numbered backup files, rather than a single unnumbered
;;; backup file.
(setq version-control t)

;;; Delete old backups silently, instead of asking for confirmation.
(setq delete-old-versions t)

;;; Don't make autosave files.
(setq auto-save-default nil)

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
  '(helm helm-projectile projectile)
  "The packages required by radon-emacs.")

;;; Install required packages, if necessary.
(unless (cl-every 'package-installed-p my-packages)
  ;; Make sure to get the latest version of each package.
  (package-refresh-contents)
  ;; Install the missing packages.
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; Make the installed packages available.
(provide 'my-packages)

;;;; Package: Windmove
;; Allows switching to adjacent windows using shift + arrow keys.
;;;;

(windmove-default-keybindings)

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

;;;; Package: Helm
;; Shows completions for switching to files and buffers in a separate,
;; easy-to-navigate buffer.
;;;;

;;; Use Helm mode for Projectile commands.
(helm-projectile-on)

;;; Use Helm mode for M-x.
(global-set-key  (kbd "M-x") 'helm-M-x)
