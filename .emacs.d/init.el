;;;; Appearance

;;; Disable the "For information about GNU Emacs..." message at startup,
;;; for *all* users.
(defun display-startup-echo-area-message ())

;;; Disable the *About GNU Emacs* buffer at startup, and go straight for
;;; the scratch buffer. This is especially useful because Projectile won't
;;; work in the startup buffer, which is annoying.
(setq inhibit-startup-screen t)

;;; Disable the menu bar, as it doesn't seem very useful...
(menu-bar-mode -1)

;;; When point is on a paren, highlight the matching paren instantly.
(setq show-paren-delay 0)
(show-paren-mode 1)

;;;; Elisp customization

;;; This is required to have access to some basic data manipulation
;;; functions, like cl-every. Why aren't these available by default?
(require 'cl-lib)

;;; Define a keybinding for instantly reloading the configuration in this
;;; file.

(defun reload-initfile ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  "Reloads init.el.")

(global-set-key (kbd "<f12>") 'reload-initfile)

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

;;;; File saving

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

;;; Trim trailing whitespace on save. This will get rid of end-of-line
;;; whitespace, reduce the number of blank lines at the end of the file
;;; to one, and add a trailing newline to the file if one is missing.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
  '(aggressive-indent cider clojure-mode company company-statistics helm helm-projectile paredit projectile)
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

;;; Use Helm mode for Projectile commands. Using helm-projectile-toggle
;;; instead of helm-projectile-on means we don't get a useless "Turn on
;;; helm-projectile key bindings" message in the minibuffer during startup.

;; The local binding of ad-redefinition works around a warning message
;; "ad-handle-definition: `tramp-read-passwd' got redefined, as described at:
;; https://github.com/emacs-helm/helm/issues/1498#issuecomment-218249480

(let ((ad-redefinition-action 'accept))
  (helm-projectile-toggle 1))

;;; Use Helm mode for M-x.
(global-set-key  (kbd "M-x") 'helm-M-x)

;;;; Package: Company
;; Shows autocompletion suggestions in a pop-up menu while typing. Includes
;; interop with CIDER.
;;;;

;;; Turn on Company mode everywhere.
(global-company-mode 1)

;;; Show completions instantly, rather than after half a second.
(setq company-idle-delay 0)

;;; Show completions after typing a single character, rather than after
;;; typing three characters.
(setq company-minimum-prefix-length 1)

;;; Show a maximum of 20 suggestions, rather than 10.
(setq company-tooltip-limit 20)

;;; Always display the entire suggestion list onscreen, placing it above
;;; the cursor if necessary.
(setq company-tooltip-minimum 21)

;;; Always display suggestions in the tooltip, even if there is only one.
(setq company-frontends '(company-pseudo-tooltip-frontend))

;;; Don't prevent non-matching input (which will dismiss the completions
;;; menu), even if the user interacts explicitly with Company.
(setq company-require-match nil)

;;; Prevent suggestions from being triggered automatically. In particular,
;;; this makes it so that:
;;; - TAB will always complete the current selection.
;;; - RET will only complete the current selection if the user has explicitly
;;;   interacted with Company.
;;; - SPC will never complete the current selection.
;;;
;;; Based on https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961

(defun company-complete-if-explicit ()
  "Complete the current selection, but only if the user has interacted
explicitly with Company."
  (interactive)
  (if (company-explicit-action-p)
      (company-complete)
    (call-interactively
     (key-binding (this-command-keys)))))

;; <return> is for windowed Emacs; RET is for terminal Emacs
(define-key company-active-map (kbd "<return>") #'company-complete-if-explicit)
(define-key company-active-map (kbd "RET") #'company-complete-if-explicit)
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map (kbd "SPC") nil)

;; Company appears to override the above keymap based on company-auto-complete-chars.
;; Turning it off ensures we have full control.
(setq company-auto-complete-chars nil)

;;;; Package: Company Statistics
;; Sorts Company completions by usage. Persistent between Emacs sessions.
;;;;

(company-statistics-mode 1)

;;;; Package: ElDoc
;; Automatically shows the signature of the function at point in the echo
;; area. Also works with variables, for which the first line of the docstring
;; is shown.
;;;;

;;; Enable ElDoc when editing Lisps and using Lisp REPLs.
(dolist (hook '(emacs-lisp-mode-hook
		lisp-interaction-mode-hook
		clojure-mode-hook
		cider-repl-mode-hook))
  (add-hook hook (lambda () (eldoc-mode 1))))

;;; Turn off the delay before ElDoc messages are shown in the echo area.
(setq eldoc-idle-delay 0)

;;;; Package: Clojure mode
;; Provides indentation and syntax highlighting for Clojure and
;; ClojureScript files.
;;;;

;;; Customize indentation like this:
;;;
;;; (some-function
;;;   argument
;;;   argument)
;;;
;;; (some-function argument
;;;                argument)
;;;
;;; (-> foo
;;;   thread
;;;   thread)
;;;
;;; (->> foo
;;;   thread
;;;   thread)

(require 'clojure-mode)

(setq clojure-indent-style ':align-arguments)

(define-clojure-indent
  (-> 1)
  (->> 1))

;;;; Package: Paredit
;; Automatically balances parentheses and provides keybindings for structural
;; editing of s-expressions.
;;;;

;;; Enable Paredit when editing Lisps and using Lisp REPLs.
(dolist (hook '(emacs-lisp-mode-hook
		lisp-interaction-mode-hook
		clojure-mode-hook
		cider-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

;;;; Package: Aggressive Indent
;; Automatically, and aggressively, indents your code. Especially useful when
;; combined with Paredit, as you can read code structure off the indentation
;; without checking the parentheses.
;;;;

(global-aggressive-indent-mode 1)

;;;; Appearance - continued

;;; Adding these tweaks at the end prevents them from making Emacs look
;;; weird while it's starting up.

;;; Load a color theme that looks good with Solarized Light.
(load-theme 'leuven t) ; the last argument suppresses a confirmation message

;;; Customize the mode bar to something like:
;;; [*] init.el        38% (149,30)   (Emacs-Lisp Projectile[dotfiles])

(defvar mode-line-modified-radon
  '(:eval (propertize (if (buffer-modified-p)
                          "[*]" "   ")
                      'face '(:foreground "#FDF6E3")))
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
