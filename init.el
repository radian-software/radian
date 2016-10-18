;; The general outline of this file is as follows:
;;
;; * Set up variables and functions for managing user-specific
;;   configuration. This needs to be done first because the
;;   user-specific options could affect the behavior of anything else
;;   in the file.
;;
;; * Run the user-specific configuration file init.before.local.el.
;;
;; * Perform Emacs-specific configuration. This is organized roughly
;;   so that more general/basic functionality is customized first.
;;
;; * Set up the package installation system.
;;
;; * Run the user-specific configuration file init.pre.local.el.
;;
;; * Install packages.
;;
;; * Run the user-specific configuration file init.post.local.el.
;;
;; * Perform package-specific configuration. This is organized roughly
;;   so that more general/basic packages are customized first.
;;
;; * Load color schemes and perform other appearance tweaks that look
;;   odd unless they are done at the very end of initialization.
;;
;; * Run the user-specific configuration file init.local.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration

;;; The following are user-specific configuration options that can be
;;; overridden in init.before.local.el. They parsed by
;;; create-init-before-local-el.sh so the user can set them up
;;; interactively.

;; Control color customizations. If you want to use the default
;; Radian color scheme (Solarized Light + Leuven), which is
;; guaranteed to work in terminal Emacs, set this to `t' (default)
;; and make sure to set your terminal emulator to use the Solarized
;; Light color scheme. This will also turn on various tweaks designed
;; to make various parts of the Leuven theme look better. If you want
;; to set up your own color scheme, or use the Emacs default, set
;; this to `nil'.
(setq radian-customize-tweak-colors t)

;;; Now we define the package list. Every package specified in this
;;; list will be automatically installed when Emacs starts, if it is
;;; not already installed. In the future, packages that are not
;;; specified in this list may be automatically uninstalled. Unless
;;; you are requesting that a package be added to or removed from the
;;; official version of Radian, you should use the
;;; `radian-add-package' and `radian-remove-package' functions in your
;;; init.before.local.el to add and remove packages in your local setup.
;;;
;;; This section is parsed by create-init-before-local-el.sh so that
;;; the user can set up their preferred packages interactively.
;;;
;;; Here we are using the defvar-nil-setq pattern described in [1],
;;; which makes it so that changes to this list will be picked up by a
;;; reload of init.el (M-RET r).
;;;
;;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html
(defvar radian-packages nil "The packages required by Radian.")
(setq radian-packages
      '(
        ace-jump-mode ; quickly jump to words, characters, or lines onscreen
        aggressive-indent ; keep code correctly indented at all times
        cider ; Clojure REPL integration
        clojure-mode ; Clojure indentation and syntax highlighting
        company ; autocompletion with pop-up menu
        company-statistics ; sort Company completions by usage
        geiser ; support for Racket
        git-commit ; edit Git commit messages intelligently
        helm ; better interface for selecting files, buffers, or commands
        helm-projectile ; use Helm for Projectile
        helm-smex ; sort M-x suggestions by usage
        markdown-toc ; generate tables of contents for Markdown files
        paredit ; keep parentheses correctly balanced at all times
        projectile ; quickly jump to files organized by project
        transpose-frame ; easily mirror, rotate, and transpose windows
        undo-tree ; more intuitive and powerful undo/redo
        ))

;;; The following are utility functions for use in adding and removing
;;; packages in init.before.local.el.

(defun radian-add-package (package)
  "Adds the provided package from `radian-packages', if it is not
already present. For use in init.before.local.el. Note that this
function will not install the provided package. To do that, call
`radian-update-packages'."
  ;; Passing `t' to `add-to-list' makes the addition happen at the end
  ;; of the list.
  (add-to-list 'radian-packages package t))

(defun radian-remove-package (package)
  "Removes the provided package from `radian-packages', if it is
present. For use in init.before.local.el. Note that this function
will not uninstall the package; it will only prevent the package
from being installed automatically. For now, you have to
uninstall packages manually, by deleting their folders in
~/.emacs.d/elpa."
  (setq radian-packages (delete package radian-packages)))

;;; The following function is used four times in init.el to load
;;; user-specific configuration files (init.before.local.el,
;;; init.pre.local.el, init.post.local.el, and init.local.le).

(defun radian-load-user-config (filename)
  "If a file by the specified name exists in the ~/.emacs.d directory,
loads it. Otherwise, fails silently."
  (let ((file (concat user-emacs-directory filename)))
    (when (file-exists-p file)
      (load-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration (1 of 4)

(radian-load-user-config "init.before.local.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Libraries

;; The cl-lib library provides `cl-every', which is used in the code
;; for determining if any packages need to be installed. The library
;; might also be needed by one of the packages loaded by Radian, but
;; I'm not sure.
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Startup

;; Disable the "For information about GNU Emacs..." message at startup,
;; for *all* users.
(defun display-startup-echo-area-message ())

;; Disable the *About GNU Emacs* buffer at startup, and go straight for
;; the scratch buffer. This is especially useful because Projectile won't
;; work in the startup buffer, which is annoying.
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Appearance

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the toolbar in windowed Emacs.
(tool-bar-mode -1)

;; Turn off the alarm bell.
(setq ring-bell-function 'ignore)

;; Prevent the cursor from blinking in windowed Emacs.
(blink-cursor-mode 0)

;; When point is on a paren, highlight the matching paren, even if it
;; wasn't just typed. Also, do it immediately, instead of after 1/8 of
;; a second. Note that `show-paren-delay' must be changed *before*
;; turning on `show-paren-mode' in order for the change to take
;; effect.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Get rid of the underline for the currently highlighted match in an
;; Isearch or query replace.
(when radian-customize-tweak-colors
  (set-face-underline 'isearch nil))

;; The default highlight color for Isearches is quite dark and makes
;; it hard to read the highlighted text. Change it to a nice light
;; blue, and get rid of the distracting underline.
(when radian-customize-tweak-colors
  (set-face-background 'lazy-highlight "#B1EAFC")
  (set-face-underline 'lazy-highlight nil))

;; Eliminate the underline on mismatched parens.
(set-face-underline 'show-paren-mismatch nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Host OS interop

;;; The following code adds support for mouse and clipboard
;;; interaction to terminal Emacs. Windowed Emacs already has this
;;; support by default.

;;; Mouse
;; Based on http://stackoverflow.com/a/8859057/3538165

(unless (display-graphic-p)

  (xterm-mouse-mode t)

  (global-set-key [mouse-4]
                  (lambda ()
                    (interactive)
                    (scroll-down 1)))

  (global-set-key [mouse-5]
                  (lambda ()
                    (interactive)
                    (scroll-up 1))))

;;; Clipboard
;; Based on https://gist.github.com/the-kenny/267162
;; Modified based on http://emacs.stackexchange.com/q/26471/12534

(unless (display-graphic-p)

  (setq radian-last-paste-to-osx nil)

  (defun copy-from-osx ()
    (let ((copied-text (shell-command-to-string "pbpaste")))
      (unless (string= copied-text radian-last-paste-to-osx)
        copied-text)))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc)))
    (setq radian-last-paste-to-osx text))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; If you have something on the system clipboard, and then kill something in
;; Emacs, then by default whatever you had on the system clipboard is gone
;; and there is no way to get it back. Setting the following option makes it
;; so that when you kill something in Emacs, whatever was previously on the
;; system clipboard is pushed into the kill ring. This way, you can paste it
;; by doing C-y M-y.
(setq save-interprogram-paste-before-kill t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finding files

;; The following code adds keybindings for jumping to the various
;; dotfiles set up by Radian. These all begin with M-RET and are
;; designed to be mnemonic, as in <M-RET e p r> standing for "go to
;; [e]macs init.[pr]e.local.el".
;;
;; Here we are using the defvar-nil-setq pattern described in [1],
;; which makes it so that changes to `radian-dotfiles' will be picked
;; up by a reload of init.el (M-RET r).
;;
;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html

(defvar radian-dotfiles nil
  "Association list from a keybinding suffix to be used after
M-RET to the file opened by the resulting keybinding.")
(setq radian-dotfiles
      '(("e i"   ".emacs.d/init.el")
        ("e b"   ".emacs.d/init.before.local.el")
        ("e p r" ".emacs.d/init.pre.local.el")
        ("e p o" ".emacs.d/init.post.local.el")
        ("e l"   ".emacs.d/init.local.el")
        ("g c"   ".gitconfig")
        ("g e"   ".gitexclude")
        ("g l"   ".gitconfig.local")
        ("l p"   ".lein/profiles.clj")
        ("t c"   ".tmux.conf")
        ("t l"   ".tmux.local.conf")
        ("z r"   ".zshrc")
        ("z a"   ".zshrc.antigen.local")
        ("z b"   ".zshrc.before.local")
        ("z l"   ".zshrc.local")))

(dolist (item radian-dotfiles)
  (global-set-key (kbd (concat "M-RET " (car item)))
                  `(lambda ()
                     ,(concat "Open ~/" (cadr item) " in the current buffer.")
                     (interactive)
                     (find-file ,(concat "~/" (cadr item))))))

;; Use IDO ("interactive do") for C-x C-f and other file-finding
;; operations. There is a known issue (#41) that sometimes causes C-x
;; b (which is not a file-finding operation, and should be using
;; Helm) to switch back to IDO. Unfortunately, a consistently
;; reproducible test case has not been forthcoming.
(ido-mode 'files)

;; Use fuzzy matching for IDO.
(setq ido-enable-flex-matching 1)

;; By default, if you type in a filename that does not exist in the
;; current directory, IDO will wait 0.7 seconds and then whisk you off
;; to some totally unrelated directory that happens to already have a
;; file by that filename. Here we turn off that "feature".
;;
;; If you want to quickly get to a file somewhere else on the
;; filesystem, helm-projectile offers a much better user experience.
;; IDO is better for navigating in the traditional directory-based
;; system.
(setq ido-auto-merge-work-directories-length -1)

;; Follow symlinks without prompting. If this isn't done, then you
;; will get a prompt every time you edit init.el with M-RET e i. (That
;; is, assuming that ~/.emacs.d/init.el is a symlink, which is how the
;; setup script sets it up.)
(setq vc-follow-symlinks t)

;; Automatically reload files that were changed on disk, if they have
;; not been modified in Emacs since the last time they were saved.
(global-auto-revert-mode 1)

;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
(setq auto-revert-interval 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired

;; Suppress the 'ls does not support --dired' warning. Doing this instead
;; of installing a dired-compatibile ls is much easier, although it may
;; cause problems with e.g. filenames that have leading spaces. If you
;; have a lot of filenames with leading spaces, though, you probably have
;; bigger problems ;)
(setq dired-use-ls-dired nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Saving files

;; Put backup files in $HOME/.emacs-backups, rather than in the
;; current directory.
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; Always use copying to make backup files. This prevents hard links
;; from being made to point at the backup file rather than the
;; original.
(setq backup-by-copying t)

;; Keep multiple numbered backup files, rather than a single
;; unnumbered backup file.
(setq version-control t)

;; Delete old backups silently, instead of asking for confirmation.
(setq delete-old-versions t)

;; Don't make autosave files.
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text formatting

;; Don't use tabs for indentation, even in deeply indented lines. (Why
;; would anyone want their editor to *sometimes* use tabs?)
(setq-default indent-tabs-mode nil)

;; Sentences end with one space, not two. If you end sentences with
;; one space, this has two concrete effects on filling with M-q.
;; Firstly, it allows Emacs to break directly after a sentence.
;; Secondly, consider the case when the end of a line has an
;; end-of-sentence period followed by trailing whitespace and when at
;; least the last word of the sentence is too long for the line. In
;; that case, this option prevents filling from inserting two spaces
;; after the period when it is wrapped to the next line.
(setq sentence-end-double-space nil)

;; Trim trailing whitespace on save. This will get rid of end-of-line
;; whitespace, and reduce the number of blank lines at the end of the
;; file to one.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Add a trailing newline if there is not one already, when saving.
;; This is enabled for default for certain modes, but we want it
;; everywhere (e.g. when editing .gitignore files).
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Killing and yanking

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Searching

;; Eliminate the quarter-second delay before I-search matches are
;; highlighted, because delays suck.
(setq lazy-highlight-initial-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Windows

;; Use S-left, S-right, S-up, and S-down to move between windows. This
;; is much more convenient and efficient than using C-x o.
(windmove-default-keybindings)

;; Use C-c left and C-c right to undo and redo (actually, cancel a
;; sequence of undos) through window layouts. For instance, you can
;; use C-x 1 to focus on a particular window, then return to your
;; previous layout with C-c left.
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Customization

;;; Radian does not use custom.el. However, people using Radian might
;;; want to. It is the Radian way to put user-specific customizations
;;; into the user-specific configuration files, so we'll tell
;;; custom.el to put its customizations into init.local.el. We don't
;;; need to call `load', since this is done later by
;;; `radian-load-user-config'.

;; Store customizations made by custom.el into ~/.emacs.d/init.local.el
;; instead of this file.
(setq custom-file (concat user-emacs-directory "init.local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ElDoc

;; Enable ElDoc when editing Lisps and using Lisp REPLs.

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook (lambda () (eldoc-mode 1))))

(when (member 'clojure-mode radian-packages)
  (add-hook 'clojure-mode-hook (lambda () (eldoc-mode 1))))

(when (member 'cider radian-packages)
  (add-hook 'cider-repl-mode-hook (lambda () (eldoc-mode 1))))

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Lisp

;; Add a keybinding (M-RET r) for reloading this file (init.el). This
;; is useful for when you have several instances of Emacs open and you
;; change something in your configuration, then later come back to an
;; old Emacs that was opened before you made the change. You can then
;; just press M-RET r to get the change into that instance.
(global-set-key (kbd "M-RET r")
                '(lambda ()
                   "Reload init.el."
                   (interactive)
                   (load-file "~/.emacs.d/init.el")))

;; Add a keybinding (C-c C-k) for evaluating a buffer of Elisp. This
;; is consistent with the keybindings for evaluating a buffer in CIDER
;; and Geiser.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-k") 'eval-buffer)))

;; Add a keybinding (M-.) for jumping to the source of Elisp functions
;; and variables. This is consistent with the keybindings for jumping
;; to source in CIDER and Geiser, with the exception of those
;; keybindings not requiring a prefix argument to look at variables
;; (because Elisp has separate namespaces for functions and variables,
;; whereas Clojure and Schemes do not).

(defun find-function-or-variable (&optional prefix)
  "Acts like `find-function' without a prefix argument, and like
`find-variable' with a prefix argument."
  (interactive "P")
  (if prefix
      (call-interactively 'find-variable)
    (call-interactively 'find-function)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.")
                           'find-function-or-variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set up package installation system

;;; The following code will install any packages specified in
;;; `radian-packages' that are not already installed. It will only
;;; call `package-refresh-contents' if it is necessary to install a
;;; new package. The main logic is based on [1]. In future, this code
;;; may be extended to uninstall packages that are not present in
;;; `radian-packages', as per issue #99.
;;;
;;; [1]: http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

;; Since `package-archives', `package-pinned-packages', and
;; `package-installed-p' are not autoloaded, we must explicitly load
;; package.el before using any of them.
(require 'package)

;; Add package repositories. GNU is the default repository; MELPA is
;; necessary to get most of the packages we are interested in.
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration (2 of 4)

(radian-load-user-config "init.pre.local.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Install and activate packages

;; Initialize package.el. This is necessary to use
;; `package-installed-p'.
(package-initialize)

;; If any of the packages in `radian-packages' are not installed...
(unless (cl-every 'package-installed-p radian-packages)
  ;; ... then make sure that we know about the latest versions of all
  ;;     the packages...
  (package-refresh-contents)
  ;; ... and install any packages that aren't already installed.
  (dolist (package radian-packages)
    (unless (package-installed-p package)
      (package-install package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration (3 of 4)

(radian-load-user-config "init.post.local.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Helm (and helm-projectile and helm-smex)

(when (member 'helm radian-packages)

  ;; Use Helm mode for many standard Emacs commands.
  (helm-mode 1)

  (when (member 'helm-projectile radian-packages)

    ;; Use Helm mode for Projectile commands. Using helm-projectile-toggle
    ;; instead of helm-projectile-on means we don't get a useless "Turn on
    ;; helm-projectile key bindings" message in the minibuffer during startup.
    ;;
    ;; The local binding of ad-redefinition works around a warning message
    ;; "ad-handle-definition: `tramp-read-passwd' got redefined", as per [1].
    ;;
    ;; [1]: https://github.com/emacs-helm/helm/issues/1498#issuecomment-218249480
    (let ((ad-redefinition-action 'accept))
      (helm-projectile-toggle 1)))

  ;; Use Helm mode for M-x. If helm-smex is enabled, use it to get
  ;; sorting by usage.
  (global-set-key (kbd "M-x")
                  (if (member 'helm-smex radian-packages)
                      'helm-smex 'helm-M-x))

  ;; Fix the unreadable default color for the prefix argument in the
  ;; Helm M-x buffer.
  (when radian-customize-tweak-colors
    (set-face-foreground 'helm-prefarg "#FFFF66"))

  ;; Use fuzzy matching for Helm.
  (setq helm-mode-fuzzy-match t)

  ;; Get rid of the awful background color for buffers corresponding to files
  ;; modified outside of Emacs.
  (when radian-customize-tweak-colors
    (set-face-background 'helm-buffer-saved-out nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Projectile

(when (member 'projectile radian-packages)

  ;; Enable Projectile everywhere.
  (projectile-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Undo Tree

(when (member 'undo-tree radian-packages)

  ;; Enable Undo Tree everywhere.
  (global-undo-tree-mode 1)

  ;; By default, `undo' (and by extension `undo-tree-undo') is bound
  ;; to C-_ and C-/, and `undo-tree-redo' is bound to M-_. It's
  ;; logical to also bind M-/ to `undo-tree-redo'. This overrides the
  ;; default binding of M-/, which is to `dabbrev-expand'.
  (global-set-key (kbd "M-/") 'undo-tree-redo)

  ;; Make undo history persistent between Emacs sessions. Don't rely
  ;; on this too much if your files tend to change outside of Emacs
  ;; (e.g. if you use Git).
  (setq undo-tree-auto-save-history t)

  ;; Put all the undo information in a single directory, instead of in
  ;; each file's directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs-undos"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: ace-jump-mode

(when (member 'ace-jump-mode radian-packages)

  ;; Create a keybinding for ace-jump-mode. Clojure mode already binds
  ;; C-c SPC, the recommended keybinding, to `clojure-align', so use
  ;; C-c C-SPC instead.
  (global-set-key (kbd "C-c C-SPC") 'ace-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Company (and company-statistics)

(when (member 'company radian-packages)

  ;; Turn on Company everywhere.
  (global-company-mode 1)

  ;; Show completions instantly, rather than after half a second.
  (setq company-idle-delay 0)

  ;; Show completions after typing a single character, rather than
  ;; after typing three characters.
  (setq company-minimum-prefix-length 1)

  ;; Show a maximum of 20 suggestions, rather than 10.
  (setq company-tooltip-limit 20)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum 21)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Don't prevent non-matching input (which will dismiss the
  ;; completions menu), even if the user interacts explicitly with
  ;; Company.
  (setq company-require-match nil)

  ;; Prevent suggestions from being triggered automatically. In particular,
  ;; this makes it so that:
  ;; - TAB will always complete the current selection.
  ;; - RET will only complete the current selection if the user has explicitly
  ;;   interacted with Company.
  ;; - SPC will never complete the current selection.
  ;;
  ;; Based on:
  ;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;; - http://emacs.stackexchange.com/a/13290/12534
  ;; - http://stackoverflow.com/a/22863701/3538165
  ;;
  ;; See also:
  ;; - http://emacs.stackexchange.com/a/24800/12534
  ;; - http://emacs.stackexchange.com/q/27459/12534

  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))

  ;; <tab> is for windowed Emacs; TAB is for terminal Emacs.
  (dolist (key '("<tab>" "TAB"))
    (define-key company-active-map (kbd key) #'company-complete-selection))

  (define-key company-active-map (kbd "SPC") nil)

  ;; Company appears to override the above keymap based on
  ;; company-auto-complete-chars. Turning it off ensures we have full
  ;; control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This appears to only be an issue in comments and
  ;; strings in Clojure.
  (setq company-dabbrev-downcase nil)

  ;; Company mode overrides standard REPL bindings for M-p and M-n
  ;; when the completions menu is visible. Prevent this, but only in
  ;; REPL modes.

  (dolist (hook (remove nil
                        (list
                         (when (member 'cider radian-packages)
                           'cider-repl-mode-hook)
                         (when (member 'geiser radian-packages)
                           'geiser-repl-mode-hook))))
    (add-hook hook
              (lambda ()
                (make-local-variable 'company-active-map)
                (setq company-active-map (copy-tree company-active-map))
                (define-key company-active-map (kbd "M-p") nil)
                (define-key company-active-map (kbd "M-n") nil))))

  (when (member 'company-statistics radian-packages)

    ;; Turn on company-statistics if available.
    (company-statistics-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Aggressive Indent

(when (member 'aggressive-indent radian-packages)

  ;; Enable Aggressive Indent everywhere, except the modes in
  ;; `aggressive-indent-excluded-modes'.
  (global-aggressive-indent-mode 1)

  ;; Disable Aggressive Indent in Re-Builder mode. I don't think it
  ;; does anything in this mode, and it shadows the C-c C-q binding
  ;; provided by Re-Builder (so you can't quit!).
  (add-to-list 'aggressive-indent-excluded-modes 'reb-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: Paredit

(when (member 'paredit radian-packages)

  ;; Enable Paredit when editing Lisps and using Lisp REPLs.

  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode))

  (when (member 'clojure-mode radian-packages)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode))

  (when (member 'cider radian-packages)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

  (when (member 'geiser radian-packages)
    (add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: clojure-mode

(when (member 'clojure-mode radian-packages)

  (eval-after-load 'clojure-mode
    (lambda ()

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
      ;;;
      ;;; (:keyword
      ;;;   map)

      (setq clojure-indent-style ':align-arguments)

      ;; We can't use define-clojure-indent here, due to a perverse
      ;; threefold conspiracy perpetrated by dash.el, recursive
      ;; macroexpansion, and the Gilardi scenario. See [1].
      ;;
      ;; Ideally, we would be able to set the identation rules for
      ;; *all* keywords at the same time. But until we figure out how
      ;; to do that, we just have to deal with every keyword
      ;; individually. See issue #26.
      ;;
      ;; [1]: http://emacs.stackexchange.com/q/26261/12534
      (dolist (spec '((-> 1)
                      (->> 1)
                      (:import 0)
                      (:overall-average 0)
                      (:require 0)
                      (:use 0)))
        (put-clojure-indent (car spec) (cdr spec)))

      ;; Make sure electric indentation *always* works. For some
      ;; reason, if this is omitted, electric indentation works most
      ;; of the time, but it fails inside Clojure docstrings. (TAB
      ;; will add the requisite two spaces, but you shouldn't have to
      ;; do this manually after pressing RET.) I'd like to find a more
      ;; elegant solution to this problem. See issue #2.
      ;;
      ;; <return> is for windowed Emacs; RET is for terminal Emacs.
      (dolist (key '("<return>" "RET"))
        (define-key clojure-mode-map (kbd key) 'newline-and-indent))

      ;; clojure-mode does not correctly identify the docstrings of
      ;; protocol methods as docstrings, and as such electric
      ;; indentation does not work for them. Additionally, when you
      ;; hack a clojure.core function, such as defonce or defrecord,
      ;; to provide docstring functionality, those docstrings are
      ;; (perhaps rightly, but annoyingly) not recognized as
      ;; docstrings either. However, there is an easy way to get
      ;; electric indentation working for all potential docstrings:
      ;; simply tell clojure-mode that *all* strings are docstrings.
      ;; This will not change the font locking, because for some weird
      ;; reason clojure-mode determines whether you're in a docstring
      ;; by the font color instead of the other way around. Note that
      ;; this will cause electric indentation by two spaces in *all*
      ;; multiline strings, but since there are not very many
      ;; non-docstring multiline strings in Clojure this is not too
      ;; inconvenient. (And, after all, it's only electric, not
      ;; aggressive, indentation.)

      ;; Unfortunately, clojure-in-docstring-p is defined as an inline function,
      ;; so we can't override it. Instead, we replace clojure-indent-line.

      (defun radian-clojure-in-docstring-p ()
        "Check whether point is in a docstring."
        (or
         (eq (get-text-property (point) 'face) 'font-lock-doc-face)
         (eq (get-text-property (point) 'face) 'font-lock-string-face)))

      (defun clojure-indent-line ()
        "Indent current line as Clojure code."
        (if (radian-clojure-in-docstring-p)
            (save-excursion
              (beginning-of-line)
              (when (and (looking-at "^\\s-*")
                         (<= (string-width (match-string-no-properties 0))
                             (string-width (clojure-docstring-fill-prefix))))
                (replace-match (clojure-docstring-fill-prefix))))
          (lisp-indent-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: CIDER

(when (member 'cider radian-packages)

  ;; Allow usage of the C-c M-j and C-c M-J shortcuts everywhere.
  (global-set-key (kbd "C-c M-j") 'cider-jack-in)
  (global-set-key (kbd "C-c M-J") 'cider-jack-in-clojurescript)

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.

  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.

  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (add-hook 'cider-connected-hook
            (lambda ()
              (save-excursion
                (goto-char (point-min))
                (insert
                 (with-current-buffer nrepl-server-buffer
                   (buffer-string))))))

  ;; Turn off dependency injection. It's simpler to put dependencies
  ;; in profiles.clj, to head off potentially confusing version
  ;; conflicts.
  (setq cider-inject-dependencies-at-jack-in nil)

  ;; Make the REPL a lot more awesome. This injects a bunch of extra
  ;; features specified by the :awesome vector in profiles.clj. Note
  ;; that refactor-nrepl is *not* enabled by default.
  (setq cider-lein-parameters "with-profile +awesome repl :headless")

  ;; The CIDER welcome message often obscures any error messages that
  ;; the above code is supposed to be making visible. So, we need to
  ;; turn off the welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; Sometimes in the CIDER REPL, when Emacs is running slowly, you
  ;; can manage to press TAB before the Company completions menu pops
  ;; up. This makes a Helm completions buffer appear, which is
  ;; disorienting. So we reset TAB to its default functionality (i.e.
  ;; indent only) in the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursur to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior.
  (setq cider-repl-pop-to-buffer-on-connect nil)

  ;; However, turning off the pop-to-buffer setting also prevents the
  ;; REPL buffer from *opening*. To fix this problem, we add an advice
  ;; to open the REPL buffer after the REPL has started.
  (defadvice cider-repl-init (after display-repl-buffer)
    (display-buffer buffer))

  ;; Use figwheel-sidecar for launching ClojureScript REPLs. This
  ;; supports a fully integrated ClojureScript development experience
  ;; in Emacs. For more information about how to use such a setup,
  ;; see [1].
  ;;
  ;; [1]: https://github.com/raxod502/minimal-webapp
  (setq cider-cljs-lein-repl
        "(do
  (require 'figwheel-sidecar.repl-api)
  (figwheel-sidecar.repl-api/start-figwheel!)
  (figwheel-sidecar.repl-api/cljs-repl))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: git-commit

(when (member 'git-commit radian-packages)

  ;; Enable the functionality of the git-commit package. This should
  ;; be enabled by default for emacsclient, but we need to turn it on
  ;; explicitly for regular Emacs.
  (global-git-commit-mode 1)

  ;; Wrap summary at 50 characters as per [1].
  ;;
  ;; [1]: http://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Package: markdown-toc

(when (member 'markdown-toc radian-packages)

  ;; Remove the header inserted before the table of contents. If you want a
  ;; header, just add one before the "markdown-toc start" comment -- this way,
  ;; you can have different header styles in different documents.
  (setq markdown-toc-header-toc-title ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Color themes

;; Load the official Radian color scheme.
(when radian-customize-tweak-colors
  ;; Passing `t' to `load-theme' suppresses the confirmation message.
  (load-theme 'leuven t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode line

;;; The following code customizes the mode bar to something like:
;;; [*] init.el        72% (389,30)  [radian]  (Emacs-Lisp Paredit AggrIndent)

(defvar mode-line-modified-radian
  '(:eval (propertize (if (and (buffer-modified-p)
                               (buffer-file-name))
                          "[*]" "   ")
                      ;; make sure to show it in the same color as the
                      ;; buffer name
                      'face 'mode-line-buffer-id))
  "Construct for the mode line that shows [*] if the buffer
has been modified, and whitespace otherwise.")

(when (member 'projectile radian-packages)
  (defvar mode-line-projectile-project
    '("["
      (:eval (projectile-project-name))
      "]")
    "Construct for the mode line that shows the current Projectile
project (or a hyphen if there is no current project) between
brackets."))

(setq-default mode-line-format
              (list
               ;; Show a warning if Emacs is low on memory.
               "%e"
               ;; Show [*] if the buffer is modified.
               mode-line-modified-radian
               " "
               ;; Show the name of the current buffer.
               mode-line-buffer-identification
               "   "
               ;; Show the row and column of point.
               mode-line-position
               (when (member 'projectile radian-packages)
                 (list
                  " "
                  ;; Show the current Projectile project.
                  mode-line-projectile-project))
               "  "
               ;; Show the active major and minor modes.
               mode-line-modes))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)

;;; Customize mode line indicators

;; aggressive-indent-mode
(when (member 'aggressive-indent radian-packages)
  (setf (cdr (assoc 'aggressive-indent-mode minor-mode-alist)) '(" AggrIndent")))

;; cider-mode
(when (member 'cider radian-packages)
  (setq cider-mode-line nil))

;; company-mode
(when (member 'company radian-packages)
  (setq minor-mode-alist (assq-delete-all 'company-mode minor-mode-alist)))

;; eldoc-mode
(setq eldoc-minor-mode-string nil)

;; helm-mode
;;
;; Note that Helm has `helm-mode-line-string', but this only affects
;; what is shown in the mode line for a Helm buffer.
(when (member 'helm radian-packages)
  (setq minor-mode-alist (assq-delete-all 'helm-mode minor-mode-alist)))

;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq mode-name "Lisp-Interaction")))

;; projectile-mode
(when (member 'projectile radian-packages)
  (setq projectile-mode-line nil))

;; undo-tree-mode
(when (member 'undo-tree radian-packages)
  (setq undo-tree-mode-lighter nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration (4 of 4)

(radian-load-user-config "init.local.el")
