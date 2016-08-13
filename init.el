;;; Ensure at least Emacs 24.5.1
(when (version< emacs-version "24.5.1")
  (error "This setup requires at least Emacs 24.5.1 (running Emacs %s)"
         emacs-version))

;;;; Tweakable parameters

;; These parameters let people using this init-file as a starting point
;; for their Emacs do some basic customization without messing with the
;; file proper.

;;; Control color customizations. Nil for no color customizations and
;;; non-nil for all color customizations (for best results use the
;;; Solarized Light theme in your terminal emulator).
(setq radon-emacs-tweak-colors t)

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

;;; Define keybindings for opening and reloading init.el.

(defun open-initfile ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
  "Opens init.el in the current buffer.")

(defun reload-initfile ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  "Reloads init.el.")

(global-set-key (kbd "<f9>") 'open-initfile)
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

;;;; File saving and loading

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
;;; whitespace, and reduce the number of blank lines at the end of the
;;; file to one.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Add a trailing newline if there is not one already, when
;;; saving. This is enabled for default for certain modes, but we want
;;; it everywhere (e.g. when editing .gitignore files).
(setq require-final-newline t)

;;; Automatically reload files that were changed on disk, if they have
;;; not been modified in Emacs since the last time they were saved.
(global-auto-revert-mode 1)

;;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
(setq auto-revert-interval 1)

;;; Follow symlinks without prompting. If this isn't done, then you will
;;; get a prompt every time you edit init.el with F9. (This is assuming
;;; that ~/.emacs.d/init.el is a symlink, which is how the setup script
;;; sets it up.)
(setq vc-follow-symlinks t)

;;; Suppress the 'ls does not support --dired' warning. Doing this instead
;;; of installing a dired-compatibile ls is much easier, although it may
;;; cause problems with e.g. filenames that have leading spaces. If you
;;; have a lot of filenames with leading spaces, though, you probably have
;;; bigger problems ;)
(setq dired-use-ls-dired nil)

;;;; Text format

;;; Don't use tabs for indentation, even in deeply indented lines. (Why
;;; would anyone want their editor to *sometimes* use tabs?)
(setq-default indent-tabs-mode nil)

;;; Sentences end with one space, not two. If you end sentences with
;;; one space, this has two concrete effects on filling with M-q.
;;; Firstly, it allows Emacs to break directly after a sentence.
;;; Secondly, consider the case when the end of a line has an
;;; end-of-sentence period followed by trailing whitespace and when at
;;; least the last word of the sentence is too long for the line. In
;;; that case, this option prevents filling from inserting two spaces
;;; after the period when it is wrapped to the next line.
(setq sentence-end-double-space nil)

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
;;; packages we need, such as Projectile. Therefore, add the melpa-stable
;;; repository. We need melpa (non-stable) for helm-smex.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t) ; this appends to the end of the list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t) ; this appends to the end of the list

(defvar my-packages
  '(
    ace-jump-mode ; quickly jump to words, characters, or lines onscreen
    aggressive-indent ; keep code correctly indented at all times
    cider ; Clojure REPL integration
    clojure-mode ; Clojure indentation and syntax highlighting
    company ; autocompletion with pop-up menu
    company-statistics ; sort company completions by usage
    helm ; better interface for selecting files, buffers, or commands
    helm-projectile ; use helm for projectile
    helm-smex ; sort M-x suggestions by usage
    magit ; a git porcelain inside Emacs
    paredit ; keep parentheses correctly balanced at all times
    projectile ; quickly jump to files organized by project
    undo-tree ; more intuitive undo/redo
    )
  "The packages required by radon-emacs.")

;;; Ensure that we get a stable version of CIDER, and not a snapshot version.
;;; This resolved an issue with the 0.13.0 update to CIDER trying to inject
;;; a nonexistent snapshot version of cider-nrepl, if you used the snapshot
;;; version of CIDER.
(setq package-pinned-packages
      '((cider . "melpa-stable")))

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

;;;; Package: Ace Jump Mode
;; Allows quickly jumping to an arbitrary word, character, or line.
;;;;

;; Clojure mode already binds C-c SPC to clojure-align, so use C-c C-SPC
;; instead.

(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)

;;;; Package: Windmove
;; Allows switching to adjacent windows using shift + arrow keys.
;;;;

(windmove-default-keybindings)

;;;; Package: Winner Mode
;; Allows navigating through window layout history using C-c <left> and C-c <right>.
;;;;

(winner-mode 1)

;;;; Package: Undo Tree
;; Replaces Emacs' default redo-as-undo behavior with a more sensible undo/redo
;; tree, which can be visualized in complex situations.
;;;;

;;; Turn on Undo Tree everywhere.
(global-undo-tree-mode 1)

;;; Override the default binding of M-/ to dabbrev-expand.
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;; Make undo history persistent between Emacs sessions.
(setq undo-tree-auto-save-history t)

;;; Put all the undo information in a single directory.
(setq undo-tree-history-directory-alist '(("." . "~/.emacs-undos")))

;;;; Package: Magit
;; A git porcelain inside Emacs.
;;;;

;;; Create a keybinding for magit-status, as suggested in:
;;; https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)

;;;; Package: IDO
;; Makes completion more intelligent by using fuzzy matching and better
;; keybindings.
;;;;

;;; Use IDO mode for C-x C-f and friends. (Most other things should be
;;; using Helm mode.)
(ido-mode 'files)

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

;;; Use Helm mode for many standard Emacs commands.
(helm-mode 1)

;;; Use Helm mode for Projectile commands. Using helm-projectile-toggle
;;; instead of helm-projectile-on means we don't get a useless "Turn on
;;; helm-projectile key bindings" message in the minibuffer during startup.

;; The local binding of ad-redefinition works around a warning message
;; "ad-handle-definition: `tramp-read-passwd' got redefined", as described at:
;; https://github.com/emacs-helm/helm/issues/1498#issuecomment-218249480

(let ((ad-redefinition-action 'accept))
  (helm-projectile-toggle 1))

;;; Use Helm mode for M-x, using helm-smex to get sorting by usage.
(global-set-key (kbd "M-x") 'helm-smex)

;;; Use fuzzy matching.
(setq helm-mode-fuzzy-match t)

;;; Get rid of the awful background color for buffers corresponding to files
;;; modified outside of Emacs.
(when radon-emacs-tweak-colors
  (set-face-background 'helm-buffer-saved-out nil))

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
;;; Also, don't display metadata in the echo area. (This conflicts with
;;; ElDoc mode.)
(setq company-frontends '(company-pseudo-tooltip-frontend))

;;; Show quick-reference numbers in the tooltip. (Select a completion with
;;; M-1 through M-0.)
(setq company-show-numbers t)

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

;;; Prevent Company completions from being lowercased in the completion menu.
;;; This appears to only be an issue in comments and strings in Clojure.
(setq company-dabbrev-downcase nil)

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

;;; Always truncate ElDoc messages to one line. This prevents the echo area from
;;; resizing itself unexpectedly when point is on a Clojure variable with a
;;; multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;;;; Package: Clojure mode
;; Provides indentation and syntax highlighting for Clojure and
;; ClojureScript files.
;;;;

(require 'clojure-mode) ; required for variables to be available

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

(define-clojure-indent
  (-> 1)
  (->> 1)
  ;; Ideally, we would be able to set the identation rules for *all*
  ;; keywords at the same time. But until we figure out how to do
  ;; that, we just have to deal with every keyword individually.
  (:import 0)
  (:overall-average 0)
  (:require 0)
  (:use 0))

;;; Make sure electric indentation *always* works. For some reason, if
;;; this is omitted, electric indentation works most of the time, but it
;;; fails inside Clojure docstrings. (TAB will add the requisite two
;;; spaces, but you shouldn't have to do this manually after pressing RET.)
;;; I'd like to find a more elegant solution to this problem.
(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;; Clojure mode does not correctly identify the docstrings of protocol
;;; methods as docstrings, and as such electric indentation does not work
;;; for them. Additionally, when you hack a clojure.core function, such as
;;; defonce or defrecord, to provide docstring functionality, those
;;; docstrings are (perhaps rightly, but annoyingly) not recognized as
;;; docstrings either. However, there is an easy way to get electric
;;; indentation working for all potential docstrings: simply tell
;;; clojure-mode that *all* strings are docstrings. This will not change
;;; the font locking, because for some weird reason clojure-mode determines
;;; whether you're in a docstring by the font color instead of the other
;;; way around. Note that this will cause electric indentation by two spaces
;;; in *all* multiline strings, but since there are not very many
;;; non-docstring multiline strings in Clojure this is not too inconvenient.
;;; (And, after all, it's only electric, not aggressive, indentation.)

;;; Unfortunately, clojure-in-docstring-p is defined as an inline function,
;;; so we can't override it. Instead, we replace clojure-indent-line.

(defun radon-clojure-in-docstring-p ()
  "Check whether point is in a docstring."
  (or
   (eq (get-text-property (point) 'face) 'font-lock-doc-face)
   (eq (get-text-property (point) 'face) 'font-lock-string-face)))

(defun clojure-indent-line ()
  "Indent current line as Clojure code."
  (if (radon-clojure-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (clojure-docstring-fill-prefix))))
          (replace-match (clojure-docstring-fill-prefix))))
    (lisp-indent-line)))

;;;; Package: CIDER
;; Provides nREPL integration for Clojure and ClojureScript, documentation
;; and source lookups, and more.
;;;;

;;; Allow usage of the C-c M-j shortcut everywhere.
(global-set-key (kbd "C-c M-j") 'cider-jack-in)

;;; By default, any error messages that occur when CIDER is starting up
;;; are placed in the *nrepl-server* buffer and not in the *cider-repl*
;;; buffer. This is silly, since no-one wants to check *nrepl-server*
;;; every time they start a REPL, and if you don't then startup errors
;;; (including errors in anything loaded by the :main namespace) are
;;; effectively silenced. So we copy everything from the *nrepl-server*
;;; buffer to the *cider-repl* buffer, as soon as the latter is available.

;;; Note that this does *not* help in the case of things going so horribly
;;; wrong that the REPL can't even start. In this case you will have to
;;; check the *nrepl-server* buffer manually. Perhaps an error message
;;; that is visible from any buffer could be added in future.

;;; Thanks to malabarba on Clojurians Slack for providing the following
;;; code:

(add-hook 'cider-connected-hook
          (lambda ()
            (save-excursion
              (goto-char (point-min))
              (insert
               (with-current-buffer nrepl-server-buffer
                 (buffer-string))))))

;;; Turn off dependency injection. It's simpler to put dependencies in
;;; profiles.clj, to head off potentially confusing version conflicts.
(setq cider-inject-dependencies-at-jack-in nil)

;;; Make the REPL a lot more awesome. This injects a bunch of extra
;;; features specified by the :awesome vector in profiles.clj. Note that
;;; refactor-nrepl is *not* enabled by default.
(setq cider-lein-parameters "with-profile +awesome repl :headless")

;;; The CIDER welcome message often obscures any error messages that the
;;; above code is supposed to be making visible. So, we need to turn off
;;; the welcome message.
(setq cider-repl-display-help-banner nil)

;;; Sometimes in the CIDER REPL, when Emacs is running slowly, you can
;;; manage to press TAB before the Company completions menu pops
;;; up. This makes a Helm completions buffer appear, which is
;;; disorienting. So we reset TAB to its default functionality
;;; (i.e. indent only) in the CIDER REPL.
(setq cider-repl-tab-command 'indent-for-tab-command)

;;; Company mode overrides the CIDER REPL's bindings for M-p and M-n when
;;; the completions menu is visible. Prevent this, but only in REPL mode.

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (make-local-variable 'company-active-map)
            (setq company-active-map (copy-tree company-active-map))
            (define-key company-active-map (kbd "M-p") nil)
            (define-key company-active-map (kbd "M-n") nil)))

;;; Don't focus the cursor in the CIDER REPL once it starts. Since the
;;; REPL takes so long to start up, especially for large projects, you
;;; either have to wait for a minute without doing anything or be
;;; prepared for your cursur to suddenly shift buffers without warning
;;; sometime in the near future. This is annoying, so turn off the
;;; behavior.
(setq cider-repl-pop-to-buffer-on-connect nil)

;;; However, turning off the pop-to-buffer setting also prevents the
;;; REPL buffer from *opening*. To fix this problem, we add an advice
;;; to open the REPL buffer after the REPL has started.
(defadvice cider-repl-init (after display-repl-buffer)
  (display-buffer buffer))

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
(when radon-emacs-tweak-colors
  (load-theme 'leuven t)) ; the last argument suppresses a confirmation message

;;; The default highlight color for I-searches is quite dark and makes it hard
;;; to read the highlighted text. Change it to a nice light blue, and get rid of
;;; the distracting underline. (I mean really, how is an *underline* supposed to
;;; help you see something that's highlighted in *bright blue*?)
(when radon-emacs-tweak-colors
  (set-face-background 'lazy-highlight "#B1EAFC")
  (set-face-underline 'lazy-highlight nil))

;;; Customize the mode bar to something like:
;;; [*] init.el        72% (389,30)  [dotfiles]  (Emacs-Lisp Paredit AggrIndent)

(defvar mode-line-modified-radon
  '(:eval (propertize (if (and (buffer-modified-p)
                               (buffer-file-name))
                          "[*]" "   ")
                      'face 'mode-line-buffer-id))
  "Construct for the mode line that shows [*] if the buffer
has been modified, and whitespace otherwise.")

(defvar mode-line-projectile-project
  '("["
    (:eval (projectile-project-name))
    "]")
  "Construct for the mode line that shows the current Projectile
project (or a hyphen if there is no current project) between
brackets.")

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
               " "
               ;; Show the current Projectile project.
               mode-line-projectile-project
               "  "
               ;; Show the active major and minor modes.
               mode-line-modes))

(column-number-mode 1) ; makes mode-line-position show the column

;;; Customize mode indicators

;;; Major modes.
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq mode-name "Lisp-Interaction")))

;;; Minor modes that provide a customizable variable.
(setq cider-mode-line nil)
(setq eldoc-minor-mode-string nil)
(setq projectile-mode-line nil)
(setq undo-tree-mode-lighter nil)

;;; Minor modes that do not provide a customizable variable.
;;; Note that Helm has helm-mode-line-string, but this only affects what is
;;; shown in the mode line for a Helm buffer.
(setf (cdr (assoc 'aggressive-indent-mode minor-mode-alist)) '(" AggrIndent"))
(setq minor-mode-alist (assq-delete-all 'company-mode minor-mode-alist))
(setq minor-mode-alist (assq-delete-all 'helm-mode minor-mode-alist))
