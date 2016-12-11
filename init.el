;; The general outline of this file is as follows:
;;
;; * Set up variables and functions for managing user-specific
;;   configuration. This needs to be done first because the
;;   user-specific options could affect the behavior of anything else
;;   in the file.
;;
;; * Run the user-specific configuration file init.before.local.el.
;;
;; * Perform configuration of native Emacs functionality.
;;
;; * Bootstrap use-package.
;;
;; * Load and configure packages.
;;
;; * Load color schemes and perform other appearance tweaks that look
;;   odd unless they are done at the very end of initialization.
;;
;; * Run the user-specific configuration file init.local.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration variables

;;; The following are user-specific configuration options that can be
;;; overridden in init.before.local.el. They parsed by
;;; create-init-before-local-el.sh so the user can set them up
;;; interactively. Configuration variables marked with "For manual
;;; configuration only" are placed in the created init.before.local.el
;;; file, but the user is not offered the option to change their
;;; values interactively.
;;;
;;; Here we are using the defvar-nil-setq pattern described in [1],
;;; which makes it so that changes to these declarations will be
;;; picked up by a reload of init.el (M-RET r).
;;;
;;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html

;; Control color customizations. If you want to use the default
;; Radian color scheme (Solarized Light + Leuven), which is
;; guaranteed to work in terminal Emacs, set this to `t' (default)
;; and make sure to set your terminal emulator to use the Solarized
;; Light color scheme. This will also turn on various tweaks designed
;; to make various parts of the Leuven theme look better. If you want
;; to set up your own color scheme, or use the Emacs default, set
;; this to `nil'.
(defvar radian-customize-tweak-colors nil)
(setq radian-customize-tweak-colors t)

;; Control completion mechanisms. If this is `helm-ido', then Helm is
;; used for most completions and IDO is used for finding files. If it
;; is `ivy', then Ivy and Counsel are used for most completions. By
;; default, this also enables Swiper for Isearches. You can disable
;; this functionality by placing (radian-disable-package 'swiper) in
;; your init.before.local.el. Note that selecting `helm-ido' or `ivy'
;; automatically disables the packages for the other completion
;; system.
(defvar radian-customize-completion-mechanism nil)
(setq radian-customize-completion-mechanism 'ivy)

;; Override package archives. If this is non-nil, then it is used as
;; the value of `package-archives' instead of the Radian default (GNU
;; plus MELPA).
;;
;; For manual configuration only.
(defvar radian-customize-package-archives nil)
(setq radian-customize-package-archives nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Optimization

;; Turn off garbage collection during startup, and then re-enable it.
;; This is supposed to improve startup time, because invoking the
;; garbage collector many times is slow. See [1].
;;
;; [1]: http://emacs.stackexchange.com/a/16595/12534

(setq gc-cons-threshold most-positive-fixnum)

(defun radian--reset-gc-cons-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

(add-hook 'emacs-startup-hook #'radian--reset-gc-cons-threshold)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Libraries

;; This provides some functions that are really quite necessary to
;; write productive Emacs-Lisp. It's unfortunate that we have to load
;; it eagerly, but it's loaded eagerly by many other packages as well,
;; so we're probably not losing much time by doing it here.
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific package management

;;; The following code sets up a simple way for users to selectively
;;; disable Radian's default packages in their init.before.local.el.
;;;
;;; Here we are using the defvar-nil-setq pattern described in [1],
;;; which makes it so that changes to this declaration will be picked
;;; up by a reload of init.el (M-RET r).
;;;
;;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html

(defvar radian-disabled-packages nil
  "Association list from packages to integers. Any package with
an entry greater than zero will not be automatically loaded by
Radian. Missing packages are treated as having entries of zero.

You can manipulate this list using `radian-disable-package',
which increments the entries of packages, and
`radian-reenable-package', which decrements the entries of
packages.

Note that this variable only affects the loading of packages that
are normally loaded automatically by Radian. If you want to
install your own package, add a `use-package' call to your
init.local.el.

Also note that you must make any modifications to this list
*before* packages are loaded, in order for your changes to have
an effect. This means your modifications should be done in
init.before.local.el.")
(setq radian-disabled-packages ())

(defun radian-disable-package (&rest packages)
  "Disables packages that would otherwise be loaded automatically
by Radian, by incrementing their entries in
`radian-disabled-packages'.

You can undo the effects of this function using
`radian-reenable-package'. In fact, you can even call
`radian-reenable-package' before this function is called, in
order to cancel out the effect preemptively.

Note that you must call this function *before* packages are
loaded, in order for your change to have an effect. This means
the call must be done in init.before.local.el."
  ;; Passing `t' to `add-to-list' makes the addition happen at the end
  ;; of the list.
  (dolist (package packages)
    (let ((association (assoc package radian-disabled-packages)))
      (if association
          (setcdr (assoc package radian-disabled-packages)
                  (1+ (cdr association)))
        (push (cons package 1) radian-disabled-packages)))))

(defun radian-reenable-package (&rest packages)
  "Undoes the effects of `radian-disable-package', by
decrementing the entries of the provided packages in
`radian-disabled-packages'.

In fact, you can even call this function before
`radian-disable-package' is called, in order to cancel out its
effect preemptively.

Note that you must call this function *before* packages are
loaded, in order for your change to have an effect. This means
the call must be done in init.before.local.el."
  (dolist (package packages)
    (let ((association (assoc package radian-disabled-packages)))
      (if association
          (setcdr (assoc package radian-disabled-packages)
                  (1+ (cdr association)))
        (push (cons package -1) radian-disabled-packages)))))

(defun radian-package-enabled-p (&rest packages)
  "Returns `t' if none of the given packages are disabled (for example,
by `radian-disable-package'), and `nil' otherwise.

See also `radian-disabled-packages'."
  (cl-every (lambda (package)
              (let ((association (assoc package radian-disabled-packages)))
                (or (not association)
                    (<= (cdr association) 0))))
            packages))

(defun radian-package-disabled-p (&rest packages)
  "Returns `t' if all of the given packages are disabled (for
example, by `radian-disable-package'), and `nil' otherwise.

See also `radian-disabled-packages'."
  (cl-every (lambda (package)
              (let ((association (assoc package radian-disabled-packages)))
                (and association
                     (> (cdr association) 0))))
            packages))

(defvar radian-quelpa-overrides nil
  "Association list from packages to quelpa forms. This alist
allows you to override where packages are loaded from. If the
entry for a package is nil (as distinct from a package not being
in the alist), then quelpa is not used to load the package even
if `:quelpa' is specified in the `use-package' form. If a package
has a non-nil entry, the package is loaded using quelpa no matter
what, and the entry is used as the recipe to pass to the
`:quelpa' keyword.

You can manipulate this list using `radian-override-quelpa' and
`radian-disable-quelpa-override'.

Note that this variable only affects the loading of packages that
are normally loaded automatically by Radian. If you want to load
your own package from quelpa, just put a `:quelpa' keyword in
your `use-package' form for that package.

Also note that you must make any modifications to this list
*before* packages are loaded, in order for your changes to have
an effect. This means your modifications should be done in
init.before.local.el.")
(setq radian-quelpa-overrides ())

(defun radian-override-quelpa (package &optional recipe)
  "Force PACKAGE to be loaded using quelpa with the provided
RECIPE, or using the default recipe from MELPA if RECIPE is not
provided. This only has an effect for packages that are loaded
via `use-package' by Radian."
  (let ((association (assoc package radian-quelpa-overrides)))
    (if association
        (setcdr association recipe)
      (push (cons package recipe) radian-quelpa-overrides))))

(defun radian-disable-quelpa-override (package)
  "Undo the effects of `radian-override-quelpa'."
  (assq-delete-all package radian-quelpa-overrides))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Deprecated API for user-specific package management

;;; The following code is deprecated, and is provided for backwards
;;; compatibility.

(defvar radian-packages nil
  "This variable is DEPRECATED. To add a package to Radian, call
`use-package' in init.local.el. To remove a package, call
`radian-disable-package' in init.before.local.el.

The packages required by Radian.")
(setq radian-packages ())

(defun radian-add-package (package)
  "This function is DEPRECATED. Call `use-package' in
init.local.el instead.

Adds the provided package to `radian-packages', if it is not
already present. For use in init.before.local.el. Note that this
function will not install the provided package."
  ;; Passing `t' to `add-to-list' makes the addition happen at the end
  ;; of the list.
  (add-to-list 'radian-packages package t)
  (radian-reenable-package package))

(defun radian-remove-package (package)
  "This function is DEPRECATED. Call `radian-disable-package' instead.

Removes the provided package from `radian-packages', if it is
present. For use in init.before.local.el. Note that this function
will not uninstall the package; it will only prevent the package
from being installed automatically. For now, you have to
uninstall packages manually, by deleting their folders in
~/.emacs.d/elpa."
  (setq radian-packages (delete package radian-packages))
  (radian-disable-package package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Function for loading user-specific configuration files

;;; The following function is used four times in init.el to load
;;; user-specific configuration files (init.before.local.el and
;;; init.local.el, as well as the deprecated init.pre.local.el and
;;; init.post.local.el).

(defun radian-load-user-config (filename)
  "If a file by the specified name exists in the ~/.emacs.d directory,
loads it. Otherwise, fails silently."
  (let ((file (concat user-emacs-directory filename)))
    (when (file-exists-p file)
      (load file 'noerror 'nomessage))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load user-specific configuration file (1 of 2)

(radian-load-user-config "init.before.local.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Process user-specific configuration

;; Normalize configuration variables.

(unless (member radian-customize-completion-mechanism
                '(helm-ido ivy))
  (lwarn
   '(radian) :error
   "%s is not a valid setting for radian-customize-completion-mechanism, setting to helm-ido"
   radian-customize-completion-mechanism)
  (setq radian-customize-completion-mechanism 'helm-ido))

;; If the user has selected `helm-ido' as their completion framework,
;; then we should disable Ivy and friends. If the user has selected
;; `ivy', then we should disable Helm and friends. Note that this is
;; just for convenience, and the user can override it by using
;; `radian-reenable-package' in their init.before.local.el (because
;; `radian-disable-package' and `radian-reenable-package' are
;; commutative).

(when (equal radian-customize-completion-mechanism 'helm-ido)
  (radian-disable-package 'flx 'ivy 'counsel 'counsel-projectile 'swiper))

(when (equal radian-customize-completion-mechanism 'ivy)
  (radian-disable-package 'helm 'helm-projectile 'helm-smex))

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

  (defun radian-scroll-down ()
    "Scroll down one line."
    (interactive)
    (scroll-down 1))

  (defun radian-scroll-up ()
    "Scroll up one line."
    (interactive)
    (scroll-up 1))

  (global-set-key (kbd "<mouse-4>") #'radian-scroll-down)
  (global-set-key (kbd "<mouse-5>") #'radian-scroll-up))

;;; Clipboard
;; Based on https://gist.github.com/the-kenny/267162
;; Modified based on http://emacs.stackexchange.com/q/26471/12534

(unless (display-graphic-p)

  (setq radian--last-paste nil)

  (defun copy-from-osx ()
    (let ((copied-text (shell-command-to-string "pbpaste")))
      (unless (string= copied-text radian--last-paste)
        copied-text)))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc)))
    (setq radian--last-paste text))

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

(defmacro radian-register-dotfile (filename &optional keybinding)
  "Tell Radian about a dotfile.

This is best demonstrated by example. If NAME is
\".emacs.d/init.el\" then `radian-register-dotfile' will create
the interactive function `radian-find-init-el'. Calling that
function will invoke `find-file' on ~/.emacs.d/init.el.

If additionally KEYBINDING is \"e i\" then
`radian-register-dotfile' will use `global-set-key' to bind
`radian-find-init-el' to (kbd \"M-RET e i\")."
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (defun-name (make-symbol
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "radian-find-"
                        (replace-regexp-in-string
                         "[^a-z0-9]" "-"
                         bare-filename)))))
         (full-filename (concat "~/" filename))
         (docstring (format "Open %s in the current buffer."
                            full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (find-file ,full-filename))))
    (if keybinding
        (let* ((full-keybinding (concat "M-RET " keybinding))
               (set-key-form `(global-set-key (kbd ,full-keybinding)
                                              #',defun-name)))
          `(progn
             ,defun-form
             ,set-key-form))
      defun-form)))

;; Emacs
(radian-register-dotfile ".emacs.d/init.el" "e i")
(radian-register-dotfile ".emacs.d/init.before.local.el" "e b")
(radian-register-dotfile ".emacs.d/init.pre.local.el" "e p r") ; deprecated
(radian-register-dotfile ".emacs.d/init.post.local.el" "e p o") ; deprecated
(radian-register-dotfile ".emacs.d/init.local.el" "e l")

;; Git
(radian-register-dotfile ".gitconfig" "g c")
(radian-register-dotfile ".gitexclude" "g e")
(radian-register-dotfile ".gitconfig.local" "g l")

;; Leiningen
(radian-register-dotfile ".lein/profiles.clj" "l p")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Vim
(radian-register-dotfile ".config/nvim/init.vim" "v i")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.antigen.local" "z a")
(radian-register-dotfile ".zshrc.before.local" "z b")
(radian-register-dotfile ".zshrc.local" "z l")

(when (equal radian-customize-completion-mechanism 'helm-ido)
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
  (setq ido-auto-merge-work-directories-length -1))

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit this file with M-RET e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable Emacs' built-in version control handling. This improves
;; performance and disables some annoying warning messages and
;; prompts, especially regarding symlinks. See [1].
;;
;; [1]: http://stackoverflow.com/a/6190338/3538165
(setq vc-handled-backends nil)

;; Automatically create any nonexistent parent directories when
;; finding a file. If the buffer for the new file is killed without
;; being saved, then offer to delete the created directory or
;; directories.

(defun radian--find-file-automatically-create-directory
    (find-file filename &optional wildcards)
  "Advice for `find-file' that automatically creates the parent
directory (or directories) of the file being visited, if
necessary. It also sets a buffer-local variable so that the user
will be prompted to delete the newly created directories if they
kill the buffer without saving it."
  ;; The variable `dirs-to-delete' is a list of the directories that
  ;; will be automatically created by `make-directory'. We will want
  ;; to offer to delete these directories if the user kills the buffer
  ;; without saving it.
  (let ((dirs-to-delete ()))
    ;; If the file already exists, we don't need to worry about
    ;; creating any directories.
    (unless (file-exists-p filename)
      ;; It's easy to figure out how to invoke `make-directory',
      ;; because it will automatically create all parent directories.
      ;; We just need to ask for the directory immediately containing
      ;; the file to be created.
      (let* ((dir-to-create (file-name-directory filename))
             ;; However, to find the exact set of directories that
             ;; might need to be deleted afterward, we need to iterate
             ;; upward through the directory tree until we find a
             ;; directory that already exists, starting at the
             ;; directory containing the new file.
             (current-dir dir-to-create))
        ;; If the directory containing the new file already exists,
        ;; nothing needs to be created, and therefore nothing needs to
        ;; be destroyed, either.
        (while (not (file-exists-p current-dir))
          ;; Otherwise, we'll add that directory onto the list of
          ;; directories that are going to be created.
          (push current-dir dirs-to-delete)
          ;; Now we iterate upwards one directory. The
          ;; `directory-file-name' function removes the trailing slash
          ;; of the current directory, so that it is viewed as a file,
          ;; and then the `file-name-directory' function returns the
          ;; directory component in that path (which means the parent
          ;; directory).
          (setq current-dir (file-name-directory
                             (directory-file-name current-dir))))
        ;; Only bother trying to create a directory if one does not
        ;; already exist.
        (unless (file-exists-p dir-to-create)
          ;; Make the necessary directory and its parents.
          (make-directory dir-to-create 'parents))))
    ;; Call the original `find-file', now that the directory
    ;; containing the file to found exists. We make sure to preserve
    ;; the return value, so as not to mess up any commands relying on
    ;; it.
    (prog1 (funcall find-file filename wildcards)
      ;; If there are directories we want to offer to delete later, we
      ;; have more to do.
      (when dirs-to-delete
        ;; Since we already called `find-file', we're now in the buffer
        ;; for the new file. That means we can transfer the list of
        ;; directories to possibly delete later into a buffer-local
        ;; variable. But we pushed new entries onto the beginning of
        ;; `dirs-to-delete', so now we have to reverse it (in order to
        ;; later offer to delete directories from innermost to
        ;; outermost).
        (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
        ;; Now we add a buffer-local hook to offer to delete those
        ;; directories when the buffer is killed, but only if it's
        ;; appropriate to do so (for instance, only if the directories
        ;; still exist and the file still doesn't exist).
        (add-hook 'kill-buffer-hook
                  #'radian--kill-buffer-delete-directory-if-appropriate
                  'append 'local)
        ;; The above hook removes itself when it is run, but that will
        ;; only happen when the buffer is killed (which might never
        ;; happen). Just for cleanliness, we automatically remove it
        ;; when the buffer is saved. This hook also removes itself when
        ;; run, in addition to removing the above hook.
        (add-hook 'after-save-hook
                  #'radian--remove-kill-buffer-delete-directory-hook
                  'append 'local)))))

;; Add the advice that we just defined.
(advice-add #'find-file :around
            #'radian--find-file-automatically-create-directory)

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "If `radian--find-file-automatically-create-directory' created
the directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Remove `radian--kill-buffer-delete-directory-if-appropriate'
from `kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto revert

;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
;; We have to do this before turning on `auto-revert-mode' for the
;; change to take effect, unless we do it through
;; `customize-set-variable' (which is slow enough to show up in
;; startup profiling).
(setq auto-revert-interval 1)

;; Automatically reload files that were changed on disk, if they have
;; not been modified in Emacs since the last time they were saved.
(global-auto-revert-mode 1)

;; Only automatically revert buffers that are visible. This should
;; improve performance (because if you have 200 buffers open...). This
;; code is courtesy of @Tobias from Emacs Stack Exchange [1]. Thanks!
;;
;; Note that calling `global-auto-revert-mode' above triggers an
;; autoload for `autorevert', so there's no need to `require' it again
;; here.
;;
;; [1]: http://emacs.stackexchange.com/a/28899/12534

(defvar auto-revert-some-buffers-filter #'get-buffer-window
  "Filter for the output of `buffer-list' in `auto-revert-buffers'.
The function is called with a buffer as argument. It should
return a non-nil value if this buffer should really be
auto-reverted.")

(defun auto-revert-some-buffers-advice--buffer-list (ret)
  "Filter output of the first call of `buffer-list' in `auto-revert-buffers'.
This filter de-installs itself after this call."
  (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list)
  (cl-remove-if-not auto-revert-some-buffers-filter ret))

(defun auto-revert-some-buffers-advice (oldfun &rest args)
  "Filter the buffers to be auto-reverted through
`auto-revert-some-buffers-filter' (which see)."
  (let (ret)
    (if global-auto-revert-mode
        (unwind-protect
            (progn
              (advice-add #'buffer-list :filter-return #'auto-revert-some-buffers-advice--buffer-list)
              (setq ret (apply oldfun args)))
          ;; being over-protective
          (advice-remove #'buffer-list #'auto-revert-some-buffers-advice--buffer-list))
      (let ((old-auto-revert-buffer-list (cl-remove-if-not auto-revert-some-buffers-filter auto-revert-buffer-list))
            ;; Note: We interpret `auto-revert-remaining-buffers' as
            ;; transient effect and don't filter this list.
            deleted-buffers)
        (let ((auto-revert-buffer-list old-auto-revert-buffer-list))
          (setq ret (apply oldfun args))
          (setq deleted-buffers (cl-set-difference old-auto-revert-buffer-list auto-revert-buffer-list)))
        (setq auto-revert-buffer-list (cl-set-difference auto-revert-buffer-list deleted-buffers))))
    ret))

(advice-add #'auto-revert-buffers :around #'auto-revert-some-buffers-advice)

;; Since we automatically revert all visible buffers after one second,
;; there's no point in asking the user whether or not they want to it
;; when they find a file. This disables that prompt.
(setq revert-without-query '(".*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired

;; Make C-x C-j jump to the current file in Dired. This binding is
;; provided automatically by dired-x.el, which is automatically loaded
;; by Helm. But if Helm is not enabled, then we have to bind the key
;; manually. For some reason, the autoload for `dired-jump' does not
;; appear to work correctly, so we have to autoload the functions
;; explicitly.
(when (radian-package-disabled-p 'helm)
  (autoload 'dired-jump "dired-x")
  (autoload 'dired-jump-other-window "dired-x")
  (global-set-key (kbd "C-x C-j") #'dired-jump)
  (global-set-key (kbd "C-x 4 C-j") #'dired-jump-other-window))

;; Suppress the 'ls does not support --dired' warning. Doing this instead
;; of installing a dired-compatibile ls is much easier, although it may
;; cause problems with e.g. filenames that have leading spaces. If you
;; have a lot of filenames with leading spaces, though, you probably have
;; bigger problems ;)
(setq dired-use-ls-dired nil)

(defun radian--dired-mode-hook ()

  ;; Automatically update Dired buffers when things change on disk. Note
  ;; that `global-auto-revert-mode' does not cover non-file buffers like
  ;; Dired buffer, by default, so we have to make this change
  ;; separately. Might want to disable this, or at least make it so that
  ;; `auto-revert-mode' is only enabled while the user is actively
  ;; viewing the buffer.
  (auto-revert-mode 1)

  ;; Don't notify the user when a `dired' buffer is reverted, since
  ;; this will happen every time a file is saved in that directory.
  (setq-local auto-revert-verbose nil))

(add-hook 'dired-mode-hook #'radian--dired-mode-hook)

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
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
;;;; Navigation

;; When using M-. and friends, always prompt for the identifier (it
;; defaults to the identifier at point). This behavior is more
;; consistent and predictable than the default, which is to jump
;; immediately if there is a valid symbol at point.
(setq xref-prompt-for-identifier t)

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

;; Disable ElDoc globally. This prevents it from being enabled in the
;; minibuffer (ElDoc messages go into the mode line when you are in
;; the minibuffer, which looks very bad with Radian's default color
;; scheme).
(global-eldoc-mode -1)

;; Enable ElDoc when editing Lisps and using Lisp REPLs.

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)

(when (radian-package-enabled-p 'clojure-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(when (radian-package-enabled-p 'cider)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

;; Show ElDoc messages in the echo area immediately, instead of after
;; 1/2 a second.
(setq eldoc-idle-delay 0)

;; Always truncate ElDoc messages to one line. This prevents the echo
;; area from resizing itself unexpectedly when point is on a variable
;; with a multiline docstring.
(setq eldoc-echo-area-use-multiline-p nil)

;; Don't show ElDoc in the mode line.
(setq eldoc-minor-mode-string nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs Lisp

;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
;;
;; Before:
;;  (:foo bar
;;        :baz quux)
;;
;; After:
;;  (:foo bar
;;   :bar quux)
;;
;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; [2]: http://emacs.stackexchange.com/q/10230/12534
(defun lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

;; Add a keybinding (M-RET r) for reloading this file (init.el). This
;; is useful for when you have several instances of Emacs open and you
;; change something in your configuration, then later come back to an
;; old Emacs that was opened before you made the change. You can then
;; just press M-RET r to get the change into that instance.

(defun radian-reload-init (&optional upgrade)
  "Reload init.el. With prefix argument, upgrades packages."
  (interactive "P")
  (message "Reloading init.el...")
  (setq radian--inhibit-loading-quelpa (not upgrade))
  (radian-load-user-config "init.el")
  (message "Reloading init.el... done."))

(global-set-key (kbd "M-RET r") #'radian-reload-init)

;; Add a keybinding (C-c C-k) for evaluating a buffer of Elisp. This
;; is consistent with the keybindings for evaluating a buffer in CIDER
;; and Geiser.

(defun radian--bind-eval-buffer ()
  (local-set-key (kbd "C-c C-k") #'eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'radian--bind-eval-buffer)

;; Add keybindings (C-h C-f and C-h C-v) for jumping to the source of
;; Elisp functions and variables. Also, add a keybinding (C-h C-o)
;; that duplicates the functionality of M-., because the latter
;; command is often rebound by other major modes. Note that this
;; overrides the default bindings of C-h C-f to `view-emacs-FAQ' and
;; C-h C-o to `describe-distribution', but I think this is not very
;; important.

(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-h C-v") #'find-variable)
(global-set-key (kbd "C-h C-o") #'xref-find-definitions)

;; Show `lisp-interaction-mode' as "Lisp-Interaction" instead of "Lisp
;; Interaction" in the mode line.

(defun radian--rename-lisp-interaction-mode ()
  (setq mode-name "Lisp-Interaction"))

(add-hook 'lisp-interaction-mode-hook #'radian--rename-lisp-interaction-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C-like languages

;; Get rid of the submode indicators in the mode line. This transforms
;; e.g. "C++/l" into "C++". Since we are overriding a function
;; provided by `cc-mode', which is not initially loaded, we have to
;; make sure to do so *after* it is loaded and not before.
(eval-after-load 'cc-mode
  '(defun c-update-modeline ()))

;; Switch to a better indentation-and-braces style. This turns the
;; following code:
;;
;; if (condition)
;;   {
;;     statement;
;;   }
;;
;; Into this:
;;
;; if (condition) {
;;   statement;
;; }
(eval-after-load 'cc-mode
  '(progn
     (if (assoc 'other c-default-style)
         (setcdr (assoc 'other c-default-style)
                 "k&r")
       (push '(other . "k&r") c-default-style))
     (setq-default c-basic-offset 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bootstrap use-package

;; Load deprecated user-specific configuration file.
(radian-load-user-config "init.pre.local.el")

;; Add package repositories. GNU is the default repository; MELPA is
;; necessary to get most of the packages we are interested in. Note
;; that `package-initialize' appears to use the value of
;; `package-archives', so we place this declaration first.
(setq package-archives
      (or radian-customize-package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("melpa" . "https://melpa.org/packages/"))))

;; Initialize package.el. We can't use anything from package.el until
;; we do this.
(package-initialize)

;; If use-package is not installed, then make sure we know what the
;; latest version is, and install it.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Tell use-package to always install missing packages if necessary.
(setq use-package-always-ensure t)

;; Tell use-package to always load packages lazily unless told
;; otherwise.
(setq use-package-always-defer t)

;; Add an advice to `use-package' to make it so that
;; `radian-quelpa-overrides' has an effect.

(defun radian--use-package-allow-quelpa-overrides (use-package name &rest args)
  "Allow the value of `radian-quelpa-overrides' to have an effect
on the `:quelpa' keyword.

Note that this advice is not implemented very intelligently, so
it will only work correctly if `:quelpa' is specified as the
first keyword in the `use-package' form (except that it can be
after `:dependencies')."
  ;; First we want to check if there's an entry for the package in
  ;; `radian-quelpa-overrides'.
  (let ((association (assoc name radian-quelpa-overrides)))
    (if association
        ;; If there is an association, we'll extract the recipe.
        (let ((recipe (cdr association)))
          (if recipe
              ;; If the recipe is non-nil it means the user wants to
              ;; use quelpa for the package.
              (if (equal (car args) :quelpa)
                  (if (keywordp (cadr args))
                      ;; This happens for (use-package name :quelpa
                      ;; :config ...).
                      (apply use-package name :quelpa recipe (cdr args))
                    ;; This happens for (use-package name
                    ;; :quelpa (recipe) :config ...).
                    (apply use-package name :quelpa recipe (cddr args)))
                ;; This happens for (use-package name :config ...).
                (apply use-package name :quelpa recipe args))
            ;; If the recipe is nil it means the user doesn't want to
            ;; use quelpa for the package.
            (if (equal (car args) :quelpa)
                (if (keywordp (cadr args))
                    ;; This happens for (use-package name :quelpa
                    ;; :config ...).
                    (apply use-package name (cdr args))
                  ;; This happens for (use-package name
                  ;; :quelpa (recipe) :config ...).
                  (apply use-package name (cddr args)))
              ;; This happens for (use-package name :config ...), in
              ;; which case there's nothing that needs to be changed.
              (apply use-package name args))))
      ;; If there's no special behavior requested, we can just pass
      ;; control to `use-package'.
      (apply use-package name args))))

(advice-add 'use-package
            :around 'radian--use-package-allow-quelpa-overrides
  '((:depth . -1)))

;; Add an advice to `use-package' to make it so that disabled packages
;; are not loaded. See the docstring for more information.

(defun radian--use-package-add-dependencies (use-package name &rest args)
  "Only initialize and load a package if it is enabled (according
to `radian-package-enabled-p'). If the first argument specified
after the package name is :dependencies, then also require any
packages specified in the list of symbols immediately
following :dependencies to be enabled.

Note that this advice is not implemented very intelligently, so
it will only work correctly if `:dependencies' is specified as
the first keyword in the `use-package' form."
  (when (radian-package-enabled-p name)
    (if (equal (car args) :dependencies)
        (when (cl-every 'radian-package-enabled-p (cadr args))
          (apply use-package name (cddr args)))
      (apply use-package name args))))

(advice-add 'use-package
            :around 'radian--use-package-add-dependencies
  '((:depth . -2)))

;; Add an advice to automatically refresh the package list before
;; installing a package (but only once). This has an effect because
;; `use-package' eventually calls through to `package-install'.
;;
;; If we don't do this, then we might get errors when you try to
;; install a package with `use-package' without first manually running
;; `package-refresh-contents'. Specifically, this will happen if the
;; package you are trying to install has been updated since the last
;; time `package-refresh-contents' was run (or, by default, the first
;; time you ever tried to install a package).
;;
;; The issue is discussed at [1], but the solution used here is
;; modified so that it does not require `cl' (currently, Radian only
;; loads `cl-lib').
;;
;; [1]: https://github.com/jwiegley/use-package/issues/256

(defun radian--package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'radian--package-install-refresh-contents))

(advice-add 'package-install :before 'radian--package-install-refresh-contents)

;; Load deprecated user-specific configuration file.
(radian-load-user-config "init.post.local.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Package management

;; Library for fetching Elisp packages in many different ways and
;; building them from source (client-side MELPA).
(use-package quelpa
  :config

  ;; Tell quelpa not to update MELPA every time Emacs is started.
  (setq quelpa-update-melpa-p nil)

  ;; Automatically upgrade packages. This does not actually take
  ;; effect if the call to quelpa comes from a `:quelpa' keyword in a
  ;; `use-package' form and
  ;; `quelpa-use-package-inhibit-loading-quelpa' is non-nil (as it is
  ;; by default in Radian). It only affects what happens when
  ;; `radian-reload-init' is called with a prefix argument; in that
  ;; case it allows packages to be upgraded, as desired.
  (setq quelpa-upgrade-p t))

;; Add a :quelpa keyword to `use-package', which allows installing
;; packages with quelpa. We have a bit of a chicken and egg problem
;; here. We don't want to load quelpa during startup, because that's
;; slow. But quelpa-use-package loads quelpa eagerly. So we have to
;; use my fork of quelpa-use-package, which fixes that. But installing
;; from a fork requires quelpa. But we don't want to load it eagerly.
;; So we have to use my fork of quelpa-use-package...
;;
;; The solution here is to use two separate code paths. On first
;; install, quelpa will be loaded eagerly and used to install
;; quelpa-use-package. But after that, quelpa-use-package can install
;; itself, thus solving the problem of quelpa being loaded eagerly.
;;
;; (Note that, as long as we are using my fork, loading
;; quelpa-use-package eagerly is not a problem. In fact, it's required
;; because we need to activate its advice before processing
;; use-package forms that might contain `:quelpa' keywords.)
;;
;; To maintain reverse compatibility, my fork of quelpa-use-package
;; only lazily loads quelpa if the variable
;; `quelpa-use-package-inhibit-loading-quelpa' is non-nil (it is nil
;; by default). So we have to set it here. But we also want to allow
;; for disabling that optimization in the case that
;; `radian-reload-init' is called with a prefix argument. To solve
;; this problem we use `defvar', which will only set
;; `radian--inhibit-loading-quelpa' to `t' if that variable has not
;; already been defined. That way, we can override the value of
;; `radian--inhibit-loading-quelpa' in `radian-reload-init' and have
;; that value be transferred to
;; `quelpa-use-package-inhibit-loading-quelpa' below, while
;; maintaining a default value of `t' when Emacs is first started.

(defvar radian--inhibit-loading-quelpa t)
(setq quelpa-use-package-inhibit-loading-quelpa
      radian--inhibit-loading-quelpa)

(if (require 'quelpa-use-package nil 'noerror)
    (progn
      ;; If I don't include this then I get an error about eager
      ;; macro-expansion because the `:quelpa' keyword in the next
      ;; use-package form is not defined for the compiler.
      (eval-when-compile
        (require 'quelpa-use-package))
      (use-package quelpa-use-package
        :quelpa (quelpa-use-package
                 :fetcher github
                 :repo "raxod502/quelpa-use-package")
        :demand t))
  (quelpa '(quelpa-use-package
            :fetcher github
            :repo "raxod502/quelpa-use-package"))
  (require 'quelpa-use-package))

;; Allow the :quelpa keyword to work even when
;; `use-package-always-ensure' is enabled. See [1].
;;
;; [1]: https://github.com/quelpa/quelpa-use-package
(quelpa-use-package-activate-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Managing Emacs

;; Allows you to restart Emacs from within Emacs.
(use-package restart-emacs
  :bind (;; Add a binding that parallels C-x C-c, so that it is very
         ;; easy and natural to restart Emacs from within Emacs.
         "C-x M-c" . restart-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Completion systems

;; Sorts M-x completions by usage. Automatically used by Ivy if
;; available.
(use-package smex
  :bind (;; Use smex for M-x.
         ("M-x" . smex)))

;; Provides a general-purpose completion and narrowing mechanism, and
;; enhanced versions of many standard Emacs commands that use it.
(use-package helm
  :demand t
  :config

  ;; Use Helm mode for many standard Emacs commands.
  (helm-mode 1)

  ;; Fix the unreadable default color for the prefix argument in the
  ;; Helm M-x buffer.
  (when radian-customize-tweak-colors
    (set-face-foreground 'helm-prefarg "#FFFF66"))

  ;; Use fuzzy matching for Helm.
  (setq helm-mode-fuzzy-match t)

  ;; Get rid of the awful background color for buffers corresponding to files
  ;; modified outside of Emacs.
  (when radian-customize-tweak-colors
    (set-face-background 'helm-buffer-saved-out nil))

  :bind (;; Use Helm mode for M-x.
         ("M-x" . helm-M-x))
  ;; Note that Helm has `helm-mode-line-string', but this only affects
  ;; what is shown in the mode line for a Helm buffer.
  :diminish helm-mode)

;; Provides enhanced versions of the Projectile commands that use
;; Helm.
(use-package helm-projectile
  :dependencies (helm)
  :demand t
  :config

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

;; Provides an enhanced version of smex that uses Helm for completion.
(use-package helm-smex
  :dependencies (helm smex)
  :bind (;; Use helm-smex for M-x.
         ("M-x" . helm-smex)))

;; Provides intelligent fuzzy matching and sorting mechanisms that
;; can be used by various other packages, including Ivy.
(use-package flx)

;; Provides a general-purpose completion mechanism.
(use-package ivy
  :demand t
  :config

  ;; Use Ivy for `completing-read'.
  (ivy-mode 1)

  ;; Use fuzzy matching for Ivy, powered by flx, but not for Swiper
  ;; (because fuzzy matching is typically not desired in grep-style
  ;; searches, just plain 'ol regex).
  ;;
  ;; [1]: http://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  ;; Increase the maximum number of candidates that will be sorted
  ;; using `flx'. (If there are more candidates, they will be sorted
  ;; alphabetically.) The default is 200, which means `flx' is almost
  ;; never used. Setting it too high (e.g. 10000) causes lag. This
  ;; seems to be a good compromise (for example, @PythonNut uses it,
  ;; see [1]). A better solution based on the code in [2] is pending.
  ;;
  ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
  ;; [2]: https://github.com/abo-abo/swiper/issues/207#issuecomment-140439385
  (setq ivy-flx-limit 2000)

  ;; Don't automatically insert a "^" character when starting an Ivy
  ;; completion. This has the effect of making it so that matches
  ;; are not required to start at the beginning of the symbol being
  ;; matched, by default.
  (setq ivy-initial-inputs-alist
        '((t . "")))

  ;; Allow the C-c C-r binding defined below to work in `sh-mode',
  ;; where it is bound by default to `sh-repeat'.
  (eval-after-load 'sh-script
    '(define-key sh-mode-map (kbd "C-c C-r") nil))

  :bind (;; Add the README-suggested keybinding for resuming the last
         ;; completion session.
         ("C-c C-r" . ivy-resume))
  :diminish ivy-mode)

;; Provides enhanced versions of many command Emacs commands that
;; use Ivy for completion, and adds a few new commands (such as
;; `counsel-git-ag').
(use-package counsel
  :config

  ;; If there is a valid file at point, pre-select in C-x C-f.
  (setq counsel-find-file-at-point t)

  :bind (;; Use Counsel for common Emacs commands.
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h l" . counsel-load-library)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-x 8 RET" . counsel-unicode-char)

         ;; Introduce a few new commands that use Counsel. The
         ;; bindings are suggested by the README [1].
         ;;
         ;; [1]: https://github.com/abo-abo/swiper
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)

         ;; After you have pressed M-:, you can use C-r to select a
         ;; previous entry using Counsel.
         :map read-expression-map
         ("C-r" . counsel-expression-history)

         ;; Improve the Ivy keybindings, making them more predictable.
         ;;
         ;; There are essentially three "accept" actions in Ivy: RET,
         ;; TAB, and C-j. Sometimes two or even all three of them do
         ;; the same thing, but there is one situation when there are
         ;; three different behaviors that might be desirable. This is
         ;; when you are in `counsel-find-file', and you have typed in
         ;; "foo", and there is a folder called "foobar" in the same
         ;; directory.
         ;;
         ;; In this situation, RET will `dired' the folder "foobar",
         ;; TAB will move Ivy into the "foobar" folder, and C-j will
         ;; create a new file called "foo". But only with the
         ;; keybindings below! The default behavior is different, and
         ;; weird. I think this is the most intuitive and useful.
         ;;
         ;; As always, TAB is for terminal Emacs and <tab> is for
         ;; windowed Emacs.
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("<tab>" . ivy-alt-done)
         ("C-j" . ivy-immediate-done)))

;; Provides enhanced versions of the Projectile commands that use Ivy.
(use-package counsel-projectile
  :init

  ;; Define the keybindings for counsel-projectile. The next two
  ;; functions are adapted from the definition of
  ;; `counsel-projectile-toggle'. Copying them out here means that we
  ;; can call them without loading `counsel-projectile', so that the
  ;; keybindings are defined but everything else is left autoloaded.
  ;; We use plain `define-key' calls because there was difficulty
  ;; getting use-package's `:bind' to work correctly, see [1].
  ;;
  ;; [1]: https://github.com/jwiegley/use-package/issues/413

  (defun radian--counsel-projectile-commander-bindings ()
    (def-projectile-commander-method ?f
      "Find file in project."
      (counsel-projectile-find-file))
    (def-projectile-commander-method ?d
      "Find directory in project."
      (counsel-projectile-find-dir))
    (def-projectile-commander-method ?b
      "Switch to project buffer."
      (counsel-projectile-switch-to-buffer))
    (def-projectile-commander-method ?A
      "Search project files with ag."
      (counsel-projectile-ag))
    (def-projectile-commander-method ?s
      "Switch project."
      (counsel-projectile-switch-project)))

  (defun radian--counsel-projectile-on ()
    (when (eq projectile-switch-project-action #'projectile-find-file)
      (setq projectile-switch-project-action #'counsel-projectile))
    (define-key projectile-mode-map [remap projectile-find-file]
      #'counsel-projectile-find-file)
    (define-key projectile-mode-map [remap projectile-find-dir]
      #'counsel-projectile-find-dir)
    (define-key projectile-mode-map [remap projectile-switch-project]
      #'counsel-projectile-switch-project)
    (define-key projectile-mode-map [remap projectile-ag]
      #'counsel-projectile-ag)
    (define-key projectile-mode-map [remap projectile-switch-to-buffer]
      #'counsel-projectile-switch-to-buffer)
    (radian--counsel-projectile-commander-bindings))

  (eval-after-load 'projectile '(radian--counsel-projectile-on)))

;; Provides an enhanced version of Isearch that uses Ivy to display
;; a preview of the results.
(use-package swiper
  :bind (;; Use Swiper for Isearches.
         ("C-s" . swiper)
         ("C-r" . swiper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: User interface

;; Provides an easy way to change the display of minor modes in the
;; mode line.
(use-package diminish
  :demand t
  :config

  ;; Don't show `abbrev-mode' in the mode line.
  (diminish 'abbrev-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Window management

;; Provides simple commands to mirror, rotate, and transpose Emacs
;; windows.
(use-package transpose-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Text editing

;; Keeps parentheses balanced at all times, and provides structural
;; navigation and editing commands for s-expressions.
(use-package paredit
  :init

  ;; Enable Paredit when editing Lisps and using Lisp REPLs.

  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)

  (when (radian-package-enabled-p 'clojure-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode))

  (when (radian-package-enabled-p 'cider)
    (add-hook 'cider-repl-mode-hook #'paredit-mode))

  (when (radian-package-enabled-p 'geiser)
    (add-hook 'geiser-repl-mode-hook #'paredit-mode)))

;; Keeps indentation correct at all times.
(use-package aggressive-indent
  :demand t
  :config

  ;; Enable Aggressive Indent everywhere, except the modes in
  ;; `aggressive-indent-excluded-modes'.
  (global-aggressive-indent-mode 1)

  ;; Disable Aggressive Indent in Re-Builder mode. I don't think it
  ;; does anything in this mode, and it shadows the C-c C-q binding
  ;; provided by Re-Builder (so you can't quit!).
  (add-to-list 'aggressive-indent-excluded-modes 'reb-mode)

  ;; Disable Aggressive Indent in `vimrc-mode', because `vimrc-mode'
  ;; does not provide any information about indentation (so
  ;; `aggressive-indent' just deletes all indentation, which is not
  ;; helpful).
  (add-to-list 'aggressive-indent-excluded-modes 'vimrc-mode)

  :diminish (aggressive-indent-mode . "AggrIndent"))

;; Provides undo/redo commands that are both more intuitive and more
;; powerful than the Emacs defaults. Allows you to visualize the
;; undo/redo tree, which uses a branching model to ensure that you can
;; never lose changes.
(use-package undo-tree
  :demand t
  :config

  ;; Enable Undo Tree everywhere.
  (global-undo-tree-mode 1)

  ;; Make undo history persistent between Emacs sessions. Don't rely
  ;; on this too much if your files tend to change outside of Emacs
  ;; (e.g. if you use Git).
  (setq undo-tree-auto-save-history t)

  ;; Put all the undo information in a single directory, instead of in
  ;; each file's directory.
  (setq undo-tree-history-directory-alist
	`(("." . ,(concat user-emacs-directory "undo-tree"))))

  ;; Don't show Undo Tree in the mode line.
  (setq undo-tree-mode-lighter nil)

  ;; Suppress the message saying that the undo history file was
  ;; saved (because this happens every single time you save a file).

  (defun radian--undo-tree-suppress-undo-history-saved-message
      (undo-tree-save-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-save-history args)))

  ;; Suppress the message saying that the undo history could not be
  ;; loaded because the file changed outside of Emacs.

  (defun radian--undo-tree-suppress-buffer-modified-message
      (undo-tree-load-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  (advice-add #'undo-tree-load-history :around
              #'radian--undo-tree-suppress-buffer-modified-message)

  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is bound
         ;; to C-_ and C-/, and `undo-tree-redo' is bound to M-_. It's
         ;; logical to also bind M-/ to `undo-tree-redo'. This overrides the
         ;; default binding of M-/, which is to `dabbrev-expand'.
         ("M-/" . undo-tree-redo)))

;; Provides a powerful autocomplete mechanism that integrates with
;; many different sources of completions.
(use-package company
  :demand t
  :config

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

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Prevent Company completions from being lowercased in the
  ;; completion menu. This has only been observed to happen for
  ;; comments and strings in Clojure.
  (setq company-dabbrev-downcase nil)

  ;; Company mode overrides standard REPL bindings for M-p and M-n
  ;; when the completions menu is visible. Prevent this, but only in
  ;; REPL modes.

  (defun radian--unbind-company-navigation-keys ()
    (make-local-variable 'company-active-map)
    (setq company-active-map (copy-tree company-active-map))
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "M-n") nil))

  (when (radian-package-enabled-p 'cider)
    (add-hook 'cider-repl-mode-hook #'radian--unbind-company-navigation-keys))

  (when (radian-package-enabled-p 'geiser)
    (add-hook 'geiser-repl-mode-hook #'radian--unbind-company-navigation-keys))

  ;; We want pressing RET to trigger a Company completion only if the
  ;; user has interacted explicitly with Company. The only way I can
  ;; find to do this is to define what is called an "extended menu
  ;; item" and bind it to RET in the `company-active-map'. Extended
  ;; menu items appear to have the ability (by returning nil) to tell
  ;; Emacs to perform whatever action the keybinding just pressed
  ;; would otherwise perform. Other solutions I have found, such as
  ;; [1], do not work correctly if the key that was pressed is
  ;; supposed to be automatically translated by Emacs (for instance,
  ;; C-c C-K to C-c C-k; or, more relevantly to Company, <return> to
  ;; RET). I found out about the option of using an extended menu item
  ;; from [2], which led to [3].
  ;;
  ;; [1]: https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;; [2]: http://stackoverflow.com/a/22863701/3538165
  ;; [3]: http://stackoverflow.com/a/22863701/3538165
  (setq radian--company-complete-if-explicit
        `(menu-item nil company-complete
                    :filter ,(lambda (cmd)
                               (when (company-explicit-action-p)
                                 cmd))))

  ;; Make RET trigger a completion if and only if the user
  ;; has explicitly interacted with Company. Note that
  ;; <return> is for windowed Emacs and RET is for
  ;; terminal Emacs.
  (define-key company-active-map (kbd "<return>")
    radian--company-complete-if-explicit)
  (define-key company-active-map (kbd "RET")
    radian--company-complete-if-explicit)

  :bind (:map company-active-map
         ;; Make TAB always complete the current selection. Note
         ;; that <tab> is for windowed Emacs and TAB is for
         ;; terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil))
  :diminish company-mode)

;; Sorts Company suggestions by usage, persistent between sessions.
(use-package company-statistics
  :dependencies (company)
  :demand t
  :config

  ;; Disable the message that is normally printed when Company
  ;; Statistics loads its statistics file from disk.
  (defun company-statistics--load ()
    (load company-statistics-file 'noerror 'nomessage 'nosuffix))

  ;; Enable Company Statistics.
  (company-statistics-mode 1))

;; Allows the expansion of user-defined abbreviations into fillable
;; templates. This is used by clj-refactor for some of its
;; refactorings.
(use-package yasnippet
  :diminish yas-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Navigation

;; Jump to frequently used directories by name.
(use-package fasd
  :demand t
  :config

  ;; Use completion from a list of fasd candidates for
  ;; `fasd-find-file', instead of just prompting for an input.
  (setq fasd-enable-initial-prompt nil)

  ;; Use Ivy (or Helm, if `radian-customize-completion-mechanism' is
  ;; set to `helm-ido') for completion, instead of `grizzl'.
  (setq fasd-completing-read-function nil)

  ;; Make it so that Emacs file-finding updates the fasd database.
  (global-fasd-mode 1)

  :bind (;; Add a keybinding for using the functionality of `fasd'.
         ("C-c f" . fasd-find-file)))

;; Provides commands to quickly navigate within and between
;; "projects".
(use-package projectile
  :demand t
  :config

  ;; Enable Projectile everywhere.
  (projectile-global-mode 1)

  ;; Don't show Projectile in the mode line. (Radian already adds a
  ;; custom indicator for the current project, so there's no need to
  ;; show the mode separately.)
  (setq projectile-mode-line nil))

;; Allows you to jump to any particular occurrence of a character
;; visible on-screen.
(use-package ace-jump-mode
  :bind (;; Create a keybinding for ace-jump-mode. Clojure mode already binds
         ;; C-c SPC, the recommended keybinding, to `clojure-align', so use
         ;; C-c C-SPC instead.
         ("C-C C-SPC" . ace-jump-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Clojure

;; Provides indentation and syntax highlighting for Clojure code.
(use-package clojure-mode
  :config

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
                  (:require 0)
                  (:use 0)))
    (put-clojure-indent (car spec) (cdr spec)))

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
      (lisp-indent-line)))

  :bind (;; Make sure electric indentation *always* works. For some
         ;; reason, if this is omitted, electric indentation works most
         ;; of the time, but it fails inside Clojure docstrings. (TAB
         ;; will add the requisite two spaces, but you shouldn't have to
         ;; do this manually after pressing RET.) I'd like to find a more
         ;; elegant solution to this problem. See issue #2.
         ;;
         ;; <return> is for windowed Emacs; RET is for terminal Emacs.
         :map clojure-mode-map
         ("<return>" . newline-and-indent)
         ("RET" . newline-and-indent)))

;; Provides Clojure and ClojureScript REPL integration, including
;; documentation and source lookups, among many other features.
(use-package cider
  :dependencies (clojure-mode)
  :config

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

  (defun radian--dump-nrepl-server-log ()
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  (add-hook 'cider-connected-hook #'radian--dump-nrepl-server-log)

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
  ;; up. This triggers a `completing-read', which is disorienting. So
  ;; we reset TAB to its default functionality (i.e. indent only) in
  ;; the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursur to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior. For a historical perspective see [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/cider/issues/1872
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

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
  (figwheel-sidecar.repl-api/cljs-repl))")

  ;; Don't show CIDER in the mode line.
  (setq cider-mode-line nil)

  :bind (;; Allow usage of the C-c M-j and C-c M-J shortcuts everywhere.
         ("C-c M-j" . cider-jack-in)
         ("C-c M-J" . cider-jack-in-clojurescript)))

;; Makes Emacs into a real Clojure IDE by providing a mountain of
;; automated refactoring tools.
(use-package clj-refactor
  :dependencies (clojure-mode cider yasnippet)
  :init

  ;; Enable clj-refactor in Clojure buffers. This is adapted from the
  ;; clj-refactor README [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/clj-refactor.el

  (defun radian--enable-clj-refactor-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c RET"))

  (add-hook 'clojure-mode-hook #'radian--enable-clj-refactor-mode)

  :diminish clj-refactor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Racket

;; Provides Racket REPL integration, including documentation and
;; source lookups. Basically CIDER for Racket.
(use-package geiser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: C-like languages

;; General support for C, C++, and Objective-C based on libclang.
(use-package irony
  :init

  ;; Enable Irony for C, C++, and Objective-C files.
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)

  :config

  ;; Taken from the README of irony-mode [1]. If it's not present,
  ;; company-irony seems to only be able to work in a single buffer.
  ;;
  ;; [1]: https://github.com/Sarcasm/irony-mode
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  ;; Automatically install irony-server if it is missing. irony-server
  ;; is necessary for Irony to work at all!
  (unless (irony--locate-server-executable)
    ;; The following `let' is copied from the definition of
    ;; `irony-install-server'. A better solution would be to
    ;; dynamically bind `irony--install-server-read-command' to
    ;; `identity' and then just use `call-interactively' to invoke
    ;; `irony-install-server' with no arguments, but I don't know how
    ;; to do this.
    (let ((command
           (format
            (concat "%s %s %s && %s --build . "
                    "--use-stderr --config Release --target install")
            (shell-quote-argument irony-cmake-executable)
            (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                          (expand-file-name
                                           irony-server-install-prefix)))
            (shell-quote-argument irony-server-source-dir)
            (shell-quote-argument irony-cmake-executable))))
      (irony-install-server command)))

  :diminish irony-mode)

;; Company integration for Irony.
(use-package company-irony
  :dependencies (company irony)
  :init

  ;; Tell Company about company-irony. For some reason, this appears
  ;; to cause Irony to be eagerly loaded. So we only do it after Irony
  ;; has been loaded.

  (defun radian--set-up-company-irony ()
    ;; Don't add `company-irony' as a backend if we have
    ;; already added `company-irony-c-headers'. The backend
    ;; for `company-irony-c-headers' is a grouped backend,
    ;; so it accounts for both, and if we add
    ;; `company-irony' it will take precedence and inhibit
    ;; the functionality of `company-irony-c-headers'.
    (unless (member '(company-irony-c-headers
                      company-irony)
                    company-backends)
      (add-to-list 'company-backends 'company-irony)))

  (add-hook 'irony-mode-hook #'radian--set-up-company-irony))

;; Extends company-irony to work for completing #includes.
(use-package company-irony-c-headers
  :dependencies (company irony company-irony)
  :init

  ;; Tell Company about company-irony-c-headers. As per the README
  ;; [1], we must add a grouped backend for things to work properly.
  ;;
  ;; [1]: https://github.com/hotpxl/company-irony-c-headers

  (defun radian--set-up-company-irony-c-headers ()
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony)))

  (add-hook 'irony-mode-hook #'radian--set-up-company-irony-c-headers))

;; ElDoc integration for Irony.
(use-package irony-eldoc
  :dependencies (irony)
  :quelpa (irony-eldoc
           :fetcher github
           :repo "raxod502/irony-eldoc")
  :init

  ;; Enable irony-eldoc.
  (add-hook 'irony-mode-hook #'irony-eldoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: VimScript

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Markdown

;; Provides syntax highlighting, indentation, and editing commands for
;; Markdown files.
(use-package markdown-mode)

;; Provides the `markdown-toc-generate-toc' command to generate a
;; table of contents for a Markdown file.
(use-package markdown-toc
  :dependencies (markdown-mode)
  :config

  ;; Remove the header inserted before the table of contents. If you want a
  ;; header, just add one before the "markdown-toc start" comment -- this way,
  ;; you can have different header styles in different documents.
  (setq markdown-toc-header-toc-title ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Version control

;; Allows editing Git commit messages from the command line (i.e. with
;; emacs or emacsclient as your core.editor).
(use-package git-commit
  :init

  ;; Enable the functionality of the git-commit package.
  ;; Unfortunately, doing this via `global-git-commit-mode' is *very
  ;; slow* because it requires `git-commit' to be loaded eagerly,
  ;; which means that `log-edit' and `with-editor' need to be loaded
  ;; eagerly, which adds up to almost 1/4 second at startup. So we
  ;; have to copy the setup code from `git-commit' in order to avoid
  ;; loading the package.

  (defconst radian--git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|BRANCH_DESCRIPTION\\)\\'")

  (defun radian--git-commit-setup-check-buffer ()
    (when (and buffer-file-name
               (string-match-p radian--git-commit-filename-regexp
                               buffer-file-name))
      ;; The `git-commit-setup' function is regrettably not
      ;; autoloaded, so we have to `require' the package manually.
      (require 'git-commit)
      (git-commit-setup)))

  (add-hook 'find-file-hook #'radian--git-commit-setup-check-buffer)

  :config

  ;; Wrap summary at 50 characters as per [1].
  ;;
  ;; [1]: http://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load packages added via deprecated API

(unless (cl-every 'package-installed-p radian-packages)
  (package-refresh-contents)
  (dolist (package radian-packages)
    (unless (package-installed-p package)
      (package-install package))))

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

(defun radian--mode-line-projectile-project ()
  "Returns a construct for the mode line that shows the current
Projectile project between brackets, with the appropriate spacing
on the left. Returns the empty string if Projectile is not
enabled or if the user is not in a Projectile project."
  (when (radian-package-enabled-p 'projectile)
    (let ((name (projectile-project-name)))
      (unless (equal name "-")
        (concat " [" name "]")))))

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
               ;; Show the current Projectile project.
               '(:eval (radian--mode-line-projectile-project))
               ;; Show the active major and minor modes.
               "  "
               mode-line-modes))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load user-specific configuration file (2 of 2)

(radian-load-user-config "init.local.el")
