;; To see the outline of this file, run M-x occur with a query of four
;; semicolons followed by a space.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User-specific configuration variables

;;; Here we are using the defvar-nil-setq pattern described in [1],
;;; which makes it so that changes to these declarations will be
;;; picked up by a reload of init.el (M-P r).
;;;
;;; [1]: http://ergoemacs.org/emacs/elisp_defvar_problem.html

(defgroup radian nil
  "Customize your Radian Emacs experience"
  :group 'emacs)

(defcustom radian-color-theme 'wombat
  "Specifies the color theme used by Radian.

You can use anything listed by `custom-available-themes'.
Specifying `doom-one' or `doom-molokai' will cause the
appropriate package to be installed automatically.

If you wish to use your own color theme, you can set this to
nil."
  :group 'radian
  :type 'symbol)

(defcustom radian-operating-system
  (pcase system-type
    ('darwin 'macos)
    ((or 'ms-dos 'windows-nt 'cygwin) 'windows)
    (_ 'linux))
  "Specifies the operating system.
This can be `macos', `linux', or `windows'. Normally this is
automatically detected and does not need to be changed.")

(defmacro radian-with-operating-system (os &rest body)
  "If the operating system is OS, eval BODY.
See `radian-operating-system' for the possible values of OS,
which should not be quoted."
  (declare (indent 1))
  `(when (eq radian-operating-system ',os)
     ,@body))

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
;;; up by a reload of init.el (M-P r).
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Function for loading user-specific configuration files

;;; The following function is used two times in init.el to load
;;; user-specific configuration files (init.before.local.el and
;;; init.local.el).

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
;;;; Bootstrap package management system

;; We aren't using package.el, but Emacs will initialize it for us if
;; we don't tell it not to.
(setq package-enable-at-startup nil)

(let ((repos-dir (concat user-emacs-directory "straight/repos/")))
  (unless (file-exists-p (concat repos-dir "straight.el"))
    (make-directory repos-dir 'parents)
    (message "Cloning repository \"straight.el\"...")
    (unless (= 0 (call-process
                  "git" nil nil nil "clone" "--recursive"
                  "https://github.com/raxod502/straight.el.git"
                  (expand-file-name
                   (concat repos-dir "straight.el"))))
      (error "Could not clone straight.el"))
    (message "Cloning repository \"straight.el\"...done"))
  (load (concat repos-dir "straight.el/bootstrap.el")
        nil 'nomessage))

(straight-use-package '(use-package
                         :fetcher github
                         :repo "raxod502/use-package"
                         :branch "wip"
                         :files ("use-package.el")))

;; Tell use-package to always load packages lazily unless told
;; otherwise.
(setq use-package-always-defer t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Libraries

;; Future-proof your Emacs Lisp customizations.
(use-package el-patch
  :recipe (:fetcher github
           :repo "raxod502/el-patch")
  :demand t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Startup

;; Disable the "For information about GNU Emacs..." message at startup,
;; for *all* users.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Disable the *About GNU Emacs* buffer at startup, and go straight for
;; the scratch buffer. This is especially useful because Projectile won't
;; work in the startup buffer, which is annoying.
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Appearance

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the scroll bars.
(scroll-bar-mode -1)

;; Disable the contextual menu that pops up when you right-click.
(global-set-key (kbd "<C-down-mouse-1>") nil)

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

;; Change the highlight color used by various things from yellow to
;; blue.
(when (equal radian-color-theme 'leuven)
  (set-face-background 'highlight "#B1EAFC"))

;; Get rid of the underline for the currently highlighted match in an
;; Isearch or query replace.
(when (equal radian-color-theme 'leuven)
  (set-face-underline 'isearch nil))

;; The default highlight color for Isearches is quite dark and makes
;; it hard to read the highlighted text. Change it to a nice light
;; blue, and get rid of the distracting underline.
(when (equal radian-color-theme 'leuven)
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

(radian-with-operating-system macos
  (unless (display-graphic-p)

    (setq radian--last-paste nil)

    (defun copy-from-osx ()
      (let ((copied-text (shell-command-to-string "pbpaste")))
        (unless (string= copied-text radian--last-paste)
          copied-text)))

    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc)))
      (setq radian--last-paste text))

    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx)))

;; If you have something on the system clipboard, and then kill something in
;; Emacs, then by default whatever you had on the system clipboard is gone
;; and there is no way to get it back. Setting the following option makes it
;; so that when you kill something in Emacs, whatever was previously on the
;; system clipboard is pushed into the kill ring. This way, you can paste it
;; by doing C-y M-y.
(setq save-interprogram-paste-before-kill t)

;;; PATH
;; On macOS, the $PATH is not set correctly for GUI applications. So
;; we have to set it ourselves.
(radian-with-operating-system macos
  ;; This conditional is from the README [1] of exec-path-from-shell,
  ;; which we are not using for performance reasons.
  ;;
  ;; [1]: https://github.com/purcell/exec-path-from-shell
  (when (memq window-system '(mac ns))
    (with-temp-buffer
      ;; See "man path_helper".
      (call-process "/usr/libexec/path_helper" nil t nil "-s")
      (goto-char (point-min))
      (search-forward-regexp "PATH=\"\\(.+\\)\"; export PATH;")
      (let ((path (match-string 1)))
        (setenv "PATH" path)
        ;; These two setq's are from the code of exec-path-from-shell.
        (setq eshell-path-env path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
      ;; For some reason, path_helper doesn't always seem to report a
      ;; MANPATH, so we won't insist on getting one.
      (when (search-forward-regexp "MANPATH=\"\\(.+\\)\"; export MANPATH;"
                                   nil 'noerror)
        (setenv "MANPATH" (match-string 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Commands

;; Enable all disabled commands.
(setq disabled-command-function nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Finding files

;; The following code adds keybindings for jumping to the various
;; dotfiles set up by Radian. These all begin with <M-P e> and are
;; designed to be mnemonic, as in <M-P e e i> standing for "[e]dit
;; [e]macs [i]nit.el".

(defmacro radian-register-dotfile (filename &optional keybinding)
  "Tell Radian about a dotfile.

This is best demonstrated by example. If NAME is
\".emacs.d/init.el\" then `radian-register-dotfile' will create
the interactive function `radian-find-init-el'. Calling that
function will invoke `find-file' on ~/.emacs.d/init.el.

If additionally KEYBINDING is \"e i\" then
`radian-register-dotfile' will use `global-set-key' to bind
`radian-find-init-el' to (kbd \"M-P e e i\")."
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (defun-name (intern
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
        (let* ((full-keybinding (concat "M-P e " keybinding))
               (set-key-form `(global-set-key (kbd ,full-keybinding)
                                              #',defun-name)))
          `(progn
             ,defun-form
             ,set-key-form))
      defun-form)))

;; Emacs
(radian-register-dotfile ".emacs.d/init.el" "e i")
(radian-register-dotfile ".emacs.d/init.before.local.el" "e b")
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
(radian-register-dotfile ".zshrc.before.local" "z b")
(radian-register-dotfile ".zshrc.local" "z l")

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit this file with <M-P e e i> and
;; then later do C-x C-f, you will be in the Radian repository instead
;; of your home directory.
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

;; When you open a file, position the cursor at the same place as the
;; last time you edited the file. See [1].
;;
;; [1]: https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

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
;; there's no point in asking the user whether or not they want to do
;; it when they find a file. This disables that prompt.
(setq revert-without-query '(".*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired

;; Make C-x C-j jump to the current file in Dired. For some reason,
;; the autoload for `dired-jump' does not appear to work correctly, so
;; we have to autoload the functions explicitly.

(autoload 'dired-jump "dired-x")
(autoload 'dired-jump-other-window "dired-x")
(global-set-key (kbd "C-x C-j") #'dired-jump)
(global-set-key (kbd "C-x 4 C-j") #'dired-jump-other-window)

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

;; Automatically wrap lines when editing plain text files.
(add-hook 'text-mode-hook #'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Killing and yanking

;; If you start typing when you have something selected, then the
;; selection will be deleted. If you press DEL while you have
;; something selected, it will be deleted rather than
;; killed. (Otherwise, in both cases the selection is deselected and
;; the normal function of the key is performed.)
(delete-selection-mode 1)

;; Make delete-selection-mode work properly with Paredit (so that, for
;; example, pressing DEL while there is a selection will actually
;; cause the region to be deleted, as expected). See the commentary in
;; delsel.el for details about what is going on here.
(with-eval-after-load 'paredit
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-newline 'delete-selection t))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Navigation

;; Make it so that if you provide a negative prefix argument to
;; C-SPC (i.e. like M-- C-SPC), then it will step forwards in the mark
;; ring (whereas C-u C-SPC steps backwards). Based on [1].
;;
;; [1]: http://stackoverflow.com/a/14539202/3538165

(defun radian--allow-unpopping-mark (set-mark-command &optional arg)
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))
    (funcall set-mark-command arg)))

(advice-add #'set-mark-command :around #'radian--allow-unpopping-mark)

;; Make it so that if you provide a prefix argument to C-x C-SPC, then
;; it will step forwards in the global mark ring, instead of
;; backwards. Tweaked from the implementation of `pop-global-mark'.

(defun radian--allow-unpopping-global-mark (pop-global-mark &optional arg)
  (interactive "P")
  (if arg
      (progn
        (or global-mark-ring
            (error "No global mark set"))
        ;; We need to do this earlier than `pop-global-mark' does the
        ;; corresponding action in order to properly undo its
        ;; behavior.
        (setq global-mark-ring (nconc (list (car (last global-mark-ring)))
                                      (butlast global-mark-ring)))
        (while (and global-mark-ring (not (marker-buffer (car (last global-mark-ring)))))
          (setq global-mark-ring (butlast global-mark-ring)))
        (let* ((marker (car (last global-mark-ring)))
               (buffer (marker-buffer marker))
               (position (marker-position marker)))
          (set-buffer buffer)
          (or (and (>= position (point-min))
                   (<= position (point-max)))
              (if widen-automatically
                  (widen)
                (error "Global mark position is outside accessible part of buffer")))
          (goto-char position)
          (switch-to-buffer buffer)))
    (funcall pop-global-mark)))

(advice-add #'pop-global-mark :around #'radian--allow-unpopping-global-mark)

;; When using M-. and friends, always prompt for the identifier (it
;; defaults to the identifier at point). This behavior is more
;; consistent and predictable than the default, which is to jump
;; immediately if there is a valid symbol at point.
(setq xref-prompt-for-identifier t)

;; Default to Elisp for M-. and friends, not etags.
(add-hook 'xref-backend-functions #'elisp--xref-backend)

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
(setq custom-file (concat user-emacs-directory "init.before.local.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org

;; Add a global keybinding for accessing the Org Agenda.
(global-set-key (kbd "C-c a") #'org-agenda)

(with-eval-after-load 'org

  ;; Prevent Org from overriding the bindings for windmove.
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)

  ;; Add replacements for the some of keybindings we just removed. It
  ;; looks like Org already binds C-up and C-down separately from M-{
  ;; and M-}, so we can't use those. Users will just have to make do
  ;; with C-c <up> and C-c <down> for now.
  ;;
  (define-key org-mode-map (kbd "C-<left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "C-<right>") #'org-shiftright))

(with-eval-after-load 'org-agenda

  ;; Prevent Org Agenda from overriding the bindings for windmove.
  (define-key org-agenda-mode-map (kbd "S-<up>") nil)
  (define-key org-agenda-mode-map (kbd "S-<down>") nil)
  (define-key org-agenda-mode-map (kbd "S-<left>") nil)
  (define-key org-agenda-mode-map (kbd "S-<right>") nil)

  ;; Same routine as above. Now for Org Agenda, we could use C-up and
  ;; C-down because M-{ and M-} are bound to the same commands. But I
  ;; think it's best to take the same approach as before, for
  ;; consistency.
  (define-key org-agenda-mode-map (kbd "C-<left>") #'org-agenda-do-date-earlier)
  (define-key org-agenda-mode-map (kbd "C-<right>") #'org-agenda-do-date-later))

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

(el-patch-defun lisp-indent-function (indent-point state)
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
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (el-patch-let (($cond (and (elt state 2)
                             (el-patch-wrap 1 1
                               (or (not (looking-at "\\sw\\|\\s_"))
                                   (looking-at ":")))))
                 ($then (progn
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
                          (current-column)))
                 ($else (let ((function (buffer-substring (point)
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
                                 (funcall method indent-point state))))))
    (let ((normal-indent (current-column))
          (el-patch-add
            (orig-point (point))))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (el-patch-swap
        (if $cond
            ;; car of form doesn't seem to be a symbol
            $then
          $else)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ($cond $then)
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
         (t $else))))))

;; Add a keybinding (M-P r) for reloading this file (init.el). This
;; is useful for when you have several instances of Emacs open and you
;; change something in your configuration, then later come back to an
;; old Emacs that was opened before you made the change. You can then
;; just press M-P r to get the change into that instance.

(defun radian-reload-init (&optional upgrade)
  "Reload init.el. With prefix argument, upgrades packages."
  (interactive "P")
  (message "Reloading init.el...")
  (radian-load-user-config "init.el")
  (message "Reloading init.el... done."))

(global-set-key (kbd "M-P r") #'radian-reload-init)

;; Add a keybinding (C-c C-k) for evaluating a buffer of Elisp. This
;; is consistent with the keybindings for evaluating a buffer in CIDER
;; and Geiser.

(defun radian--eval-buffer ()
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (eval-buffer)
  (message "Evaluating %s... done." (buffer-name)))

(defun radian--bind-eval-buffer ()
  (local-set-key (kbd "C-c C-k") #'radian--eval-buffer))

(add-hook 'emacs-lisp-mode-hook #'radian--bind-eval-buffer)

;; Add keybindings (C-h C-f and C-h C-v) for jumping to the source of
;; Elisp functions and variables. Also, add a keybinding (C-h C-o)
;; that performs the functionality of M-. only for Elisp, because the
;; latter command is often rebound by other major modes. Note that
;; this overrides the default bindings of C-h C-f to `view-emacs-FAQ'
;; and C-h C-o to `describe-distribution', but I think this is not
;; very important.

(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-h C-v") #'find-variable)

(defun find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend)))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

(global-set-key (kbd "C-h C-o") #'find-symbol)

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
(with-eval-after-load 'cc-mode
  (advice-add #'c-update-modeline :override #'ignore))

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
(with-eval-after-load 'cc-mode
  (if (assoc 'other c-default-style)
      (setcdr (assoc 'other c-default-style)
              "k&r")
    (push '(other . "k&r") c-default-style))
  (setq-default c-basic-offset 2))

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

;; Provides intelligent fuzzy matching and sorting mechanisms that can
;; be used by various other packages, including Ivy and historian.el.
(use-package flx)

;; Provides a general-purpose completion mechanism.
(use-package ivy
  :init

  ;; Lazy-load Ivy.

  (defvar ivy-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap switch-to-buffer]
        'ivy-switch-buffer)
      (define-key map [remap switch-to-buffer-other-window]
        'ivy-switch-buffer-other-window)
      map)
    "Keymap for `ivy-mode'.")

  (el-patch-define-minor-mode ivy-mode
    "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    :lighter " ivy"
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))

  ;; Use Ivy for `completing-read'.
  (ivy-mode 1)

  ;; Don't show it in the mode line (the `:diminish' below only takes
  ;; effect after the lazy-load is triggered).
  (diminish 'ivy-mode)

  :config

  ;; Use fuzzy matching for Ivy, powered by flx, but not for Swiper
  ;; (because fuzzy matching is typically not desired in grep-style
  ;; searches, just plain 'ol regex).
  ;;
  ;; [1]: http://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  ;; Increase the maximum number of candidates that will be sorted
  ;; using `flx'. The default is 200, which means `flx' is almost
  ;; never used. Setting it too high (e.g. 10000) causes lag. This
  ;; seems to be a good compromise (for example, @PythonNut uses it,
  ;; see [1]).
  ;;
  ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
  (setq ivy-flx-limit 2000)

  ;; A recent (at the time of this writing) patch to Ivy improved the
  ;; default sorting of candidates in many cases, but broke smex. This
  ;; hopefully temporary hack restores the functionality of smex by
  ;; reverting to the old sorting function in `counsel-M-x'.

  (defun ivy--dumb-flx-sort (name cands)
    "Sort according to closeness to string NAME the string list CANDS."
    (condition-case nil
        (if (and cands
                 (< (length cands) ivy-flx-limit))
            (let* ((flx-name (if (string-match "^\\^" name)
                                 (substring name 1)
                               name))
                   (cands-with-score
                    (delq nil
                          (mapcar
                           (lambda (x)
                             (let ((score (flx-score x flx-name ivy--flx-cache)))
                               (and score
                                    (cons score x))))
                           cands))))
              (if cands-with-score
                  (mapcar #'cdr
                          (sort cands-with-score
                                (lambda (x y)
                                  (> (caar x) (caar y)))))
                cands))
          cands)
      (error
       cands)))

  (defun radian--preserve-smex-sorting (counsel-M-x &optional initial-input)
    (cl-letf (((symbol-function #'ivy--flx-sort)
               (symbol-function #'ivy--dumb-flx-sort)))
      (funcall counsel-M-x initial-input)))

  (advice-add #'counsel-M-x :around #'radian--preserve-smex-sorting)

  :bind (;; Add a keybinding for resuming the last completion session.
         ;; The keybinding C-c C-r is suggested in the README for ivy,
         ;; but it's overridden by `sh-mode' and `clojure-mode'.
         ("C-x C-r" . ivy-resume))
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

         ;; Interactively select a kill to yank using ivy, when using
         ;; `yank-pop'.
         ("M-y" . counsel-yank-pop)

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

  (el-patch-defun counsel-projectile-commander-bindings ()
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

  (el-patch-defun counsel-projectile-toggle (toggle)
    "Toggle Ivy version of Projectile commands."
    (if (> toggle 0)
        (progn
          (when (eq projectile-switch-project-action #'projectile-find-file)
            (setq projectile-switch-project-action #'counsel-projectile))
          (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
          (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
          (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)
          (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-ag)
          (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
          (counsel-projectile-commander-bindings))
      (progn
        (when (eq projectile-switch-project-action #'counsel-projectile)
          (setq projectile-switch-project-action #'projectile-find-file))
        (define-key projectile-mode-map [remap projectile-find-file] nil)
        (define-key projectile-mode-map [remap projectile-find-dir] nil)
        (define-key projectile-mode-map [remap projectile-switch-project] nil)
        (define-key projectile-mode-map [remap projectile-ag] nil)
        (define-key projectile-mode-map [remap projectile-switch-to-buffer] nil)
        (projectile-commander-bindings))))

  (with-eval-after-load 'projectile
    (counsel-projectile-toggle 1)))

;; Provides an enhanced version of Isearch that uses Ivy to display
;; a preview of the results.
(use-package swiper
  :bind (;; Use Swiper for Isearches.
         ("C-s" . swiper)
         ("C-r" . swiper)))

;; Remembers your choices in completion menus.
(use-package historian
  :recipe (:fetcher github
           :repo "PythonNut/historian.el")
  :demand t
  :config

  ;; Enable the functionality of historian.el.
  (historian-mode 1))

;; Uses Historian to sort Ivy candidates by frecency+flx.
(use-package historian-ivy
  :recipe (:fetcher github
           :repo "PythonNut/historian.el")
  :after ivy
  :config

  ;; Enable the functionality of historian-ivy.
  (historian-ivy-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: User interface

;; Provides an easy way to change the display of minor modes in the
;; mode line.
(use-package diminish
  :demand t
  :config

  ;; Don't show `abbrev-mode' in the mode line.
  (diminish 'abbrev-mode)

  ;; Don't show `smerge-mode' in the mode line.
  (with-eval-after-load 'smerge-mode
    (diminish 'smerge-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Window management

;; Provides simple commands to mirror, rotate, and transpose Emacs
;; windows.
(use-package transpose-frame)

;; Provides simple commands to swap Emacs windows.
(use-package buffer-move)

;; Provides more intuitive behavior for C-x <left> and C-x <right>.
(use-package iflipb
  :config

  ;; Don't skip buffers that start with an asterisk.
  (setq iflipb-ignore-buffers nil)

  ;; Allow the use of C-x <right> as an "undo" operation even after
  ;; breaking the C-x <arrow> chain and running another command.
  (setq iflipb-permissive-flip-back t)

  ;; Don't show the state of the buffer switching in the minibuffer,
  ;; because the buffers are shown from left to right. (This means
  ;; that C-x <left> moves right and C-x <right> moves left, which is
  ;; confusing.) This means that `iflipb' is no longer an alt-tab
  ;; emulator, but just a silent improvement on C-x <left> and C-x
  ;; <right>.

  (advice-add #'iflipb-format-buffers :override #'ignore)
  (advice-add #'iflipb-message :before-while #'identity)

  :bind (;; Replace the standard C-x <left> and C-x <right> bindings
         ;; with subtly improved versions.
         ("C-x <left>" . iflipb-next-buffer)
         ("C-x <right>" . iflipb-previous-buffer)))

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

  ;; Don't show Undo Tree in the mode line.
  (setq undo-tree-mode-lighter nil)

  ;; Suppress the message saying that the undo history file was
  ;; saved (because this happens every single time you save a file).

  (defun radian--undo-tree-suppress-undo-history-saved-message
      (undo-tree-save-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-save-history args)))

  (advice-add #'undo-tree-save-history :around
              #'radian--undo-tree-suppress-undo-history-saved-message)

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

  ;; Only search the current buffer to get suggestions for
  ;; company-dabbrev (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

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
  ;; [3]: http://emacs.stackexchange.com/a/27482/12534
  (setq radian--company-complete-if-explicit
        `(menu-item nil company-complete
                    :filter ,(lambda (cmd)
                               (when (company-explicit-action-p)
                                 cmd))))

  ;; Make RET trigger a completion if and only if the user has
  ;; explicitly interacted with Company. Note that <return> is for
  ;; windowed Emacs and RET is for terminal Emacs. Also note that
  ;; since we are mapping the keys to extended menu items, we can't
  ;; use the `:bind' keyword provided by `use-package'.
  (define-key company-active-map (kbd "<return>")
    radian--company-complete-if-explicit)
  (define-key company-active-map (kbd "RET")
    radian--company-complete-if-explicit)

  ;; We then do the same for the up and down arrows. Note that we use
  ;; `company-select-previous' instead of
  ;; `company-select-previous-or-abort'. I think the former makes more
  ;; sense since the general idea of this `company' configuration is
  ;; to decide whether or not to steal keypresses based on whether the
  ;; user has explicitly interacted with `company', not based on the
  ;; number of candidates.

  (setq radian--company-select-previous-if-explicit
        `(menu-item nil company-select-previous
                    :filter ,(lambda (cmd)
                               (when (company-explicit-action-p)
                                 cmd))))

  (setq radian--company-select-next-if-explicit
        `(menu-item nil company-select-next
                    :filter ,(lambda (cmd)
                               (when (company-explicit-action-p)
                                 cmd))))

  (define-key company-active-map (kbd "<up>")
    radian--company-select-previous-if-explicit)
  (define-key company-active-map (kbd "<down>")
    radian--company-select-next-if-explicit)

  :bind (;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         :map company-active-map
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
  (el-patch-defun company-statistics--load ()
    "Restore statistics."
    (load company-statistics-file 'noerror
          (el-patch-swap nil 'nomessage)
          'nosuffix))

  ;; Enable Company Statistics.
  (company-statistics-mode 1))

;; Allows the expansion of user-defined abbreviations into fillable
;; templates. This is used by clj-refactor for some of its
;; refactorings.
(use-package yasnippet
  :config

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.

  (when (radian-package-enabled-p 'company)
    (with-eval-after-load 'company

      ;; This function translates the "event types" I get from
      ;; `map-keymap' into things that I can pass to `lookup-key'
      ;; and `define-key'. It's a hack, and I'd like to find a
      ;; built-in function that accomplishes the same thing while
      ;; taking care of any edge cases I might have missed in this
      ;; ad-hoc solution.
      (defun radian--normalize-event (event)
        (if (vectorp event)
            event
          (vector event)))

      ;; Here we define a hybrid keymap that delegates first to
      ;; `company-active-map' and then to `yas-keymap'.
      (setq radian--yas-company-keymap
            ;; It starts out as a copy of `yas-keymap', and then we
            ;; merge in all of the bindings from
            ;; `company-active-map'.
            (let ((keymap (copy-keymap yas-keymap)))
              (map-keymap
               (lambda (event company-cmd)
                 (let* ((event (radian--normalize-event event))
                        (yas-cmd (lookup-key yas-keymap event)))
                   ;; Here we use an extended menu item with the
                   ;; `:filter' option, which allows us to
                   ;; dynamically decide which command we want to
                   ;; run when a key is pressed.
                   (define-key keymap event
                     `(menu-item
                       nil ,company-cmd :filter
                       (lambda (cmd)
                         ;; There doesn't seem to be any obvious
                         ;; function from Company to tell whether or
                         ;; not a completion is in progress (à la
                         ;; `company-explicit-action-p'), so I just
                         ;; check whether or not `company-my-keymap'
                         ;; is defined, which seems to be good
                         ;; enough.
                         (if company-my-keymap
                             ',company-cmd
                           ',yas-cmd))))))
               company-active-map)
              keymap))

      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (defun radian--company-overrides-yasnippet
          (yas--make-control-overlay &rest args)
        (let ((yas-keymap radian--yas-company-keymap))
          (apply yas--make-control-overlay args)))

      (advice-add #'yas--make-control-overlay :around
                  #'radian--company-overrides-yasnippet)))

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

  ;; Use Ivy for completion, instead of `grizzl'.
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

;; Allows you to jump to characters and words visible onscreen.
(use-package avy
  :config

  ;; Use De Bruijn sequences for jump sequences. This allows you to
  ;; fixate on a particular place you want to jump to, and jump type
  ;; whatever shows up there.
  (setq avy-style 'de-bruijn)

  :bind (;; Bind common avy commands to the <M-P g> prefix.
         ("M-P g c" . avy-goto-char)
         ("M-P g t" . avy-goto-char-timer)
         ("M-P g l" . avy-goto-line)
         ("M-P g W" . avy-goto-word-1)
         ("M-P g w" . avy-goto-word-0)))

;; Highlights matches and previews replacements in query replace.
(use-package visual-regexp-steroids
  :bind (;; Replace the regular query replace with the regexp query
         ;; replace provided by this package.
         ("M-%" . vr/query-replace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Clojure

;; Provides indentation and syntax highlighting for Clojure code.
(use-package clojure-mode
  :init

  ;; We define some patches after clojure-mode is loaded. We need to
  ;; make sure el-patch knows how to find these patches.

  (defun radian--enable-clojure-mode-patches ()
    (require 'clojure-mode)
    (radian-clojure-strings-as-docstrings-mode 1))

  (defun radian--disable-clojure-mode-patches ()
    (radian-clojure-strings-as-docstrings-mode -1))

  (add-hook 'el-patch-pre-validate-hook
            #'radian--enable-clojure-mode-patches)
  (add-hook 'el-patch-post-validate-hook
            #'radian--disable-clojure-mode-patches)

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

  ;; `clojure-mode' does not correctly identify the docstrings of
  ;; protocol methods as docstrings, and as such electric and
  ;; aggressive indentation do not work for them. Additionally, when
  ;; you hack a clojure.core function, such as defonce or defrecord,
  ;; to provide docstring functionality, those docstrings are
  ;; (perhaps rightly, but annoyingly) not recognized as docstrings
  ;; either. However, there is an easy way to get electric indentation
  ;; working for all potential docstrings: simply tell `clojure-mode'
  ;; that *all* strings are docstrings. This will not change the font
  ;; locking, because for some weird reason `clojure-mode' determines
  ;; whether you're in a docstring by the font color instead of the
  ;; other way around. Note that this will cause electric indentation
  ;; by two spaces in *all* multiline strings, but since there are not
  ;; very many non-docstring multiline strings in Clojure this is not
  ;; too inconvenient. (And, after all, it's only electric, not
  ;; aggressive, indentation.)

  ;; Unfortunately, `clojure-in-docstring-p' is defined as an inline
  ;; function, so we can't override it. Instead, we replace
  ;; `clojure-indent-line'. But inside a new minor mode, so that the
  ;; user can toggle it if they need to use `aggressive-indent-mode'
  ;; and multiline strings that are not docstrings at the same time.

  (define-minor-mode radian-clojure-strings-as-docstrings-mode
    "Toggles whether or not all strings are treated as docstrings
in Clojure. You want to turn this off if you have multiline
strings that are not docstrings."
    nil nil nil
    (if radian-clojure-strings-as-docstrings-mode
        (progn
          (el-patch-defsubst clojure-in-docstring-p ()
            "Check whether point is in a docstring."
            (el-patch-wrap 1 1
              (or
               (eq (get-text-property (point) 'face) 'font-lock-doc-face)
               (eq (get-text-property (point) 'face) 'font-lock-string-face))))
          (el-patch-defun clojure-indent-line ()
            "Indent current line as Clojure code."
            (if (clojure-in-docstring-p)
                (save-excursion
                  (beginning-of-line)
                  (when (and (looking-at "^\\s-*")
                             (<= (string-width (match-string-no-properties 0))
                                 (string-width (clojure-docstring-fill-prefix))))
                    (replace-match (clojure-docstring-fill-prefix))))
              (lisp-indent-line))))
      (el-patch-unpatch 'clojure-in-docstring-p 'defsubst)
      (el-patch-unpatch 'clojure-indent-function 'defun)))

  (add-hook 'clojure-mode-hook #'radian-clojure-strings-as-docstrings-mode)

  ;; Improve the performance of `clojure-project-dir' by memoizing it.
  ;; This alleviates some quite horrible lag generated by CIDER
  ;; calling this function continually.

  (defvar clojure-project-dir-cache (make-hash-table :test 'equal))

  (el-patch-defun clojure-project-dir (&optional dir-name)
    "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
    (el-patch-let (($dir-name (dir-name (or dir-name default-directory)))
                   ($choices (choices (delq nil
                                            (mapcar (lambda (fname)
                                                      (locate-dominating-file dir-name fname))
                                                    clojure-build-tool-files))))
                   ($body (when (> (length choices) 0)
                            (car (sort choices #'file-in-directory-p)))))
      (el-patch-swap
        (let* ($dir-name
               $choices)
          $body)
        (let ($dir-name)
          (or (gethash dir-name clojure-project-dir-cache)
              (puthash dir-name
                       (let ($choices) $body)
                       clojure-project-dir-cache))))))

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
  :init

  ;; We define some patches after CIDER is loaded. We need to make
  ;; sure el-patch knows how to find these patches.

  (defun radian--enable-cider-patches ()
    (require 'cider))

  (add-hook 'el-patch-pre-validate-hook
            #'radian--enable-cider-patches)

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

  ;; Use the :emacs profile defined in profiles.clj. This enables lots
  ;; of cool extra features in the REPL.
  (setq cider-lein-parameters "with-profile +emacs repl :headless")

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
  ;; in Emacs, for use with e.g. [1]. The last three forms are from
  ;; the definition of `cider--cljs-repl-types'; the first two work
  ;; around [2].
  ;;
  ;; [1]: https://github.com/reagent-project/reagent-template
  ;; [2]: https://github.com/reagent-project/reagent-template/issues/132
  (setq cider-cljs-lein-repl
        "(do
  (require 'clojure.java.shell)
  (clojure.java.shell/sh \"lein\" \"clean\")
  (require 'figwheel-sidecar.repl-api)
  (figwheel-sidecar.repl-api/start-figwheel!)
  (figwheel-sidecar.repl-api/cljs-repl))")

  ;; Don't show CIDER in the mode line.
  (setq cider-mode-line nil)

  ;; Make some modes perform their actions less often in buffers where
  ;; CIDER is active. This is helpful because CIDER is slow, and will
  ;; lag Emacs if its logic is called too often.

  (defun radian--disable-aggressive-indent-on-save ()
    (remove-hook 'before-save-hook
                 ;; Yes, this is a typo in `aggressive-indent'.
                 #'aggressive-indent--proccess-changed-list-and-indent
                 'local))

  (defun radian--reduce-cider-lag ()
    (setq-local company-idle-delay 1) ; increased from 0
    (setq-local company-minimum-prefix-length 3) ; increased from 0
    (setq-local eldoc-idle-delay 0.5) ; increased from 0
    (when cider-mode
      (radian--disable-aggressive-indent-on-save)
      (add-hook 'aggressive-indent-mode-hook
                #'radian--disable-aggressive-indent-on-save)))

  (add-hook 'cider-mode-hook #'radian--reduce-cider-lag)
  (add-hook 'cider-repl-mode-hook #'radian--reduce-cider-lag)

  ;; Suppress the "Starting a custom ClojureScript REPL" message,
  ;; because it provides no useful information.

  (el-patch-defun cider-create-sibling-cljs-repl (client-buffer)
    "Create a ClojureScript REPL with the same server as CLIENT-BUFFER.
The new buffer will correspond to the same project as CLIENT-BUFFER, which
should be the regular Clojure REPL started by the server process filter."
    (interactive (list (cider-current-connection)))
    (let* ((nrepl-repl-buffer-name-template "*cider-repl CLJS%s*")
           (nrepl-create-client-buffer-function #'cider-repl-create)
           (nrepl-use-this-as-repl-buffer 'new)
           (client-process-args (with-current-buffer client-buffer
                                  (unless (or nrepl-server-buffer nrepl-endpoint)
                                    (error "This is not a REPL buffer, is there a REPL active?"))
                                  (list (car nrepl-endpoint)
                                        (elt nrepl-endpoint 1)
                                        (when (buffer-live-p nrepl-server-buffer)
                                          (get-buffer-process nrepl-server-buffer)))))
           (cljs-proc (apply #'nrepl-start-client-process client-process-args))
           (cljs-buffer (process-buffer cljs-proc)))
      (with-current-buffer cljs-buffer
        ;; The new connection has now been bumped to the top, but it's still a
        ;; Clojure REPL!  Additionally, some ClojureScript REPLs can actually take
        ;; a while to start (some even depend on the user opening a browser).
        ;; Meanwhile, this REPL will gladly receive requests in place of the
        ;; original Clojure REPL.  Our solution is to bump the original REPL back
        ;; up the list, so it takes priority on Clojure requests.
        (cider-make-connection-default client-buffer)
        (el-patch-remove
          (pcase (assoc cider-cljs-lein-repl cider--cljs-repl-types)
            (`(,_ ,name ,info)
             (message "Starting a %s REPL%s" name (or info "")))
            (_ (message "Starting a custom ClojureScript REPL"))))
        (cider-nrepl-send-request
         (list "op" "eval"
               "ns" (cider-current-ns)
               "session" nrepl-session
               "code" cider-cljs-lein-repl)
         (cider-repl-handler (current-buffer)))
        (cider--offer-to-open-app-in-browser nrepl-server-buffer))))

  :bind (;; Allow usage of the C-c M-j and C-c M-J shortcuts everywhere.
         ("C-c M-j" . cider-jack-in)
         ("C-c M-J" . cider-jack-in-clojurescript)))

;; Makes Emacs into a real Clojure IDE by providing a mountain of
;; automated refactoring tools.
(use-package clj-refactor
  :dependencies (clojure-mode cider yasnippet)
  :init

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.
  (advice-add #'cljr--post-command-message :override #'message)

  ;; By default, clj-refactor enables the refactor-nrepl middleware in
  ;; an *autoload*, meaning that it's already happened as soon as
  ;; clj-refactor has loaded. But refactor-nrepl doesn't work outside
  ;; a Clojure project, and signals a warning in that case. So we need
  ;; to *selectively* inject refactor-nrepl, which means we need to
  ;; disable it right away so we can do it buffer-locally later. See
  ;; [1] for discussion of the problem.
  ;;
  ;; [1]: https://github.com/clojure-emacs/refactor-nrepl/issues/177
  (setq cljr-inject-dependencies-at-jack-in nil)

  ;; Enable clj-refactor in Clojure buffers. This is adapted from the
  ;; clj-refactor README [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/clj-refactor.el

  (defun radian--enable-clj-refactor-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c RET"))

  (add-hook 'clojure-mode-hook #'radian--enable-clj-refactor-mode)

  ;; Because we disabled injection of the clj-refactor middleware
  ;; earlier, we have to do it buffer-locally now (but only if we're
  ;; in a project).

  (defun radian--inject-cljr-dependencies (&rest args)
    (when (cljr--project-dir)
      (setq-local cljr-inject-dependencies-at-jack-in t)
      (make-local-variable 'cider-jack-in-lein-plugins)
      (make-local-variable 'cider-jack-in-nrepl-middlewares)
      (cljr--inject-jack-in-dependencies)))

  (advice-add #'cider-jack-in :before #'radian--inject-cljr-dependencies)

  :config

  ;; We also need to tell clj-refactor not to check that
  ;; refactor-nrepl is installed properly when we are not in a
  ;; project.

  (advice-add #'cljr--init-middleware
              :before-while #'cljr--project-dir)

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.
  (advice-add #'cljr--post-command-message :override #'message)

  :diminish clj-refactor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Racket

;; Provides Racket REPL integration, including documentation and
;; source lookups. Basically CIDER for Racket.
(use-package geiser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Python

;; Integrated development environment for Python.
(use-package anaconda-mode
  :init

  ;; Enable the functionality of anaconda-mode in Python buffers, as
  ;; suggested in the README [1].
  ;;
  ;; [1]: https://github.com/proofit404/anaconda-mode
  (add-hook 'python-mode-hook #'anaconda-mode)

  :config

  ;; Prevent anaconda-mode from overriding our binding for M-TAB,
  ;; which we want to trigger Company.
  (with-eval-after-load 'company
    (define-key anaconda-mode-map (kbd "C-M-i") nil))

  :diminish anaconda-mode)

;; Company integration for anaconda-mode.
(use-package company-anaconda

  :init

  ;; Enable the functionality of company-anaconda in Python buffers,
  ;; as suggested in the README [1].
  ;;
  ;; [1]: https://github.com/proofit404/company-anaconda

  (with-eval-after-load 'company

    (defun radian--enable-company-anaconda ()
      (add-to-list 'company-backends #'company-anaconda))

    (add-hook 'python-mode-hook #'radian--enable-company-anaconda)))

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

  (defun radian--install-irony-server-if-missing ()
    (unless (irony--locate-server-executable)
      (if (yes-or-no-p "Install irony-server? ")
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
            (irony-install-server command))
        (message "irony-mode will not work until you M-x irony-install-server"))))

  (add-hook 'irony-mode-hook #'radian--install-irony-server-if-missing)

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
  :recipe (:fetcher github
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
(use-package markdown-mode
  :config

  ;; Automatically wrap text when editing Markdown files.
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

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
;;;; Packages: YAML

;; Provides syntax highlighting, indentation, and editing commands for
;; YAML files.
(use-package yaml-mode
  :config

  ;; Don't automatically wrap text when editing YAML files.

  (defun radian--disable-auto-fill-mode ()
    (auto-fill-mode -1))

  (add-hook 'yaml-mode-hook #'radian--disable-auto-fill-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages: Version control

;; A Git Porcelain inside Emacs.
(use-package magit
  :recipe (:fetcher github
           :repo "raxod502/magit"
           :branch "magit-version-symlinks"
           :files ("lisp/magit*.el"
                   "lisp/git-rebase.el"
                   "Documentation/magit.texi"
                   "Documentation/AUTHORS.md"
                   "COPYING" (:exclude "lisp/magit-popup.el")))
  :bind (;; Add important keybindings for Magit as described in the
         ;; manual [1].
         ;;
         ;; [1]: https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

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
;;;; Color themes

;; Handle color themes that require installing packages or other
;; additional configuration.

(pcase radian-color-theme
  ((or 'doom-one 'doom-molokai)
   (use-package doom-themes
     :demand t
     :config

     ;; Comments are invisible if I don't do the following.
     (setq doom-enable-brighter-comments t)

     ;; It's impossible to tell which window is active if I don't do
     ;; this.
     (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer))))

;; Load the appropriate color scheme as specified in
;; `radian-color-theme'.

(when radian-color-theme
  (load-theme radian-color-theme 'no-confirm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Mode line

;;; The following code customizes the mode bar to something like:
;;; [*] init.el        96% (2410,30)  [radian:master*]  (Emacs-Lisp Paredit AggrIndent)

(defun radian--mode-line-buffer-modified ()
  "Returns a construct for the mode line that shows [*] if the
buffer has been modified, and whitespace otherwise."
  (propertize (if (and (buffer-modified-p)
                       (buffer-file-name))
                  "[*]" "   ")
              ;; make sure to show it in the same color as the
              ;; buffer name
              'face 'mode-line-buffer-id))

;; To display information about the current Git status in the mode
;; line, we need to load this library.
(require 'vc-git)

(defvar-local radian--mode-line-project-and-branch nil
  "Construct for the mode line that shows the current Projectile
project (if Projectile is enabled and the user is within a
project), the current Git branch (if the user is within a Git
repo), and whether the working directory is dirty.

Computed and cached in this variable by
`radian--compute-mode-line-project-and-branch' for performance
reasons.")

(defun radian--compute-mode-line-project-and-branch ()
  (let ((old radian--mode-line-project-and-branch)
        (new
         (let* ((project-name (when (featurep 'projectile)
                                (projectile-project-name)))
                (project-name (unless (equal project-name "-")
                                project-name))
                (git (locate-dominating-file default-directory ".git"))
                (branch-name (when git
                               (or (vc-git--symbolic-ref default-directory)
                                   (substring (vc-git-working-revision
                                               default-directory)
                                              0 7))))
                (dirty (when git
                         (with-temp-buffer
                           (call-process "git" nil t nil
                                         "status" "--porcelain"
                                         "--ignore-submodules=dirty")
                           (if (> (buffer-size) 0)
                               "*" "")))))
           (cond
            ((and project-name git)
             (format "  [%s:%s%s]" project-name branch-name dirty))
            (project-name
             (format "  [%s]" project-name))
            ;; This should never happen unless you do something
            ;; perverse like create a version-controlled Projectile
            ;; project whose name is a hyphen, but we want to handle
            ;; it anyway.
            (git
             (format "  [%s%s]" branch-name dirty))))))
    (unless (equal old new)
      (setq radian--mode-line-project-and-branch new)
      (force-mode-line-update))))

;; We will make sure this information is updated after one second of
;; inactivity, for the current buffer.

(defvar radian--mode-line-timer-primary nil)
(defvar radian--mode-line-timer-secondary nil)

(defun radian--compute-mode-line-and-reschedule ()
  (when radian--mode-line-timer-secondary
    (cancel-timer radian--mode-line-timer-secondary))
  (radian--compute-mode-line-project-and-branch)
  (setq radian--mode-line-timer-secondary
        (run-with-idle-timer
         (time-add 1 (current-idle-time)) nil
         #'radian--compute-mode-line-and-reschedule)))

(when radian--mode-line-timer-primary
  (cancel-timer radian--mode-line-timer-primary))

(when radian--mode-line-timer-secondary
  (cancel-timer radian--mode-line-timer-secondary))

(setq radian--mode-line-timer-primary
      (run-with-idle-timer
       1 'repeat #'radian--compute-mode-line-and-reschedule))

(setq-default mode-line-format
              '(;; Show a warning if Emacs is low on memory.
                "%e"
                ;; Show [*] if the buffer is modified.
                (:eval (radian--mode-line-buffer-modified))
                " "
                ;; Show the name of the current buffer.
                mode-line-buffer-identification
                "   "
                ;; Show the row and column of point.
                mode-line-position
                ;; Show the current Projectile project.
                radian--mode-line-project-and-branch
                ;; Show the active major and minor modes.
                "  "
                mode-line-modes))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Load user-specific configuration file (2 of 2)

(radian-load-user-config "init.local.el")
