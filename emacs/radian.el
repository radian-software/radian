;; -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Load built-in utility libraries

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;; Define Radian customization group

(defgroup radian nil
  "Customize your Radian Emacs experience."
  :prefix "radian-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/radian"))

;;; Define utility functions and variables

(defvar radian-directory (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            radian-lib-file)))
  "Path to the Radian Git repository.")

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:

Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.

You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "radian-defadvice: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (advice-add ',place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hook docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOK is the hook to which to add the
function. DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (string-match-p "-hook$" (symbol-name hook))
    (error "Symbol `%S' is not a hook" hook))
  (unless (stringp docstring)
    (error "radian-defhook: no docstring provided"))
  `(progn
     (defun ,name ,arglist
       ,(format "%s\n\nThis function is for use in `%S'."
                docstring hook)
       ,@body)
     (add-hook ',hook ',name)))

(defmacro radian-operating-system-p (os)
  "Return non-nil if OS matches the system type.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro radian-with-operating-system (os &rest body)
  "If OS matches the system type, eval and return BODY. Else return nil.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (declare (indent 1))
  `(when (radian-operating-system-p ,os)
     ,@body))

(defun radian-managed-p (filename)
  "Return non-nil if FILENAME is managed by Radian.
This means that FILENAME is a symlink whose target is inside
`radian-directory'."
  (let ((truename (file-truename filename)))
    (string-prefix-p radian-directory truename
                     (when (file-name-case-insensitive-p truename)
                       'ignore-case))))

(defmacro radian--with-silent-load (&rest body)
  "Execute BODY, silencing any calls to `load' within."
  (declare (indent 0))
  `(cl-letf* ((load-orig (symbol-function #'load))
              ((symbol-function #'load)
               (lambda (file &optional noerror _nomessage &rest args)
                 (apply load-orig file noerror 'nomessage args))))
     ,@body))

(defmacro radian--with-silent-write (&rest body)
  "Execute BODY, silencing any calls to `write-region' within."
  (declare (indent 0))
  `(cl-letf* ((write-region-orig (symbol-function #'write-region))
              ((symbol-function #'write-region)
               (lambda (start end filename &optional append visit lockname
                              mustbenew)
                 (funcall write-region-orig start end filename append 0
                          lockname mustbenew)
                 (set-buffer-modified-p nil)
                 (set-visited-file-modtime)))
              ((symbol-function #'message) #'ignore))
     ,@body))

(defun radian--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun radian--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

;;; Define hooks and load local configuration

;; Reset the value of this variable so that stale functions don't
;; stick around.
(setq radian--finalize-init-hook nil)

(defcustom radian-before-straight-hook nil
  "Hook run just before Radian bootstraps straight.el."
  :type 'hook)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)

(defun radian--run-hook (hook)
  "Run the given local init HOOK, a symbol.
This delegates to `run-hooks', binding `straight-current-profile'
appropriately."
  (let ((straight-current-profile 'radian-local))
    (run-hooks hook)))

;; Allow to disable local customizations with a
;; command-line argument.
(if (member "--no-local" command-line-args)

    ;; Make sure to delete --no-local from the list, because
    ;; otherwise Emacs will issue a warning about the unknown
    ;; argument.
    (setq command-line-args
          (delete "--no-local" command-line-args))

  ;; Load local customizations.
  (load radian-local-init-file 'noerror 'nomessage))

;;; Disable GC during startup

;; Disabling GC (by setting `gc-cons-threshold' to a very large value,
;; in this case 500MB) during startup is said to improve startup time
;; by reducing the number of GC runs.

(defvar radian--orig-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold'.")

(radian-defhook radian--reenable-gc ()
  radian--finalize-init-hook
  "Reset `gc-cons-threshold' to its original value.
Otherwise, Emacs will just get slower and slower over time."
  (setq gc-cons-threshold radian--orig-gc-cons-threshold))

(setq gc-cons-threshold (* 5 1000 1000))

;;; Networking

;; Use `with-eval-after-load' instead of `use-feature' because we have
;; not yet set up package management.

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
(with-eval-after-load 'gnutls

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;; Feature `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http

  (radian-defadvice radian--no-query-on-http-kill
      (buffer)
    :filter-return url-http
    "Disable query-on-exit for all network connections.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request. (It also works around a bug
in `anaconda-mode' which causes unanswered network connections to
pile up, and then interrupt Emacs shutdown.)"
    (prog1 buffer
      (set-process-query-on-exit-flag
       (get-buffer-process buffer) nil))))

;;; Set up package management
;;;; straight.el

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '(;; Packages registered in this file.
        (radian . "radian.el")
        ;; Packages registered in the local init-file during hooks.
        (radian-local . "radian-local.el")
        ;; Packages registered interactively.
        (nil . "default.el")))

;; Pretend to dynamically bind `straight-current-profile' to `radian'
;; over the init-file. We do this to avoid having straight.el
;; configuration mentioned in the top-level init-file.

(radian-defhook radian--reset-straight-current-profile ()
  radian--finalize-init-hook
  "Reset `straight-current-profile' to nil.
This function is used on `radian--finalize-init-hook' to emulate
binding the variable dynamically over the entire init-file."
  (setq straight-current-profile nil))

(setq straight-current-profile 'radian)

;; Treat loading the init-file as a transaction. See the straight.el
;; documentation for more information about this.

(radian-defhook radian--finalize-straight-transaction ()
  radian--finalize-init-hook
  "Finalize the init-file's straight.el transaction."
  (setq straight-treat-as-init nil)
  ;; Just in case the straight.el bootstrap failed, do not mask the
  ;; error with a void-function error.
  (when (fboundp 'straight-finalize-transaction)
    (straight-finalize-transaction)))

(setq straight-treat-as-init t)

;; Use the develop branch of straight.el on Radian's develop branch.
;; (On Radian's master branch, we use the master branch of
;; straight.el.)
(setq straight-repository-branch "develop")

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the less robust but equally fast `before-save-hook' modification
;; checker. In both cases, allow using find(1) when explicitly
;; requested.
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications '(check-on-save find-when-checking)))

(radian--run-hook 'radian-before-straight-hook)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; use-package

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;;;; straight.el configuration

;; Feature `straight-x' from package `straight' provides
;; experimental/unstable extensions to straight.el which are not yet
;; ready for official inclusion.
(use-feature straight-x
  ;; Add an autoload for this extremely useful command.
  :commands (straight-x-fetch-all))

;;; Configure ~/.emacs.d paths

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;;; Prevent Emacs-provided Org from being loaded

;; The following is a temporary hack until straight.el supports
;; building Org, see:
;;
;; * https://github.com/raxod502/straight.el/issues/211
;; * https://github.com/raxod502/radian/issues/410
;;
;; There are three things missing from our version of Org: the
;; functions `org-git-version' and `org-release', and the feature
;; `org-version'. We provide all three of those ourself, therefore.

;; Package `git' is a library providing convenience functions for
;; running Git.
(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)

;;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop")
  :demand t)

;;; Fixes to internal functions

;; Backported bugfix for `while-no-input' from Emacs 27 which helps to
;; prevent spurious "Quit" events from being registered in some
;; situations. See [1].
;;
;; [1]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31692.
(el-patch-defmacro while-no-input (&rest body)
  (el-patch-concat
    "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
    (el-patch-add
      "\n\nThis function includes a backported fix for bug#31692;
see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31692."))
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym)
               (el-patch-add val))
           (el-patch-wrap 2
             (setq val (or (input-pending-p)
	                   (progn ,@body))))
           (el-patch-add
             (cond
              ;; When input arrives while throw-on-input is non-nil,
              ;; kbd_buffer_store_buffered_event sets quit-flag to the
              ;; value of throw-on-input.  If, when BODY finishes,
              ;; quit-flag still has the same value as throw-on-input, it
              ;; means BODY never tested quit-flag, and therefore ran to
              ;; completion even though input did arrive before it
              ;; finished.  In that case, we must manually simulate what
              ;; 'throw' in process_quit_flag would do, and we must
              ;; reset quit-flag, because leaving it set will cause us
              ;; quit to top-level, which has undesirable consequences,
              ;; such as discarding input etc.  We return t in that case
              ;; because input did arrive during execution of BODY.
              ((eq quit-flag throw-on-input)
               (setq quit-flag nil)
               t)
              ;; This is for when the user actually QUITs during
              ;; execution of BODY.
              (quit-flag
               nil)
              (t val))))))))

;;; Keybindings

;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*', `bind-keys', `bind-keys*', and `unbind-key') which
;; provides a much prettier API for manipulating keymaps than
;; `define-key' and `global-set-key' do. It's also the same API that
;; `:bind' and similar keywords in `use-package' use.
(use-package bind-key)

(defvar radian-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a prefix.
This keymap is bound under M-P.")

(bind-key* "M-P" radian-keymap)

(defmacro radian-bind-key (key-name command &optional predicate)
  "Bind a key in `radian-keymap'."
  `(bind-key ,key-name ,command radian-keymap ,predicate))

(defun radian-join-keys (&rest keys)
  "Join key sequences. Empty strings and nils are discarded.
\(radian--join-keys \"M-P e\" \"e i\") => \"M-P e e i\"
\(radian--join-keys \"M-P\" \"\" \"e i\") => \"M-P e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

;;; Environment
;;;; Environment variables

(defun radian-source-profile ()
  "Load ~/.profile and set environment variables exported therein."
  (interactive)
  (let ((profile-file "~/.profile")
        (buf-name " *radian-env-output*"))
    (when (and profile-file
               (file-exists-p profile-file)
               (executable-find "python"))
      (ignore-errors (kill-buffer buf-name))
      (with-current-buffer (get-buffer-create buf-name)
        (let* ((python-script
                (expand-file-name "scripts/print_env.py" radian-directory))
               (delimiter (radian--random-string))
               (sh-script (format ". %s && %s %s"
                                  (shell-quote-argument
                                   (expand-file-name profile-file))
                                  (shell-quote-argument python-script)
                                  (shell-quote-argument delimiter)))
               (return (call-process "sh" nil t nil "-c" sh-script))
               (found-delimiter
                (progn
                  (goto-char (point-min))
                  (search-forward delimiter nil 'noerror))))
          (if (and (= 0 return) found-delimiter)
              (let* ((results (split-string
                               (buffer-string) (regexp-quote delimiter)))
                     (results (cl-subseq results 1 (1- (length results)))))
                (if (cl-evenp (length results))
                    (cl-loop for (var value) on results by #'cddr do
                             (setenv var value)
                             (when (string= var "PATH")
                               (setq exec-path (parse-colon-path value))))
                  (message
                   "Loading %s produced malformed result; see buffer %S"
                   profile-file
                   buf-name)))
            (message "Failed to load %s; see buffer %S"
                     profile-file
                     buf-name)))))))

(defvar radian--source-profile-timer
  (run-at-time 1 nil #'radian-source-profile)
  "Timer used to run `radian-source-profile'.
We shouldn't need environment variables to be set correctly
during init, so deferring their processing saves some time at
startup.")

;;;; Clipboard integration

;; On macOS, clipboard integration works out of the box in windowed
;; mode but not terminal mode. The following code to fix it was
;; originally based on [1], and then modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: https://emacs.stackexchange.com/q/26471/12534
(radian-with-operating-system macOS
  (unless (display-graphic-p)

    (defvar radian--clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (defun radian--clipboard-paste ()
      "Return the contents of the macOS clipboard, as a string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Command pbpaste returns the clipboard contents as a
             ;; string.
             (text (shell-command-to-string "pbpaste")))
        ;; If this function returns nil then the system clipboard is
        ;; ignored and the first element in the kill ring (which, if
        ;; the system clipboard has not been modified since the last
        ;; kill, will be the same) is used instead. Including this
        ;; `unless' clause prevents you from getting the same text
        ;; yanked the first time you run `yank-pop'. (Of course, this
        ;; is less relevant due to `counsel-yank-pop', but still is
        ;; definitely the correct behavior.)
        (unless (string= text radian--clipboard-last-copy)
          text)))

    (defun radian--clipboard-copy (text)
      "Set the contents of the macOS clipboard to given TEXT string."
      (let* (;; Setting `default-directory' to a directory that is
             ;; sure to exist means that this code won't error out
             ;; when the directory for the current buffer does not
             ;; exist.
             (default-directory "/")
             ;; Setting `process-connection-type' makes Emacs use a pipe to
             ;; communicate with pbcopy, rather than a pty (which is
             ;; overkill).
             (process-connection-type nil)
             ;; The nil argument tells Emacs to discard stdout and
             ;; stderr. Note, we aren't using `call-process' here
             ;; because we want this command to be asynchronous.
             ;;
             ;; Command pbcopy writes stdin to the clipboard until it
             ;; receives EOF.
             (proc (start-process "pbcopy" nil "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))
      (setq radian--clipboard-last-copy text))

    (setq interprogram-paste-function #'radian--clipboard-paste)
    (setq interprogram-cut-function #'radian--clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

;;;; Mouse integration

;; On macOS, mouse integration works out of the box in windowed mode
;; but not terminal mode. The following code to fix it was based on
;; https://stackoverflow.com/a/8859057/3538165.
(radian-with-operating-system macOS
  (unless (display-graphic-p)

    ;; Enable basic mouse support (click and drag).
    (xterm-mouse-mode t)

    ;; Note that the reason for the next two functions is that
    ;; `scroll-down' and `scroll-up' scroll by a "near full screen" by
    ;; default, whereas we want a single line.

    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1))

    ;; Enable scrolling with the mouse wheel.
    (bind-keys ("<mouse-4>" . radian-scroll-down)
               ("<mouse-5>" . radian-scroll-up))))

;;; Candidate selection

;; Package `ivy' provides a user interface for choosing from a list of
;; options by typing a query to narrow the list, and then selecting
;; one of the remaining candidates. This offers a significant
;; improvement over the default Emacs interface for candidate
;; selection.
(use-package ivy
  ;; Use my fork until https://github.com/abo-abo/swiper/issues/1632
  ;; is fixed.
  :straight (:host github :repo "abo-abo/swiper"
                   :files (:defaults
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el")
                           "doc/ivy-help.org")
                   :fork (:repo "raxod502/swiper" :branch "fork/1"))
  :init/el-patch

  (defvar ivy-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [remap switch-to-buffer]
        'ivy-switch-buffer)
      (define-key map [remap switch-to-buffer-other-window]
        'ivy-switch-buffer-other-window)
      map)
    "Keymap for `ivy-mode'.")

  (define-minor-mode ivy-mode
    (el-patch-concat
      "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
      (el-patch-add
        "\n\nTo make it easier to lazy-load `ivy', this function
sets `completion-in-region-function' regardless of the value of
`ivy-do-completion-in-region'."))
    :group 'ivy
    :global t
    :keymap ivy-mode-map
    (el-patch-remove
      :lighter " ivy")
    (if ivy-mode
        (progn
          (setq completing-read-function 'ivy-completing-read)
          (el-patch-splice 2
            (when ivy-do-completion-in-region
              (setq completion-in-region-function 'ivy-completion-in-region))))
      (setq completing-read-function 'completing-read-default)
      (setq completion-in-region-function 'completion--in-region)))

  (ivy-mode +1)

  :config

  ;; With enough packages loaded, it is easy to get commands like
  ;; `describe-symbol' to offer more than 30,000 candidates. Allow
  ;; sorting in these cases.
  (setq ivy-sort-max-size 50000)

  :blackout t)

;; Package `ivy-hydra' provides the C-o binding for Ivy menus which
;; allows you to pick from a set of options for what to do with a
;; selected candidate.
(use-package ivy-hydra)

;; Package `counsel' provides purpose-built replacements for many
;; built-in Emacs commands that use enhanced configurations of `ivy'
;; to provide extra features.
(use-package counsel
  :init/el-patch

  (defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (el-patch-remove
                   (describe-function . counsel-describe-function)
                   (describe-variable . counsel-describe-variable))
                 (apropos-command . counsel-apropos)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)))
        (define-key map (vector 'remap (car binding)) (cdr binding)))
      map)
    (el-patch-concat
      "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements."
      (el-patch-add
        "\n\nBindings that are remapped by `helpful' have been removed.")))

  (defcustom counsel-mode-override-describe-bindings nil
    "Whether to override `describe-bindings' when `counsel-mode' is active."
    :group 'ivy
    :type 'boolean)

  (define-minor-mode counsel-mode
    "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}"
    :group 'ivy
    :global t
    :keymap counsel-mode-map
    (el-patch-remove
      :lighter " counsel")
    (if counsel-mode
        (progn
          (when (and (fboundp 'advice-add)
                     counsel-mode-override-describe-bindings)
            (advice-add #'describe-bindings :override #'counsel-descbinds))
          (define-key minibuffer-local-map (kbd "C-r")
            'counsel-minibuffer-history))
      (when (fboundp 'advice-remove)
        (advice-remove #'describe-bindings #'counsel-descbinds))))

  :init

  (counsel-mode +1)

  :bind* (;; Keybinding suggested by the documentation of Counsel, see
          ;; https://github.com/abo-abo/swiper.
          ("C-c k" . counsel-rg))
  :config/el-patch

  (defcustom counsel-rg-base-command
    (el-patch-concat
      "rg -S --no-heading --line-number --color never "
      (el-patch-add
        "-z --sort path ")
      "%s .")
    (el-patch-concat
      "Alternative to `counsel-ag-base-command' using ripgrep.

Note: don't use single quotes for the regex."
      (el-patch-add
        "\n\nSupport for searching compressed files and for
reporting results in a deterministic order has been added by
`el-patch'."))
    :type 'string
    :group 'ivy)

  :blackout t)

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `ivy-prescient' provides intelligent sorting and filtering
;; for candidates in Ivy menus.
(use-package ivy-prescient
  :demand t
  :after ivy
  :config

  ;; Use `prescient' for Ivy menus.
  (ivy-prescient-mode +1))

;;; Window management

(radian-defadvice radian--advice-keyboard-quit-minibuffer-first
    (keyboard-quit)
  :around keyboard-quit
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

;; Split windows horizontally (into tall subwindows) rather than
;; vertically (into wide subwindows) by default.
(el-patch-defun split-window-sensibly (&optional window)
  "Split WINDOW in a way suitable for `display-buffer'.
WINDOW defaults to the currently selected window.
If `split-height-threshold' specifies an integer, WINDOW is at
least `split-height-threshold' lines tall and can be split
vertically, split WINDOW into two windows one above the other and
return the lower window.  Otherwise, if `split-width-threshold'
specifies an integer, WINDOW is at least `split-width-threshold'
columns wide and can be split horizontally, split WINDOW into two
windows side by side and return the window on the right.  If this
can't be done either and WINDOW is the only window on its frame,
try to split WINDOW vertically disregarding any value specified
by `split-height-threshold'.  If that succeeds, return the lower
window.  Return nil otherwise.

By default `display-buffer' routines call this function to split
the largest or least recently used window.  To change the default
customize the option `split-window-preferred-function'.

You can enforce this function to not split WINDOW horizontally,
by setting (or binding) the variable `split-width-threshold' to
nil.  If, in addition, you set `split-height-threshold' to zero,
chances increase that this function does split WINDOW vertically.

In order to not split WINDOW vertically, set (or bind) the
variable `split-height-threshold' to nil.  Additionally, you can
set `split-width-threshold' to zero to make a horizontal split
more likely to occur.

Have a look at the function `window-splittable-p' if you want to
know how `split-window-sensibly' determines whether WINDOW can be
split."
  (let ((window (or window (selected-window))))
    (or (el-patch-let
            (($fst (and (window-splittable-p window)
                        ;; Split window vertically.
                        (with-selected-window window
                          (split-window-below))))
             ($snd (and (window-splittable-p window t)
                        ;; Split window horizontally.
                        (with-selected-window window
                          (split-window-right)))))
          (el-patch-swap $fst $snd)
          (el-patch-swap $snd $fst))
        (and
         ;; If WINDOW is the only usable window on its frame (it
         ;; is the only one or, not being the only one, all the
         ;; other ones are dedicated) and is not the minibuffer
         ;; window, try to split it s/vertically/horizontally
         ;; disregarding the value of `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-height-threshold 0))
           (when (window-splittable-p window)
             (with-selected-window window
               (split-window-below))))))))

;; Feature `windmove' provides keybindings S-left, S-right, S-up, and
;; S-down to move between windows. This is much more convenient and
;; efficient than using the default binding, C-x o, to cycle through
;; all of them in an essentially unpredictable order.
(use-feature windmove
  :demand t
  :config

  (windmove-default-keybindings))

;; Feature `winner' provides an undo/redo stack for window
;; configurations, with undo and redo being C-c left and C-c right,
;; respectively. (Actually "redo" doesn't revert a single undo, but
;; rather a whole sequence of them.) For instance, you can use C-x 1
;; to focus on a particular window, then return to your previous
;; layout with C-c left.
(use-feature winner
  :demand t
  :config

  (winner-mode +1))

;; Package `transpose-frame' provides simple commands to mirror,
;; rotate, and transpose Emacs windows: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame)

;; Package `buffer-move' provides simple commands to swap Emacs
;; windows: `buf-move-up', `buf-move-down', `buf-move-left',
;; `buf-move-right'.
(use-package buffer-move)

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-feature ibuffer
  :bind (([remap list-buffers] . ibuffer)))

;;; Finding files

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file" which normally
;; appears when you visit a symlinked file by the same name. (Doing
;; this isn't dangerous, as it will just redirect you to the existing
;; buffer.)
(setq find-file-suppress-same-file-warnings t)

;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(use-feature saveplace
  :demand t
  :config

  (save-place-mode +1)

  :config/el-patch

  (defun save-place-alist-to-file ()
    ;; No docstring on this function originally. To clarify, the reason
    ;; for this patch is to silence the save.
    (el-patch-add
      "Write `save-place-alist' to a file, but do so silently.")
    (let ((file (expand-file-name save-place-file))
          (coding-system-for-write 'utf-8))
      (with-current-buffer (get-buffer-create " *Saved Places*")
        (delete-region (point-min) (point-max))
        (when save-place-forget-unreadable-files
          (save-place-forget-unreadable-files))
        (insert (format ";;; -*- coding: %s -*-\n"
                        (symbol-name coding-system-for-write)))
        (let ((print-length nil)
              (print-level nil))
          (pp save-place-alist (current-buffer)))
        (let ((version-control
               (cond
                ((null save-place-version-control) nil)
                ((eq 'never save-place-version-control) 'never)
                ((eq 'nospecial save-place-version-control) version-control)
                (t
                 t))))
          (condition-case nil
              ;; Don't use write-file; we don't want this buffer to visit it.
              (write-region (point-min) (point-max) file
                            (el-patch-add nil 'nomsg))
            (file-error (message "Saving places: can't write %s" file)))
          (kill-buffer (current-buffer)))))))

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.
(use-package projectile
  ;; Why do we do this convoluted lazy-loading instead of just not
  ;; enabling `projectile-mode'? It's because setting up the
  ;; `counsel-projectile' keybindings requires for `projectile-mode'
  ;; to be enabled and for `projectile-command-map' and
  ;; `projectile-mode-map' to be defined.
  :init/el-patch

  (defcustom projectile-keymap-prefix nil
    "Projectile keymap prefix."
    :group 'projectile
    :type 'string)

  (defvar projectile-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
      (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
      (define-key map (kbd "4 C-o") #'projectile-display-buffer)
      (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
      (define-key map (kbd "4 D") #'projectile-dired-other-window)
      (define-key map (kbd "4 f") #'projectile-find-file-other-window)
      (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
      (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
      (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
      (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
      (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
      (define-key map (kbd "5 D") #'projectile-dired-other-frame)
      (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
      (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
      (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
      (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
      (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
      (define-key map (kbd "a") #'projectile-find-other-file)
      (define-key map (kbd "b") #'projectile-switch-to-buffer)
      (define-key map (kbd "C") #'projectile-configure-project)
      (define-key map (kbd "c") #'projectile-compile-project)
      (define-key map (kbd "d") #'projectile-find-dir)
      (define-key map (kbd "D") #'projectile-dired)
      (define-key map (kbd "e") #'projectile-recentf)
      (define-key map (kbd "E") #'projectile-edit-dir-locals)
      (define-key map (kbd "f") #'projectile-find-file)
      (define-key map (kbd "g") #'projectile-find-file-dwim)
      (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
      (define-key map (kbd "i") #'projectile-invalidate-cache)
      (define-key map (kbd "I") #'projectile-ibuffer)
      (define-key map (kbd "j") #'projectile-find-tag)
      (define-key map (kbd "k") #'projectile-kill-buffers)
      (define-key map (kbd "l") #'projectile-find-file-in-directory)
      (define-key map (kbd "m") #'projectile-commander)
      (define-key map (kbd "o") #'projectile-multi-occur)
      (define-key map (kbd "p") #'projectile-switch-project)
      (define-key map (kbd "q") #'projectile-switch-open-project)
      (define-key map (kbd "P") #'projectile-test-project)
      (define-key map (kbd "r") #'projectile-replace)
      (define-key map (kbd "R") #'projectile-regenerate-tags)
      (define-key map (kbd "s g") #'projectile-grep)
      (define-key map (kbd "s r") #'projectile-ripgrep)
      (define-key map (kbd "s s") #'projectile-ag)
      (define-key map (kbd "S") #'projectile-save-project-buffers)
      (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
      (define-key map (kbd "T") #'projectile-find-test-file)
      (define-key map (kbd "u") #'projectile-run-project)
      (define-key map (kbd "v") #'projectile-vc)
      (define-key map (kbd "V") #'projectile-browse-dirty-projects)
      (define-key map (kbd "x e") #'projectile-run-eshell)
      (define-key map (kbd "x i") #'projectile-run-ielm)
      (define-key map (kbd "x t") #'projectile-run-term)
      (define-key map (kbd "x s") #'projectile-run-shell)
      (define-key map (kbd "z") #'projectile-cache-current-file)
      (define-key map (kbd "<left>") #'projectile-previous-project-buffer)
      (define-key map (kbd "<right>") #'projectile-next-project-buffer)
      (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
      map)
    "Keymap for Projectile commands after `projectile-keymap-prefix'.")
  (fset 'projectile-command-map projectile-command-map)

  (defvar projectile-mode-map
    (let ((map (make-sparse-keymap)))
      (when projectile-keymap-prefix
        (define-key map projectile-keymap-prefix 'projectile-command-map))
      (easy-menu-define projectile-mode-menu map
        "Menu for Projectile"
        '("Projectile"
          ["Find file" projectile-find-file]
          ["Find file in known projects" projectile-find-file-in-known-projects]
          ["Find test file" projectile-find-test-file]
          ["Find directory" projectile-find-dir]
          ["Find file in directory" projectile-find-file-in-directory]
          ["Find other file" projectile-find-other-file]
          ["Switch to buffer" projectile-switch-to-buffer]
          ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
          ["Kill project buffers" projectile-kill-buffers]
          ["Save project buffers" projectile-save-project-buffers]
          ["Recent files" projectile-recentf]
          ["Previous buffer" projectile-previous-project-buffer]
          ["Next buffer" projectile-next-project-buffer]
          "--"
          ["Toggle project wide read-only" projectile-toggle-project-read-only]
          ["Edit .dir-locals.el" projectile-edit-dir-locals]
          "--"
          ["Switch to project" projectile-switch-project]
          ["Switch to open project" projectile-switch-open-project]
          ["Discover projects in directory" projectile-discover-projects-in-directory]
          ["Browse dirty projects" projectile-browse-dirty-projects]
          ["Open project in dired" projectile-dired]
          "--"
          ["Search in project (grep)" projectile-grep]
          ["Search in project (ag)" projectile-ag]
          ["Replace in project" projectile-replace]
          ["Multi-occur in project" projectile-multi-occur]
          "--"
          ["Run shell" projectile-run-shell]
          ["Run eshell" projectile-run-eshell]
          ["Run ielm" projectile-run-ielm]
          ["Run term" projectile-run-term]
          "--"
          ["Cache current file" projectile-cache-current-file]
          ["Invalidate cache" projectile-invalidate-cache]
          ["Regenerate [e|g]tags" projectile-regenerate-tags]
          "--"
          ["Configure project" projectile-configure-project]
          ["Compile project" projectile-compile-project]
          ["Test project" projectile-test-project]
          ["Run project" projectile-run-project]
          ["Repeat last external command" projectile-repeat-last-command]
          "--"
          ["Project info" projectile-project-info]
          ["About" projectile-version]))
      map)
    "Keymap for Projectile mode.")

  (define-minor-mode projectile-mode
    "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
    (el-patch-remove
      :lighter projectile--mode-line)
    :keymap projectile-mode-map
    :group 'projectile
    :require 'projectile
    :global t
    (cond
     (projectile-mode
      (el-patch-remove
        ;; setup the commander bindings
        (projectile-commander-bindings)
        ;; initialize the projects cache if needed
        (unless projectile-projects-cache
          (setq projectile-projects-cache
                (or (projectile-unserialize projectile-cache-file)
                    (make-hash-table :test 'equal))))
        (unless projectile-projects-cache-time
          (setq projectile-projects-cache-time
                (make-hash-table :test 'equal)))
        ;; load the known projects
        (projectile-load-known-projects)
        ;; update the list of known projects
        (projectile--cleanup-known-projects)
        (projectile-discover-projects-in-search-path)
        (add-hook 'find-file-hook 'projectile-find-file-hook-function)
        (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
        (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
        (ad-activate 'compilation-find-file)
        (ad-activate 'delete-file)))
     (el-patch-remove
       (t
        (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
        (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
        (ad-deactivate 'compilation-find-file)
        (ad-deactivate 'delete-file)))))

  :init

  ;; Defining the prefix key must be done manually as per
  ;; documentation for recent versions of Projectile, see
  ;; https://projectile.readthedocs.io/en/latest/installation/.
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode +1)

  :defer 1
  :config

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  (defun radian--projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))

  (put 'projectile-indexing-method 'safe-local-variable
       #'radian--projectile-indexing-method-p)

  :blackout t)

;; Package `counsel-projectile' provides alternate versions of
;; Projectile commands which use Counsel.
(use-package counsel-projectile
  :init/el-patch

  (defcustom counsel-projectile-key-bindings
    '((projectile-find-file        . counsel-projectile-find-file)
      (projectile-find-file-dwim   . counsel-projectile-find-file-dwim)
      (projectile-find-dir         . counsel-projectile-find-dir)
      (projectile-switch-to-buffer . counsel-projectile-switch-to-buffer)
      (projectile-grep             . counsel-projectile-grep)
      (projectile-ag               . counsel-projectile-ag)
      (projectile-ripgrep          . counsel-projectile-rg)
      (projectile-switch-project   . counsel-projectile-switch-project)
      (" "                         . counsel-projectile)
      ("si"                        . counsel-projectile-git-grep)
      ("Oc"                        . counsel-projectile-org-capture)
      ("Oa"                        . counsel-projectile-org-agenda))
    "Alist of counsel-projectile key bindings.

Each element is of the form \(KEY . DEF\) where KEY is either a
key sequence to bind in `projectile-command-map' or a projectile
command to remap in `projectile-mode-map', and DEF is the
counsel-projectile command to which KEY is remapped or bound."
    :type '(alist :key-type (choice (function :tag "Projectile command")
                                    key-sequence)
                  :value-type (function :tag "Counsel-projectile command"))
    :group 'counsel-projectile)

  (define-minor-mode counsel-projectile-mode
    "Toggle Counsel-Projectile mode on or off.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Counsel-Projectile mode turns on Projectile mode, thus enabling
all projectile key bindings, and adds the counsel-projectile key
bindings on top of them.

The counsel-projectile key bindings either remap existing
projectile commands to their counsel-projectile replacements or
bind keys to counsel-projectile commands that have no projectile
counterparts."
    :group 'counsel-projectile
    :require 'counsel-projectile
    :global t
    (cond
     (counsel-projectile-mode
      (projectile-mode)
      (dolist (binding counsel-projectile-key-bindings)
        (if (functionp (car binding))
            (define-key projectile-mode-map `[remap ,(car binding)] (cdr binding))
          (define-key projectile-command-map (car binding) (cdr binding)))))
     (t
      (dolist (binding counsel-projectile-key-bindings)
        (if (functionp (car binding))
            (define-key projectile-mode-map `[remap ,(car binding)] nil)
          (define-key projectile-command-map (car binding) nil)))
      (projectile-mode -1))))

  :init

  (counsel-projectile-mode +1)

  :config

  ;; Sort files using `prescient', instead of just showing them in
  ;; lexicographic order.
  (setq counsel-projectile-sort-files t))

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
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
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
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
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file          ; C-x C-w
               ))
  (advice-add fun :around #'radian--advice-find-file-create-directories))

(defmacro radian-register-dotfile
    (filename &optional keybinding pretty-filename)
  "Establish functions and keybindings to open a dotfile.

The FILENAME should be a path relative to the user's home
directory. Two interactive functions are created: one to find the
file in the current window, and one to find it in another window.

If KEYBINDING is non-nil, the first function is bound to that key
sequence after it is prefixed by \"M-P e\", and the second
function is bound to the same key sequence, but prefixed instead
by \"M-P o\".

This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\" and KEYBINDING is \"e i\". Then
`radian-register-dotfile' will create the interactive functions
`radian-find-init-el' and `radian-find-init-el-other-window', and
it will bind them to the key sequences \"M-P e e i\" and \"M-P o
e i\" respectively.

If PRETTY-FILENAME, a string, is non-nil, then it will be used in
place of \"init-el\" in this example. Otherwise, that string will
be generated automatically from the basename of FILENAME.

To pass something other than a literal string as FILENAME,
unquote it using a comma."
  (when (and (listp filename) (eq (car filename) '\,))
    (setq filename (eval (cadr filename))))
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (full-filename (expand-file-name filename "~"))
         (defun-name (intern
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "radian-find-"
                        (or pretty-filename
                            (replace-regexp-in-string
                             "[^a-z0-9]" "-"
                             bare-filename))))))
         (defun-other-window-name
           (intern
            (concat (symbol-name defun-name)
                    "-other-window")))
         (docstring (format "Edit file %s."
                            full-filename))
         (docstring-other-window
          (format "Edit file %s, in another window."
                  full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (find-file ,full-filename)))
         (defun-other-window-form
           `(defun ,defun-other-window-name ()
              ,docstring-other-window
              (interactive)
              (find-file-other-window ,full-filename)))
         (full-keybinding
          (when keybinding
            (radian-join-keys "e" keybinding)))
         (full-other-window-keybinding
          (radian-join-keys "o" keybinding)))
    `(progn
       ,defun-form
       ,defun-other-window-form
       ,@(when full-keybinding
           `((radian-bind-key ,full-keybinding #',defun-name)))
       ,@(when full-other-window-keybinding
           `((radian-bind-key
              ,full-other-window-keybinding
              #',defun-other-window-name)))
       ;; Return the symbols for the two functions defined.
       (list ',defun-name ',defun-other-window-name))))

;; Now we register shortcuts to files relevant to Radian.

(radian-register-dotfile ,radian-directory "r a" "radian-repo")

;; Emacs
(radian-register-dotfile
 ,(expand-file-name "init.el" user-emacs-directory)
 "e i")
(radian-register-dotfile
 ,(expand-file-name "emacs/radian.el" radian-directory)
 "e r")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian.el" user-emacs-directory)
 "e v" "radian-versions-el")
(radian-register-dotfile
 ,(expand-file-name "init.local.el" user-emacs-directory) "e l")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian-local.el" user-emacs-directory)
 "e V" "radian-local-versions-el")

;; Git
(radian-register-dotfile ".gitconfig" "g c")
(radian-register-dotfile ".gitexclude" "g e")
(radian-register-dotfile ".gitconfig.local" "g l")

;; Leiningen
(radian-register-dotfile ".lein/profiles.clj" "l p")

;; Shell
(radian-register-dotfile ".profile" "p r")
(radian-register-dotfile ".profile.local" "p l")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.local" "z l")

;;; Saving files

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

;;; Editing
;;;; Text formatting

(defun radian-reverse-region-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapcar
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

(defun radian--auto-fill-disable ()
  "Disable `auto-fill-mode' in the current buffer."
  (auto-fill-mode -1))

(define-minor-mode radian-fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  nil nil nil
  (if radian-fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode radian-fix-whitespace-global-mode
  radian-fix-whitespace-mode radian-fix-whitespace-mode)

(radian-fix-whitespace-global-mode +1)

(put 'radian-fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace in various special ways.
(use-feature whitespace
  :init

  (define-minor-mode radian-highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    nil nil nil
    (if radian-highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))

  :blackout t)

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
(use-feature outline
  :demand t
  :config

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;;;; Kill and yank

(radian-defadvice radian--advice-stop-kill-at-whitespace
    (kill-line &rest args)
  :around kill-line
  "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you press \\[kill-line] when point is
followed by some whitespace and then a newline. Without this
advice, \\[kill-line] will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
\\[kill-line] will kill just the whitespace, and another
invocation will kill the newline."
  (let ((show-trailing-whitespace t))
    (apply kill-line args)))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

(radian-defadvice radian--advice-disallow-password-copying (func &rest args)
  :around read-passwd
  "Don't allow copying a password to the kill ring."
  (cl-letf (((symbol-function #'kill-new) #'ignore)
            ((symbol-function #'kill-append) #'ignore))
    (apply func args)))

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed. (Otherwise, in both cases the
;; selection is deselected and the normal function of the key is
;; performed.)
(use-feature delsel
  :demand t
  :config

  (delete-selection-mode +1))

;;;; Undo/redo

;; Package `undo-tree' replaces the default Emacs undo system, which
;; is poorly designed and hard to use, with a much more powerful
;; tree-based system. In basic usage, you don't even have to think
;; about the tree, because it acts like a conventional undo/redo
;; system. Bindings are C-/, M-/, and C-x u.
(use-package undo-tree
  :demand t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is
         ;; bound to C-_ and C-/, and `undo-tree-redo' is bound to
         ;; M-_. It's logical to also bind M-/ to `undo-tree-redo'.
         ;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :map undo-tree-map
         ("M-/" . undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  (radian-defadvice radian--advice-suppress-undo-tree-buffer-modified-message
      (undo-tree-load-history &rest args)
    :around undo-tree-load-history
    "Suppress the annoying message saying undo history could not be loaded.
Normally, this message is printed when undo history could not be
loaded since the file was changed outside of Emacs."
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil)

  :blackout t)

;;;; Navigation

;; Feature `subword' provides a minor mode which causes the
;; `forward-word' and `backward-word' commands to stop at
;; capitalization changes within a word, so that you can step through
;; the components of CamelCase symbols one at a time.
(use-feature subword
  :demand t
  :config

  (global-subword-mode +1)

  :blackout t)

(radian-defadvice radian--advice-allow-unpopping-mark
    (set-mark-command &optional arg)
  :around set-mark-command
  "Allow \\[set-mark-command] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[set-mark-command]), then it will step in
the reverse direction from \\[universal-argument]
\\[set-mark-command]."
  ;; Based on https://stackoverflow.com/a/14539202/3538165.
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      ;; If we don't have any marks set, no-op.
      (when mark-ring
        ;; I can't remember how this code works. Sorry.
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring)))))
    ;; If no prefix argument, or prefix argument is nonnegative, defer
    ;; to the original behavior.
    (funcall set-mark-command arg)))

(radian-defadvice radian--advice-allow-unpopping-global-mark
    (pop-global-mark &optional arg)
  :around pop-global-mark
  "Allow \\[pop-global-mark] to step in reverse.
If a negative prefix argument is given (like
\\[negative-argument] \\[pop-global-mark]), then it will step in
the reverse direction from \\[pop-global-mark]."
  (interactive "P")
  (if arg
      ;; Tweaked from the implementation of `pop-global-mark'.
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

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(use-feature bookmark
  :config

  (radian-defadvice radian--advice-bookmark-silence (&rest _)
    :override bookmark-maybe-message
    "Silence useless messages from bookmark.el."))

;;;;; Definition location

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(use-package dumb-jump
  :init

  (dumb-jump-mode +1)

  :bind (:map dumb-jump-mode-map
              ("M-Q" . dumb-jump-quick-look)))

;;;; Find and replace

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs))

;; Feature `isearch' provides a basic and fast mechanism for jumping
;; forward or backward to occurrences of a given search string.
(use-feature isearch
  :config

  ;; Eliminate the 0.25s idle delay for isearch highlighting, as in my
  ;; opinion it usually produces a rather disjointed and distracting
  ;; UX.
  (setq isearch-lazy-highlight-initial-delay 0))

;; Package `swiper' provides an alternative to `isearch' which instead
;; uses `ivy' to display and select from the results.
(use-package swiper
  :init

  (radian-bind-key "g" #'swiper)

  :config

  ;; Use only one color for subgroups in Swiper highlighting.
  (setq swiper-faces '(swiper-match-face-1
                       swiper-match-face-2
                       swiper-match-face-2
                       swiper-match-face-2)))

;;; Electricity: automatic things
;;;; Autorevert

;; Feature `autorevert' allows the use of file-watchers or polling in
;; order to detect when the file visited by a buffer has changed, and
;; optionally reverting the buffer to match the file (unless it has
;; unsaved changes).
(use-feature autorevert
  :defer 2
  :init

  (defun radian--autorevert-silence ()
    "Silence messages from `auto-revert-mode' in the current buffer."
    (setq-local auto-revert-verbose nil))

  :config/el-patch

  (defun auto-revert-buffers ()
    (el-patch-concat
      "Revert buffers as specified by Auto-Revert and Global Auto-Revert Mode.

Should `global-auto-revert-mode' be active all file buffers are checked.

Should `auto-revert-mode' be active in some buffers, those buffers
are checked.

Non-file buffers that have a custom `revert-buffer-function' and
`buffer-stale-function' are reverted either when Auto-Revert
Mode is active in that buffer, or when the variable
`global-auto-revert-non-file-buffers' is non-nil and Global
Auto-Revert Mode is active.

This function stops whenever there is user input.  The buffers not
checked are stored in the variable `auto-revert-remaining-buffers'.

To avoid starvation, the buffers in `auto-revert-remaining-buffers'
are checked first the next time this function is called.

This function is also responsible for removing buffers no longer in
Auto-Revert Mode from `auto-revert-buffer-list', and for canceling
the timer when no buffers need to be checked."
      (el-patch-add
        "\n\nOnly currently displayed buffers are reverted."))

    (setq auto-revert-buffers-counter
          (1+ auto-revert-buffers-counter))

    (save-match-data
      (let ((bufs (el-patch-wrap 2
                    (cl-remove-if-not
                     #'get-buffer-window
                     (if global-auto-revert-mode
                         (buffer-list)
                       auto-revert-buffer-list))))
            remaining new)
        ;; Partition `bufs' into two halves depending on whether or not
        ;; the buffers are in `auto-revert-remaining-buffers'.  The two
        ;; halves are then re-joined with the "remaining" buffers at the
        ;; head of the list.
        (dolist (buf auto-revert-remaining-buffers)
          (if (memq buf bufs)
              (push buf remaining)))
        (dolist (buf bufs)
          (if (not (memq buf remaining))
              (push buf new)))
        (setq bufs (nreverse (nconc new remaining)))
        (while (and bufs
                    (not (and auto-revert-stop-on-user-input
                              (input-pending-p))))
          (let ((buf (car bufs)))
            (with-current-buffer buf
              (if (buffer-live-p buf)
                  (progn
                    ;; Test if someone has turned off Auto-Revert Mode
                    ;; in a non-standard way, for example by changing
                    ;; major mode.
                    (if (and (not auto-revert-mode)
                             (not auto-revert-tail-mode)
                             (memq buf auto-revert-buffer-list))
                        (auto-revert-remove-current-buffer))
                    (when (auto-revert-active-p)
                      ;; Enable file notification.
                      (when (and auto-revert-use-notify
                                 (not auto-revert-notify-watch-descriptor))
                        (auto-revert-notify-add-watch))
                      (auto-revert-handler)))
                ;; Remove dead buffer from `auto-revert-buffer-list'.
                (auto-revert-remove-current-buffer))))
          (setq bufs (cdr bufs)))
        (setq auto-revert-remaining-buffers bufs)
        ;; Check if we should cancel the timer.
        (when (and (not global-auto-revert-mode)
                   (null auto-revert-buffer-list))
          (cancel-timer auto-revert-timer)
          (setq auto-revert-timer nil)))))

  :config

  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
  ;; We have to do this before turning on `auto-revert-mode' for the
  ;; change to take effect. (Note that if we set this variable using
  ;; `customize-set-variable', all it does is toggle the mode off and
  ;; on again to make the change take effect, so that way is dumb.)
  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  ;; Auto-revert all buffers, not only file-visiting buffers. The
  ;; docstring warns about potential performance problems but this
  ;; should not be an issue since we only revert visible buffers.
  (setq global-auto-revert-non-file-buffers t)

  ;; Since we automatically revert all visible buffers after one
  ;; second, there's no point in asking the user whether or not they
  ;; want to do it when they find a file. This disables that prompt.
  (setq revert-without-query '(".*"))

  :blackout auto-revert-mode)

;;;; Automatic delimiter pairing

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(use-package smartparens
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.
  (add-to-list 'sp-ignore-modes-list #'org-mode)
  (add-to-list 'sp-ignore-modes-list #'org-agenda-mode)

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  ;; When pressing RET after a newly entered pair, add an extra
  ;; newline and indent. See
  ;; https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312.
  ;;
  ;; What is currently here is to be considered a hack.

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (dolist (mode '(c-mode c++-mode css-mode objc-mode java-mode
                         js2-mode json-mode
                         python-mode sh-mode web-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (dolist (mode '(js2-mode json-mode python-mode web-mode))
    (sp-local-pair mode "[" nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (dolist (mode '(python-mode))
    (sp-local-pair mode "(" nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>")))
    (sp-local-pair mode "\"\"\"" "\"\"\"" :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  :blackout t)

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 3
  :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (global-company-mode +1)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

;;;; Automatic display of documentation in the minibuffer

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
(use-feature eldoc
  :demand t
  :config

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!
  (radian-defadvice radian--advice-disable-eldoc-on-flycheck
      (&rest _)
    :after-while eldoc-display-message-no-interference-p
    "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
    (not (and (bound-and-true-p flycheck-mode)
              (flycheck-overlay-errors-at (point)))))

  :blackout t)

;;;; Automatic syntax checking

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting, or more generally syntax checking. It comes
;; with a large number of checkers pre-defined, and other packages
;; define more.
(use-package flycheck
  :defer 4
  :init

  (defun radian--flycheck-disable-checkers (&rest checkers)
    "Disable the given Flycheck syntax CHECKERS, symbols.
This function affects only the current buffer, and neither causes
nor requires Flycheck to be loaded."
    (unless (boundp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (make-local-variable 'flycheck-disabled-checkers)
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))

  :bind-keymap (("C-c !" . flycheck-command-map))

  :config

  (global-flycheck-mode +1)

  (dolist (name '("python" "python2" "python3"))
    (add-to-list 'safe-local-variable-values
                 `(flycheck-python-pycompile-executable . ,name)))

  ;; Run a syntax check when changing buffers, just in case you
  ;; modified some other files that impact the current one. See
  ;; https://github.com/flycheck/flycheck/pull/1308.
  (add-to-list 'flycheck-check-syntax-automatically 'idle-buffer-switch)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to only briefly. This allows "refreshing" the syntax
  ;; check state for several buffers quickly after e.g. changing a
  ;; config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors in the echo area after only 0.2 seconds, not 0.9.
  (setq flycheck-display-errors-delay 0.2)

  :config

  (radian-bind-key "p" #'flycheck-previous-error)
  (radian-bind-key "n" #'flycheck-next-error)

  :blackout t)

;;;; Indentation

;; Don't use tabs for indentation. Use only spaces. Frankly, the fact
;; that `indent-tabs-mode' is even *available* as an *option* disgusts
;; me, much less the fact that it's *enabled* by default (meaning that
;; *both* tabs and spaces are used at the same time).
(setq-default indent-tabs-mode nil)

(defun radian-indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (indent-region beginning end)))))))

(bind-key* "C-M-q" #'radian-indent-defun)

;;;; Snippet expansion

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations. We prefer to use `yasnippet' instead, though.
(use-feature abbrev
  :blackout t)

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. It is also used by
;; `clj-refactor' for some of its refactorings.
(use-package yasnippet
  :bind (:map yas-minor-mode-map

              ;; Disable TAB from expanding snippets, as I don't use it and
              ;; it's annoying.
              ("TAB" . nil)
              ("<tab>" . nil))
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (use-feature company
    :config

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key' and
    ;; `define-key'. It's a hack, and I'd like to find a built-in
    ;; function that accomplishes the same thing while taking care of
    ;; any edge cases I might have missed in this ad-hoc solution.
    (defun radian--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar radian--yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
         (lambda (event company-cmd)
           (let* ((event (radian--yasnippet-normalize-event event))
                  (yas-cmd (lookup-key yas-keymap event)))
             ;; Here we use an extended menu item with the
             ;; `:filter' option, which allows us to dynamically
             ;; decide which command we want to run when a key is
             ;; pressed.
             (define-key keymap event
               `(menu-item
                 nil ,company-cmd :filter
                 (lambda (cmd)
                   ;; There doesn't seem to be any obvious
                   ;; function from Company to tell whether or not
                   ;; a completion is in progress (à la
                   ;; `company-explicit-action-p'), so I just
                   ;; check whether or not `company-my-keymap' is
                   ;; defined, which seems to be good enough.
                   (if company-my-keymap
                       ',company-cmd
                     ',yas-cmd))))))
         company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (radian-defadvice radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      :around yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;; Language support
;;;; Text-based languages

;; Feature `text-mode' provides a major mode for editing plain text.
(use-feature text-mode
  :config

  (add-hook 'text-mode-hook #'auto-fill-mode)

  (radian-defhook radian--flycheck-text-setup ()
    text-mode-hook
    "Disable some Flycheck checkers for plain text."
    (radian--flycheck-disable-checkers 'proselint)))

;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(use-feature lisp-mode
  :init

  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;;;; AppleScript
;; https://developer.apple.com/library/content/documentation/AppleScript/Conceptual/AppleScriptLangGuide/introduction/ASLR_intro.html

;; Package `apples-mode' provides a major mode for AppleScript.
(use-package apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;;;; C, C++, Objective-C, Java
;; https://en.wikipedia.org/wiki/C_(programming_language)
;; https://en.wikipedia.org/wiki/C%2B%2B
;; https://en.wikipedia.org/wiki/Objective-C
;; https://en.wikipedia.org/wiki/Java_(programming_language)

;; Feature `cc-mode' provides major modes for C, C++, Objective-C, and
;; Java.
(use-feature cc-mode
  :config

  (radian-defadvice radian--advice-inhibit-c-submode-indicators (&rest _)
    :override c-update-modeline
    "Unconditionally inhibit CC submode indicators in the mode lighter.")

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
  ;; if (condition)
  ;; {
  ;;   statement;
  ;; }
  ;;
  ;; We do this by defining a custom style that is based on BSD, and
  ;; then overriding the indentation (which is set to 8 spaces by
  ;; default). This style is only used for languages which do not have
  ;; a more specific style set in `c-default-style'.
  (c-add-style "radian-bsd"
               '("bsd"
                 (c-basic-offset . 2)))
  (setf (map-elt c-default-style 'other) "radian-bsd")

  (put 'c-default-style 'safe-local-variable #'stringp)

  (defun radian--flycheck-c/c++-setup ()
    "Disable some Flycheck checkers for CC modes.
This function is for use in `c-mode-hook' and `c++-mode-hook'."
    ;; These checkers are usually not accurate enough to find proper
    ;; headers and such. Disable them by default.
    (radian--flycheck-disable-checkers 'c/c++-clang 'c/c++-gcc))

  (add-hook 'c-mode-hook #'radian--flycheck-c/c++-setup)
  (add-hook 'c++-mode-hook #'radian--flycheck-c/c++-setup))

;; Package `irony-mode' provides a framework to use libclang to get
;; semantic information about C, C++, and Objective-C code. Such
;; information can be used by Company, ElDoc, or other packages.
(use-package irony
  :init

  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)

  :config

  ;; This tells `irony-mode' to discover compile options in a
  ;; .clang_complete file or another similar format automatically. See
  ;; [1] for further discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/irony-mode#configuration
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  :blackout t)

;; Package `company-irony' provides a Company backend that uses Irony
;; to complete symbols. See also `company-irony-c-headers'.
(use-package company-irony
  :demand t
  :after (:all company irony))

;; Package `company-irony-c-headers' provides a Company backend that
;; uses Irony to complete header file #includes. See also
;; `company-irony'.
(use-package company-irony-c-headers
  :demand t
  :after company-irony
  :config

  (radian-defhook radian--company-irony-setup ()
    irony-mode-hook
    "Configure Company to use Irony as a backend."
    (setq-local company-backends
                (cons (list #'company-irony #'company-irony-c-headers)
                      radian--company-backends-global))))

;; Package `irony-eldoc' provides an ElDoc backend that uses Irony to
;; display function signatures.
(use-package irony-eldoc
  :demand t
  :after irony
  :config

  (add-hook 'irony-mode-hook #'irony-eldoc))

;; Package `flycheck-irony' provides a Flycheck syntax checker that
;; uses Irony to display compilation errors and warnings.
(use-package flycheck-irony
  :demand t
  :after (:all flycheck irony)
  :config

  ;; This setup is global, so no need to run it on a mode hook.
  (flycheck-irony-setup)

  ;; Also use cppcheck. See [1] for discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/flycheck-irony/issues/9
  (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

;;;; Clojure
;; https://clojure.org/

;; Package `clojure-mode' provides a major mode for Clojure.
(use-package clojure-mode
  :config/el-patch

  ;; `clojure-mode' does not correctly identify the docstrings of
  ;; protocol methods as docstrings, and as such electric indentation
  ;; does not work for them. Additionally, when you hack a
  ;; clojure.core function, such as defonce or defrecord, to provide
  ;; docstring functionality, those docstrings are (perhaps rightly,
  ;; but annoyingly) not recognized as docstrings either. However,
  ;; there is an easy way to get electric indentation working for all
  ;; potential docstrings: simply tell `clojure-mode' that *all*
  ;; strings are docstrings. This will not change the font locking,
  ;; because for some weird reason `clojure-mode' determines whether
  ;; you're in a docstring by the font color instead of the other way
  ;; around. Note that this will cause electric indentation by two
  ;; spaces in *all* multiline strings, but since there are not very
  ;; many non-docstring multiline strings in Clojure this is not too
  ;; inconvenient.
  ;;
  ;; Unfortunately, `clojure-in-docstring-p' is defined as an inline
  ;; function, so after changing it, we also have to replace
  ;; `clojure-indent-line'. That is done in an advice in the
  ;; `radian-clojure-strings-as-docstrings-mode' minor mode.

  (defsubst (el-patch-swap clojure-in-docstring-p
                           radian--clojure-in-string-p)
    ()
    (el-patch-concat
      "Check whether point is in "
      (el-patch-swap "a docstring" "any type of string")
      ".")
    (let ((ppss (syntax-ppss)))
      ;; are we in a string?
      (when (nth 3 ppss)
        ;; check font lock at the start of the string
        ((el-patch-swap eq memq)
         (get-text-property (nth 8 ppss) 'face)
         (el-patch-wrap 1
           ('font-lock-string-face 'font-lock-doc-face))))))

  (defun (el-patch-swap clojure-indent-line
                        radian--advice-clojure-strings-as-docstrings)
      ()
    (el-patch-concat
      "Indent current line as Clojure code."
      (el-patch-add
        "\n\nThis is an `:override' advice for `clojure-indent-line'."))
    (if (el-patch-swap
          (clojure-in-docstring-p)
          (radian--clojure-in-string-p))
        (save-excursion
          (beginning-of-line)
          (when (and (looking-at "^\\s-*")
                     (<= (string-width (match-string-no-properties 0))
                         (string-width (clojure-docstring-fill-prefix))))
            (replace-match (clojure-docstring-fill-prefix))))
      (lisp-indent-line)))

  :config

  ;; Customize indentation like this:
  ;;
  ;; (some-function
  ;;   argument
  ;;   argument)
  ;;
  ;; (some-function argument
  ;;                argument)
  ;;
  ;; (-> foo
  ;;   thread
  ;;   thread)
  ;;
  ;; (->> foo
  ;;   thread
  ;;   thread)
  ;;
  ;; (:keyword
  ;;   map)

  (setq clojure-indent-style :align-arguments)

  ;; Ideally, we would be able to set the identation rules for *all*
  ;; keywords at the same time. But until we figure out how to do
  ;; that, we just have to deal with every keyword individually. See
  ;; https://github.com/raxod502/radian/issues/26.
  (radian-protect-macros
    (define-clojure-indent
      (-> 1)
      (->> 1)
      (:import 0)
      (:require 0)
      (:use 0)))

  (define-minor-mode radian-clojure-strings-as-docstrings-mode
    "Treat all Clojure strings as docstrings.
You want to turn this on if you want to treat strings like
docstrings even though they technically are not, and you want to
turn it off if you have multiline strings that are not
docstrings."
    nil nil nil
    (if radian-clojure-strings-as-docstrings-mode
        (advice-add #'clojure-indent-line :override
                    #'radian--advice-clojure-strings-as-docstrings)
      (advice-remove #'clojure-indent-line
                     #'radian--advice-clojure-strings-as-docstrings))))

;; Package `cider' provides integrated Clojure and ClojureScript REPLs
;; directly in Emacs, a Company backend that uses a live REPL
;; connection to retrieve completion candidates, and documentation and
;; source lookups for Clojure code.
(use-package cider
  :config

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.
  ;;
  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.
  ;;
  ;; Thanks to malabarba on Clojurians Slack for providing the
  ;; following code:

  (radian-defhook radian--cider-dump-nrepl-server-log ()
    cider-connected-hook
    "Copy contents of *nrepl-server* to beginning of *cider-repl*."
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string)))))

  ;; Use the :emacs profile defined in profiles.clj. This enables lots
  ;; of cool extra features in the REPL.
  (when (radian-managed-p "~/.lein/profiles.clj")
    (setq cider-lein-parameters "with-profile +emacs repl :headless"))

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
  ;; anything or be prepared for your cursor to suddenly shift buffers
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

  :blackout t)

;; Package `clj-refactor' provides automated refactoring commands for
;; Clojure code.
(use-package clj-refactor
  ;; Use my fork which has support for automatically sorting project
  ;; dependencies after adding them to the project.clj.
  :straight (:host github :repo "clojure-emacs/clj-refactor.el"
                   :fork (:repo "raxod502/clj-refactor.el" :branch "fork/3")
                   :files (:defaults "CHANGELOG.md"))
  :init

  (radian-defhook radian--clj-refactor-enable ()
    clojure-mode-hook
    "Enable `clj-refactor' mode properly.
This means that `yas-minor-mode' also needs to be enabled, and
the `clj-refactor' keybindings need to be installed."
    (clj-refactor-mode +1)
    (yas-minor-mode +1)
    (cljr-add-keybindings-with-prefix "C-c RET"))

  :config

  ;; Make clj-refactor show its messages right away, instead of
  ;; waiting for you to do another command.

  (radian-defadvice radian--advice-clj-refactor-message-eagerly (&rest args)
    :override cljr--post-command-message
    "Make `clj-refactor' show messages right away.
Otherwise, it waits for you to do another command, and then
overwrites the message from *that* command."
    (apply #'message args))

  ;; Automatically sort project dependencies after changing them.
  (setq cljr-auto-sort-project-dependencies t)

  ;; Don't print a warning when starting a REPL outside of project
  ;; context.
  (setq cljr-suppress-no-project-warning t)

  :blackout t)

;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.
(use-package go-mode)

;;;; Haskell
;; https://www.haskell.org/

;; Package `haskell-mode' provides a major mode and REPL integration
;; for Haskell.
(use-package haskell-mode
  :config

  ;; Enable REPL integration.
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  (radian-defadvice radian--advice-haskell-fix-back-to-indentation
      (back-to-indentation)
    :around back-to-indentation
    "Fix `back-to-indentation' in `literate-haskell-mode'.
Otherwise, it just moves point to column 0, which is wrong.

This works around an upstream bug; see
https://github.com/haskell/haskell-mode/issues/1594."
    (if (derived-mode-p 'literate-haskell-mode)
        (progn
          (beginning-of-line 1)
          (when-let ((c (char-after)))
            (when (= c ? )
              (forward-char)))
          (skip-syntax-forward " " (line-end-position))
          (backward-prefix-chars))
      (funcall back-to-indentation))))

;; Feature `haskell' from package `haskell-mode' is a meta-feature
;; which includes many other features from the package, and also for
;; some reason is where `interactive-haskell-mode' is defined.
(use-feature haskell
  :blackout interactive-haskell-mode)

;; Feature `haskell-customize' from package `haskell-mode' defines the
;; user options for the package.
(use-feature haskell-customize
  :config

  ;; Disable in-buffer underlining of errors and warnings, since we
  ;; already have them from Flycheck.
  (setq haskell-process-show-overlays nil)

  ;; Work around upstream bug, see
  ;; https://github.com/haskell/haskell-mode/issues/1553.

  (setq haskell-process-args-ghci
        '("-ferror-spans" "-fshow-loaded-modules"))

  (setq haskell-process-args-cabal-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  (setq haskell-process-args-stack-ghci
        '("--ghci-options=-ferror-spans -fshow-loaded-modules"
          "--no-build" "--no-load"))

  (setq haskell-process-args-cabal-new-repl
        '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

  ;; Allow `haskell-mode' to use Stack with the global project instead
  ;; of trying to invoke GHC directly, if not inside any sort of
  ;; project.
  (setq haskell-process-type 'stack-ghci))

;; Package `hindent' provides a way to invoke the Haskell code
;; formatter of the same name as a `fill-paragraph' replacement. You
;; need to install the hindent(1) utility for this to work.
(use-package hindent
  :demand t
  :after haskell-mode
  :config

  (radian-defhook radian--hindent-enable-maybe (&optional arg)
    haskell-mode-hook
    "Enable `hindent-mode' if not in `literate-haskell-mode'.
ARG is passed to `hindent-mode' toggle function."
    ;; Don't enable `hindent-mode' in `literate-haskell-mode'. See
    ;; https://github.com/commercialhaskell/hindent/issues/496.
    (unless (derived-mode-p 'literate-haskell-mode)
      (hindent-mode arg)))

  :blackout t)

;;;; HTML
;; https://www.w3.org/TR/html5/

;; Package `web-mode' provides a major mode for HTML and related
;; templating languages (PHP, ASP, Handlebars, etc.). It replaces the
;; major mode `html-mode' which comes with Emacs.
(use-package web-mode
  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at http://web-mode.org/ as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :config

  ;; Indent by two spaces by default.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t))

;;;; JavaScript
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript.
(use-feature js
  :config

  ;; Indent by two spaces by default.
  (setq js-indent-level 2))

;; Package `js2-mode' provides a better major mode for JavaScript. It
;; builds on the major mode `js-mode' which comes with Emacs.
(use-package js2-mode
  ;; The `js2-mode' package does not come with `auto-mode-alist'
  ;; autoloads.
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :config

  ;; Treat shebang lines (e.g. for node) correctly.
  (setq js2-skip-preprocessor-directives t)

  ;; Replace the mode lighters. By default they are Javascript-IDE and
  ;; JSX-IDE, which are not only improperly capitalized but also
  ;; excessively wordy.
  :blackout ((js2-mode . "JavaScript")
             (js2-jsx-mode . "JSX")))

;; Package `tern' provides a static code analyzer for JavaScript. This
;; includes ElDoc and jump-to-definition out of the box.
(use-package tern
  :demand t
  :after js2-mode
  :config

  (add-hook 'js2-mode-hook #'tern-mode)

  :blackout t)

;; Package `company-tern' provides a Company backend which uses Tern.
(use-package company-tern
  :demand t
  :after (:all js2-mode company tern)
  :config

  (radian-defhook radian--company-tern-enable ()
    js2-mode-hook
    "Enable `company-tern' in the current buffer."
    (setq-local company-backends
                (cons 'company-tern radian--company-backends-global))))

;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(use-package markdown-mode

  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         :map markdown-mode-style-map
              ("C-p" . markdown-insert-pre)
              ("C-q" . markdown-insert-blockquote))
  :config

  (radian-defhook radian--flycheck-markdown-setup ()
    markdown-mode-hook
    "Disable some Flycheck checkers for Markdown."
    (radian--flycheck-disable-checkers
     'markdown-markdownlint-cli
     'markdown-mdl
     'proselint))

  (radian-defadvice radian--disable-markdown-metadata-fontification (&rest _)
    :override markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))

;;;; Python
;; https://www.python.org/

;; Feature `python' provides a major mode for Python.
(use-feature python
  :config

  ;; The only consistent style.
  (setq python-fill-docstring-style 'django)

  (radian-defhook radian--python-fix-outline-mode-config ()
    python-mode-hook
    "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp))

  (radian-defhook radian--python-no-reindent-on-colon ()
    python-mode-hook
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars)))

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (radian-defhook radian--python-use-correct-flycheck-executables ()
    python-mode-hook
    "Use the correct Python executables for Flycheck."
    (let ((executable python-shell-interpreter))
      (save-excursion
        (save-match-data
          (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                    (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
            (setq executable (substring-no-properties (match-string 1))))))
      ;; Try to compile using the appropriate version of Python for
      ;; the file.
      (setq-local flycheck-python-pycompile-executable executable)
      ;; We might be running inside a virtualenv, in which case the
      ;; modules won't be available. But calling the executables
      ;; directly will work.
      (setq-local flycheck-python-pylint-executable "pylint")
      (setq-local flycheck-python-flake8-executable "flake8"))))

;; Package `elpy' provides a language server for Python, including
;; integration with most other packages that need to draw information
;; from it (e.g. Company).
(use-package elpy
  :demand t
  :after python
  :config

  ;; Don't highlight indentation levels, as it looks rather weird.
  (setq elpy-modules (remq 'elpy-module-highlight-indentation elpy-modules))

  ;; Don't use Flymake, since we use Flycheck instead.
  (setq elpy-modules (remq 'elpy-module-flymake elpy-modules))

  ;; Use the correct version of Python.
  (setq elpy-rpc-python-command python-shell-interpreter)

  (elpy-enable)

  :blackout t)

;;;; ReST
;; http://docutils.sourceforge.net/rst.html

;; Feature `rst-mode' provides a major mode for ReST.
(use-feature rst-mode
  :config

  (radian-defhook radian--flycheck-rst-setup ()
    rst-mode-hook
    "If inside Sphinx project, disable the `rst' Flycheck checker.
This prevents it from signalling spurious errors. See also
https://github.com/flycheck/flycheck/issues/953."
    (when (locate-dominating-file default-directory "conf.py")
      (radian--flycheck-disable-checkers 'rst))))

;;;; Ruby
;; https://www.ruby-lang.org/

;; Package `robe' provides a language server for Ruby which draws
;; information for autocompletions and source code navigation from a
;; live REPL in the project context. Start it with `robe-start'.
(use-package robe
  :init

  (add-hook 'ruby-mode-hook #'robe-mode)

  :blackout t)

;; Package `ruby-electric' allows you to have Emacs insert a paired
;; "end" when you type "do", and analogously for other paired
;; keywords.
(use-package ruby-electric
  :init/el-patch

  ;; We already have paired delimiter support from Smartparens.
  ;; However, `ruby-electric' provides its own copy of this
  ;; functionality, in a less optimal way. (In particular, typing a
  ;; closing paren when your cursor is right before a closing paren
  ;; will insert another paren rather than moving through the existing
  ;; one.) Unfortunately, `ruby-electric-delimiters-alist' is defined
  ;; as a constant, so we can't customize it by setting it to nil
  ;; (actually, we can, but byte-compilation inserts the value
  ;; literally at its use sites, so this does not take effect).
  ;; Instead, we override the definition of `ruby-electric-mode-map'
  ;; to make it ignore `ruby-electric-delimiters-alist'. Also note
  ;; that we are actually doing this before `ruby-electric' is loaded.
  ;; This is so that the modification will actually affect the
  ;; definition of `ruby-electric-mode', which gets whatever value
  ;; `ruby-electric-mode-map' happens to have at definition time. (The
  ;; alternative is to also patch `ruby-electric-mode'.)

  (defvar ruby-electric-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map " " 'ruby-electric-space/return)
      (define-key map [remap delete-backward-char] 'ruby-electric-delete-backward-char)
      (define-key map [remap newline] 'ruby-electric-space/return)
      (define-key map [remap newline-and-indent] 'ruby-electric-space/return)
      (define-key map [remap electric-newline-and-maybe-indent] 'ruby-electric-space/return)
      (define-key map [remap reindent-then-newline-and-indent] 'ruby-electric-space/return)
      (el-patch-remove
        (dolist (x ruby-electric-delimiters-alist)
          (let* ((delim   (car x))
                 (plist   (cdr x))
                 (name    (plist-get plist :name))
                 (func    (plist-get plist :handler))
                 (closing (plist-get plist :closing)))
            (define-key map (char-to-string delim) func)
            (if closing
                (define-key map (char-to-string closing) 'ruby-electric-closing-char)))))
      map)
    (el-patch-concat
      "Keymap used in ruby-electric-mode"
      (el-patch-add ".\n\nThe single-character bindings have been removed.")))

  :init

  (add-hook 'ruby-mode #'ruby-electric-mode)

  :blackout t)

;;;; Rust
;; https://www.rust-lang.org/

;; Package `rust-mode' provides a major mode for Rust.
(use-package rust-mode)

;; Package `racer' provides a language server for Rust, and a Company
;; backend which uses this server to display autocompletions. Racer
;; also provides source code navigation support.
(use-package racer
  :init

  (add-hook 'rust-mode #'racer-mode)

  :blackout t)

;;;; Scheme

;; http://www.schemers.org/

;; Package `geiser' provides REPL integration for several
;; implementations of Scheme.
(use-package geiser)

;;;; Shell
;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

(use-feature sh-script
  :config/el-patch

  ;; Inhibit the "Indentation setup for shell type *sh" message.
  (defun sh-set-shell (shell &optional no-query-flag insert-flag)
    (el-patch-concat
      "Set this buffer's shell to SHELL (a string).
When used interactively, insert the proper starting #!-line,
and make the visited file executable via `executable-set-magic',
perhaps querying depending on the value of `executable-query'.

When this function is called noninteractively, INSERT-FLAG (the third
argument) controls whether to insert a #!-line and think about making
the visited file executable, and NO-QUERY-FLAG (the second argument)
controls whether to query about making the visited file executable.

Calls the value of `sh-set-shell-hook' if set.

Shell script files can cause this function be called automatically
when the file is visited by having a `sh-shell' file-local variable
whose value is the shell name (don't quote it)."
      (el-patch-add
        "\n\nThis function does not print superfluous messages."))
    (interactive (list (completing-read
                        (format "Shell (default %s): "
                                sh-shell-file)
                        ;; This used to use interpreter-mode-alist, but that is
                        ;; no longer appropriate now that uses regexps.
                        ;; Maybe there could be a separate variable that lists
                        ;; the shells, used here and to construct i-mode-alist.
                        ;; But the following is probably good enough:
                        (append (mapcar (lambda (e) (symbol-name (car e)))
                                        sh-ancestor-alist)
                                '("csh" "rc" "sh"))
                        nil nil nil nil sh-shell-file)
                       (eq executable-query 'function)
                       t))
    (if (string-match "\\.exe\\'" shell)
        (setq shell (substring shell 0 (match-beginning 0))))
    (setq sh-shell (sh-canonicalize-shell shell))
    (if insert-flag
        (setq sh-shell-file
              (executable-set-magic shell (sh-feature sh-shell-arg)
                                    no-query-flag insert-flag)))
    (setq mode-line-process (format "[%s]" sh-shell))
    (setq-local sh-shell-variables nil)
    (setq-local sh-shell-variables-initialized nil)
    (setq-local imenu-generic-expression
                (sh-feature sh-imenu-generic-expression))
    (let ((tem (sh-feature sh-mode-syntax-table-input)))
      (when tem
        (setq-local sh-mode-syntax-table
                    (apply 'sh-mode-syntax-table tem))
        (set-syntax-table sh-mode-syntax-table)))
    (dolist (var (sh-feature sh-variables))
      (sh-remember-variable var))
    (if (setq-local sh-indent-supported-here
                    (sh-feature sh-indent-supported))
        (progn
          (el-patch-remove
            (message "Setting up indent for shell type %s" sh-shell))
          (let ((mksym (lambda (name)
                         (intern (format "sh-smie-%s-%s"
                                         sh-indent-supported-here name)))))
            (add-function :around (local 'smie--hanging-eolp-function)
                          (lambda (orig)
                            (if (looking-at "[ \t]*\\\\\n")
                                (goto-char (match-end 0))
                              (funcall orig))))
            (add-hook 'smie-indent-functions #'sh-smie--indent-continuation nil t)
            (smie-setup (symbol-value (funcall mksym "grammar"))
                        (funcall mksym "rules")
                        :forward-token  (funcall mksym "forward-token")
                        :backward-token (funcall mksym "backward-token")))
          (setq-local parse-sexp-lookup-properties t)
          (unless sh-use-smie
            (setq-local sh-kw-alist (sh-feature sh-kw))
            (let ((regexp (sh-feature sh-kws-for-done)))
              (if regexp
                  (setq-local sh-regexp-for-done
                              (sh-mkword-regexpr (regexp-opt regexp t)))))
            (el-patch-remove
              (message "setting up indent stuff"))
            ;; sh-mode has already made indent-line-function local
            ;; but do it in case this is called before that.
            (setq-local indent-line-function 'sh-indent-line))
          (if sh-make-vars-local
              (sh-make-vars-local))
          (el-patch-remove
            (message "Indentation setup for shell type %s" sh-shell)))
      (el-patch-remove
        (message "No indentation for this shell type."))
      (setq-local indent-line-function 'sh-basic-indent-line))
    (when font-lock-mode
      (setq font-lock-set-defaults nil)
      (font-lock-set-defaults)
      (font-lock-flush))
    (setq sh-shell-process nil)
    (run-hooks 'sh-set-shell-hook))

  ;; Inhibit the "Indentation variables are now local" message.
  (defun sh-make-vars-local ()
    (el-patch-concat
      "Make the indentation variables local to this buffer.
Normally they already are local.  This command is provided in case
variable `sh-make-vars-local' has been set to nil.

To revert all these variables to the global values, use
command `sh-reset-indent-vars-to-global-values'."
      (el-patch-add
        "\n\nThis function does not print superfluous messages."))
    (interactive)
    (mapc 'make-local-variable sh-var-list)
    (el-patch-remove
      (message "Indentation variables are now local."))))

;;;; Swift
;; https://developer.apple.com/swift/

;; Package `swift-mode' provides a major mode for Swift code.
(use-package swift-mode)

;;;; TeX
;; https://www.tug.org/begin.html

;; Package `auctex' provides major modes for TeX code, including
;; compiler and viewer integration.
(straight-use-package 'auctex)

;; Feature `tex' from package `auctex' provides the base major mode
;; for TeX.
(use-feature tex
  :config/el-patch

  ;; Remove annoying messages when opening *.tex files.
  (defun TeX-update-style (&optional force)
    (el-patch-concat
      "Run style specific hooks"
      (el-patch-add
        ", silently,")
      " for the current document.

Only do this if it has not been done before, or if optional argument
FORCE is not nil.")
    (unless (or (and (boundp 'TeX-auto-update)
                     (eq TeX-auto-update 'BibTeX)) ; Not a real TeX buffer
                (and (not force)
                     TeX-style-hook-applied-p))
      (setq TeX-style-hook-applied-p t)
      (el-patch-remove
        (message "Applying style hooks..."))
      (TeX-run-style-hooks (TeX-strip-extension nil nil t))
      ;; Run parent style hooks if it has a single parent that isn't itself.
      (if (or (not (memq TeX-master '(nil t)))
              (and (buffer-file-name)
                   (string-match TeX-one-master
                                 (file-name-nondirectory (buffer-file-name)))))
          (TeX-run-style-hooks (TeX-master-file)))
      (if (and TeX-parse-self
               (null (cdr-safe (assoc (TeX-strip-extension nil nil t)
                                      TeX-style-hook-list))))
          (TeX-auto-apply))
      (run-hooks 'TeX-update-style-hook)
      (el-patch-remove
        (message "Applying style hooks...done"))))

  :config

  ;; The following configuration is recommended in the manual at
  ;; https://www.gnu.org/software/auctex/manual/auctex/Quick-Start.html.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (radian-with-operating-system macOS
    (when (or (file-directory-p "/Applications/TeXShop.app")
              (file-directory-p "/Applications/TeX/TeXShop.app"))

      ;; Use TeXShop for previewing LaTeX, rather than Preview. This
      ;; means we have to define the command to run TeXShop as a "viewer
      ;; program", and then tell AUCTeX to use the TeXShop viewer when
      ;; opening PDFs.
      (add-to-list 'TeX-view-program-list
                   '("TeXShop" "/usr/bin/open -a TeXShop.app %s.pdf"))
      (setf (map-elt TeX-view-program-selection 'output-pdf) '("TeXShop"))))

  (radian-defadvice radian--advice-inhibit-tex-style-loading-message
      (TeX-load-style-file file)
    :around TeX-load-style-file
    "Inhibit the \"Loading **/auto/*.el (source)...\" messages."
    (cl-letf* (((symbol-function #'raw-load) (symbol-function #'load))
               ((symbol-function #'load)
                (lambda (file &optional
                              noerror _nomessage
                              nosuffix must-suffix)
                  (raw-load file noerror 'nomessage nosuffix must-suffix))))
      (funcall TeX-load-style-file file)))

  (radian-defadvice radian--advice-inhibit-tex-removing-duplicates-message
      (TeX-auto-list-information name)
    :around TeX-auto-list-information
    "Inhibit the \"Removing duplicates...\" messages."
    (let ((inhibit-message t))
      (funcall TeX-auto-list-information name)))

  (radian-defhook radian--flycheck-tex-setup ()
    TeX-mode-hook
    "Disable some Flycheck checkers in TeX buffers."
    (radian--flycheck-disable-checkers 'tex-chktex 'tex-lacheck)))

;; Feature `tex-buf' from package `auctex' provides support for
;; running TeX commands and displaying their output.
(use-feature tex-buf
  :config

  ;; Save buffers automatically when compiling, instead of prompting.
  (setq TeX-save-query nil)

  (radian-defadvice radian--advice-hide-tex-compilation-buffers (name)
    :filter-return TeX-process-buffer-name
    "Hide AUCTeX compilation buffers by prepending a space to their names.
This prevents them from getting in the way of buffer selection."
    (concat " " name)))

;; Feature `latex' from package `auctex' provides the major mode for
;; LaTeX.
(use-feature latex
  :config

  ;; Don't be afraid to break inline math between lines.
  (setq LaTeX-fill-break-at-separators nil)

  ;; When inserting a left brace, delete the current selection first,
  ;; as per `delete-selection-mode'.
  (put 'LaTeX-insert-left-brace 'delete-selection t)

  (put 'LaTeX-using-Biber 'safe-local-variable #'booleanp))

;; Feature `font-latex' from package `auctex' provides the syntax
;; highlighting for the LaTeX major mode.
(use-feature font-latex
  :init

  ;; Do the following customizations before `font-latex' is loaded,
  ;; since otherwise we would have to call
  ;; `font-latex-update-sectioning-faces'.

  ;; Prevent superscripts and subscripts from being displayed in a
  ;; different font size.
  (setq font-latex-fontify-script nil)

  ;; Prevent section headers from being displayed in different font
  ;; sizes.
  (setq font-latex-fontify-sectioning 1))

;; Package `company-auctex' provides a Company backend that uses
;; information from AUCTeX for autocompletion.
(use-package company-auctex
  :demand t
  :after (:all company tex)
  :config

  (company-auctex-init))

;;;; TypeScript
;; https://www.typescriptlang.org/

;; Package `typescript-mode' provides a major mode for TypeScript.
(use-package typescript-mode
  :config

  ;; The standard TypeScript indent width is two spaces, not four.
  (setq typescript-indent-level 2)

  (radian-defhook radian--flycheck-typescript-setup ()
    typescript-mode-hook
    "If inside node_modules, disable the `typescript-tslint' Flycheck checker.
If we don't disable it, then it will generally just generate
several thousand errors, disable itself, and print a warning."
    (when (locate-dominating-file "node_modules")
      (radian--flycheck-disable-checkers 'typescript-tslint)))

  ;; Fix capitalization. It's TypeScript, not typescript.
  :blackout "TypeScript")

;; Package `tide' provides integration with the tsserver TypeScript
;; language server in order to provide source navigation, a Company
;; backend, and code formatting.
(use-package tide
  :demand t
  :after typescript-mode
  :config

  (add-hook 'typescript-mode-hook #'tide-setup)

  (define-minor-mode radian-tide-format-mode
    "Minor mode to reformat buffer using tsserver on save."
    nil nil nil
    (if radian-tide-format-mode
        (add-hook 'before-save-hook #'tide-format-before-save nil 'local)
      (remove-hook 'before-save-hook #'tide-format-before-save)))

  (add-hook 'tide-mode-hook #'radian-tide-format-mode)

  ;; Maintain standard TypeScript indent width.
  (setq tide-format-options '(:indentSize 2 :tabSize 2))

  :blackout t)

;;;; VimScript
;; http://vimdoc.sourceforge.net/htmldoc/usr_41.html

;; Package `vimrc-mode' provides a major mode for VimScript.
;; Provides syntax highlighting for VimScript files.
(use-package vimrc-mode
  :config

  (radian-defhook radian--fix-vimrc-indentation ()
    vimrc-mode-hook
    "Indent by two spaces in `vimrc-mode' rather than eight."
    ;; Based on https://stackoverflow.com/a/1819405/3538165.
    (setq-local tab-width 2)
    (setq-local indent-line-function 'insert-tab)))

;;; Configuration file formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.
(use-package apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.
(use-package crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.
(use-package dockerfile-mode)

;; Package `gitconfig-mode' provides a major mode for .gitconfig and
;; .gitmodules files.
(use-package gitconfig-mode)

;; Package `gitignore-mode' provides a major mode for .gitignore
;; files.
(use-package gitignore-mode)

;; Package `json-mode' provides a major mode for JSON.
(use-package json-mode
  :init/el-patch

  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.

This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'."))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(use-package pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(use-package pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(use-package ssh-config-mode)

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.
(use-package terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.
(use-package toml-mode
  ;; Correct the capitalization from "Toml" to "TOML".
  :blackout "TOML")

;; Package `yaml-mode' provides a major mode for YAML.
(use-package yaml-mode
  :config

  (add-hook 'yaml-mode-hook #'radian--auto-fill-disable))

;;; Introspection
;;;; Help

;; Feature `help' powers the *Help* buffer and related functionality.
(use-feature help
  :bind (:map help-map
              ("M-k" . radian-describe-keymap))
  :config

  (radian-defadvice radian--advice-help-inhibit-hints (&rest _)
    :override help-window-display-message
    "Inhibit the \"Type q in help window to delete it\" hints.
Normally these are printed in the echo area whenever you open a
help buffer.")

  (radian-defadvice radian--advice-help-disable-revert-prompt
      (help-mode-revert-buffer ignore-auto _noconfirm)
    :around help-mode-revert-buffer
    "Don't ask for confirmation before reverting help buffers.
\(Reverting is done by pressing \\<help-mode-map>\\[revert-buffer].)"
    (funcall help-mode-revert-buffer ignore-auto 'noconfirm))

  (defun radian-describe-keymap (keymap)
    "Display the bindings defined by KEYMAP, a symbol or keymap.
Interactively, select a keymap from the list of all defined
keymaps."
    (interactive
     (list
      (intern
       (completing-read
        "Keymap: " obarray
        (lambda (m)
          (and (boundp m)
               (keymapp (symbol-value m))))
        'require-match))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "Keymap `%S' defines the following bindings:" keymap)
                "\n\n"
                (substitute-command-keys (format "\\{%S}" keymap))))))

  (radian-defhook radian--xref-help-setup ()
    help-mode-hook
    "Make xref look up Elisp symbols in help buffers.
Otherwise, it will try to find a TAGS file using etags, which is
unhelpful."
    (add-hook 'xref-backend-functions #'elisp--xref-backend nil 'local)))

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(use-package helpful
  :init

  (use-feature counsel
    :config

    ;; Have the alternate "help" action for `counsel-M-x' use Helpful
    ;; instead of the default Emacs help.
    (setf (nth 0 (alist-get "h" (plist-get ivy--actions-list 'counsel-M-x)
                            nil nil #'equal))
          (lambda (x) (helpful-function (intern x)))))

  :bind (;; Remap standard commands.
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F" . helpful-function)
         ("M-f" . helpful-macro)
         ("C" . helpful-command)

         :map global-map
         ("C-c C-d" . helpful-at-point)))

;;;; Custom

;; Feature `cus-edit' powers Customize buffers and related
;; functionality.
(use-feature cus-edit
  :config

  ;; Don't show the search box in Custom.
  (setq custom-search-field nil))

;;;; Emacs Lisp development

;; Feature `elisp-mode' provides the major mode for Emacs Lisp. Very
;; important! It also provides the major mode for the *scratch*
;; buffer, which is very similar but slightly different. Not as
;; important.
(use-feature elisp-mode
  :config

  (radian-defhook radian--flycheck-elisp-setup ()
    emacs-lisp-mode-hook
    "Disable some Flycheck checkers for Emacs Lisp."
    ;; These checkers suck at reporting error locations, so they're
    ;; actually quite distracting to work with.
    (radian--flycheck-disable-checkers 'emacs-lisp 'emacs-lisp-checkdoc))

  ;; Note that this function is actually defined in `elisp-mode'
  ;; because screw modularity.
  (radian-defadvice radian--advice-company-elisp-use-helpful
      (func &rest args)
    :around elisp--company-doc-buffer
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  ;; The default mode lighter has a space instead of a hyphen.
  ;; Disgusting!
  :blackout (lisp-interaction-mode . "Lisp-Interaction"))

(defun radian-reload-init ()
  (interactive)
  (message "Reloading init-file...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init-file...done"))

(radian-bind-key "r" #'radian-reload-init)

(defun radian-eval-buffer-or-region (&optional start end)
  "Evaluate the current region, or the whole buffer if no region is active.
In Lisp code, START and END denote the region to be evaluated;
they default to `point-min' and `point-max' respectively.

If evaluating a buffer visiting this file, then delegate instead
to `radian-reload-init'."
  (interactive)
  (if (and (string= buffer-file-name radian-lib-file)
           (not (region-active-p)))
      (radian-reload-init)
    (let ((name nil))
      (if (region-active-p)
          (progn
            (setq start (region-beginning))
            (setq end (region-end))
            (setq name "region"))
        (setq start (point-min))
        (setq end (point-max))
        (setq name (buffer-name)))
      (let ((load-file-name (buffer-file-name)))
        (message "Evaluating %s..." name)
        (straight-transaction
          (eval-region start end))
        (message "Evaluating %s...done" name)))))

;; This keybinding is used for evaluating a buffer of Clojure code in
;; CIDER, and for evaluating a buffer of Scheme code in Geiser.
(bind-key "C-c C-k" #'radian-eval-buffer-or-region)

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend))
        ;; Make this command behave the same as `find-function' and
        ;; `find-variable', i.e. always prompt for an identifier,
        ;; defaulting to the one at point.
        (xref-prompt-for-identifier t))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

;; By default, C-h f, C-h v, and C-h o are bound to
;; `describe-function', `describe-variable', and `describe-symbol'
;; respectively. By analogy, C-h C-f, C-h C-v, and C-h C-o should be
;; bound as follows. (There's no `find-symbol' function by default for
;; some reason; note that `xref-find-definitions' is not a replacement
;; because it is major-mode dependent.) By further analogy, we should
;; bind `find-library'.
(bind-keys*
 ("C-h C-f" . find-function)
 ("C-h C-v" . find-variable)
 ("C-h C-o" . radian-find-symbol)
 ("C-h C-l" . find-library))

;;;;; Emacs Lisp linting

;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
(use-feature checkdoc
  :init

  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code.
(use-feature elisp-lint
  :init

  ;; From the package. We need this because some packages set this as
  ;; a file-local variable, but we don't install the package so Emacs
  ;; doesn't know the variable is safe.
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

;; Package `package-lint' provides a command that lets you check for
;; common package.el packaging problems in your packages.
(use-package package-lint)

;;; Applications
;;;; Organization

;; Use `use-feature' here because we already installed Org earlier.

;; Package `org' provides too many features to describe in any
;; reasonable amount of space. It is built fundamentally on
;; `outline-mode', and adds TODO states, deadlines, properties,
;; priorities, etc. to headings. Then it provides tools for
;; interacting with this data, including an agenda view, a time
;; clocker, etc. There are *many* extensions.
(use-feature org
  :bind (:map org-mode-map

              ;; Prevent Org from overriding the bindings for
              ;; windmove. By default, these keys are mapped to
              ;; `org-shiftleft', etc.
              ("S-<left>" . nil)
              ("S-<right>" . nil)
              ("S-<up>" . nil)
              ("S-<down>" . nil)

              ;; Add replacements for the keybindings we just removed.
              ;; C-<left> and C-<right> are unused by Org. C-<up> and
              ;; C-<down> are bound to `org-backward-paragraph', etc.
              ;; (but see below).
              ("C-<left>" . org-shiftleft)
              ("C-<right>" . org-shiftright)
              ("C-<up>" . org-shiftup)
              ("C-<down>" . org-shiftdown)

              ;; By default, Org maps C-<up> to
              ;; `org-backward-paragraph' instead of
              ;; `backward-paragraph' (and analogously for C-<down>).
              ;; However, it doesn't do the same remapping for the
              ;; other bindings of `backward-paragraph' (e.g. M-{).
              ;; Here we establish that remapping. (This is important
              ;; since we remap C-<up> and C-<down> to other things,
              ;; above. So otherwise there would be no easy way to
              ;; invoke `org-backward-paragraph' and
              ;; `org-forward-paragraph'.)
              ([remap backward-paragraph] . org-backward-paragraph)
              ([remap forward-paragraph] . org-forward-paragraph)

              ;; See discussion of this function below.
              ("C-M-RET" . radian-org-insert-heading-at-point)
              ("C-M-<return>" . radian-org-insert-heading-at-point))
  :bind* (;; Add the global keybindings for accessing Org Agenda and
          ;; Org Capture that are recommended in the Org manual.
          ("C-c a" . org-agenda)
          ("C-c c" . org-capture))
  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)

  ;; But add a new function for recovering the old behavior (see
  ;; `:bind' above).
  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))

  ;; Show headlines but not content by default.
  (setq org-startup-folded 'content)

  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian--list-of-strings-p)

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil))

;; Feature `org-indent' provides an alternative view for Org files in
;; which sub-headings are indented.
(use-feature org-indent
  :init

  (add-hook 'org-mode-hook #'org-indent-mode))

;; Feature `org-agenda' from package `org' provides the agenda view
;; functionality, which allows for collating TODO items from your Org
;; files into a single buffer.
(use-feature org-agenda
  :bind (:map org-agenda-mode-map

              ;; Prevent Org Agenda from overriding the bindings for
              ;; windmove.
              ("S-<up>" . nil)
              ("S-<down>" . nil)
              ("S-<left>" . nil)
              ("S-<right>" . nil)

              ;; Same routine as above. Now for Org Agenda, we could use
              ;; C-up and C-down because M-{ and M-} are bound to the same
              ;; commands. But I think it's best to take the same approach
              ;; as before, for consistency.
              ("C-<left>" . org-agenda-do-date-earlier)
              ("C-<right>" . org-agenda-do-date-later))
  :config

  (radian-defadvice radian--advice-org-agenda-default-directory
      (org-agenda &rest args)
    :around org-agenda
    "If `org-directory' exists, set `default-directory' to it in the agenda.
This makes the behavior of `find-file' more reasonable."
    (let ((default-directory (if (file-exists-p org-directory)
                                 org-directory
                               default-directory)))
      (apply org-agenda args)))

  (radian-defadvice radian--advice-blackout-org-agenda
      (&rest _)
    :override org-agenda-set-mode-name
    "Override the `org-agenda' mode lighter to just \"Org-Agenda\"."
    "Org-Agenda")

  (radian-defhook radian--org-agenda-setup ()
    org-agenda-mode-hook
    "Disable `visual-line-mode' locally."
    ;; See https://superuser.com/a/531670/326239.
    (visual-line-mode -1)
    (let ((inhibit-message t)
          (message-log-max nil))
      ;; I'm not exactly sure why this is necessary. More research is
      ;; needed.
      (toggle-truncate-lines +1)))

  ;; Hide blocked tasks in the agenda view.
  (setq org-agenda-dim-blocked-tasks 'invisible))

;; Feature `org-clock' from package `org' provides the task clocking
;; functionality.
(use-feature org-clock
  ;; We have to autoload these functions in order for the below code
  ;; that enables clock persistence without slowing down startup to
  ;; work.
  :commands (org-clock-load org-clock-save)
  :init

  ;; Allow clock data to be saved persistently.
  (setq org-clock-persist t)

  ;; Actually enable clock persistence. This is taken from
  ;; `org-clock-persistence-insinuate', but we can't use that function
  ;; since it causes both `org' and `org-clock' to be loaded for no
  ;; good reason.
  (add-hook 'org-mode-hook 'org-clock-load)
  (radian-defhook radian--org-clock-save ()
    kill-emacs-hook
    "Run `org-clock-save', but only if Org has been loaded.
Using this on `kill-emacs-hook' instead of `org-clock-save'
prevents a delay on killing Emacs when Org was not yet loaded."
    (when (featurep 'org)
      (org-clock-save)))

  :bind* (;; Make some `org-mode-map' bindings global instead.
          ("C-c C-x C-i" . org-clock-in)
          ("C-c C-x C-o" . org-clock-out)
          ("C-c C-x C-x" . org-clock-in-last)
          ("C-c C-x C-j" . org-clock-goto)
          ("C-c C-x C-q" . org-clock-cancel))

  :config/el-patch

  ;; Silence the messages that are usually printed when the clock data
  ;; is loaded from disk.
  (defun org-clock-load ()
    (el-patch-concat
      "Load clock-related data from disk, maybe resuming a stored clock."
      (el-patch-add "\n\nDo so without emitting any superfluous messages."))
    (when (and org-clock-persist (not org-clock-loaded))
      (if (not (file-readable-p org-clock-persist-file))
	  (el-patch-swap
            (message "Not restoring clock data; %S not found" org-clock-persist-file)
            nil)
        (el-patch-remove
          (message "Restoring clock data"))
        ;; Load history.
        (el-patch-wrap 1
          (radian--with-silent-load
            (load-file org-clock-persist-file)))
        (setq org-clock-loaded t)
        (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
		       org-clock-stored-history)
	  (org-clock-history-push position (find-file-noselect file)))
        ;; Resume clock.
        (pcase org-clock-stored-resume-clock
	  (`(,(and file (pred file-exists-p)) . ,position)
	   (with-current-buffer (find-file-noselect file)
	     (when (or (not org-clock-persist-query-resume)
		       (y-or-n-p (format "Resume clock (%s) "
				         (save-excursion
					   (goto-char position)
					   (org-get-heading t t)))))
	       (goto-char position)
	       (let ((org-clock-in-resume 'auto-restart)
		     (org-clock-auto-clock-resolution nil))
	         (org-clock-in)
	         (when (org-invisible-p) (org-show-context))))))
	  (_ nil)))))

  :config

  (defun radian--advice-org-clock-load-automatically (&rest _)
    "Run `org-clock-load'.
This is a `:before' advice for various Org functions which might
be invoked before `org-mode-hook' is run."
    (org-clock-load))

  (dolist (fun '(org-clock-in
                 org-clock-out
                 org-clock-in-last
                 org-clock-goto
                 org-clock-cancel))
    (advice-add fun :before #'radian--advice-org-clock-load-automatically)))

;;;; Filesystem management

;; When deleting a file interactively, move it to the trash instead.
(setq delete-by-moving-to-trash t)

;; Package `osx-trash' provides functionality that allows Emacs to
;; place files in the trash on macOS.
(use-package osx-trash
  :commands (osx-trash-move-file-to-trash)
  :init/el-patch

  (defun osx-trash-setup ()
    "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`osx-trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
    (when (and (eq system-type 'darwin)
               (not (fboundp 'system-move-file-to-trash)))
      (defalias 'system-move-file-to-trash
        'osx-trash-move-file-to-trash)))

  (osx-trash-setup))

;;;;; Dired

;; For some reason, the autoloads from `dired-aux' and `dired-x' are
;; not loaded automatically. Do it.
(require 'dired-loaddefs)

;; Dired has some trouble parsing out filenames that have e.g. leading
;; spaces, unless the ls program used has support for Dired. GNU ls
;; has this support, so if it is available we tell Dired (and the
;; `list-directory' command, not that it sees much use) to use it.
;;
;; This is in an advice so that we can defer the PATH search until
;; necessary.
(radian-defadvice radian--use-gls-for-list-directory (&rest _)
  :before list-directory
  "Make Dired use GNU ls, if it is available."
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  ;; Only do the check once, for efficiency.
  (advice-remove #'list-directory #'radian--use-gls-for-list-directory))

;; Feature `dired' provides a simplistic filesystem manager in Emacs.
(use-feature dired
  :bind (:map dired-mode-map
              ;; This binding is way nicer than ^. It's inspired by
              ;; Sunrise Commander.
              ("J" . dired-up-directory))
  :config

  (radian-defadvice radian--advice-dired-check-for-ls-dired (&rest _)
    :before dired-insert-directory
    "Check if ls --dired is supported ahead of time, and silently.

This advice prevents Dired from printing a message if your ls
does not support the --dired option. (We do this by performing
the check ourselves, and refraining from printing a message in
the problematic case.)"
    (when (eq dired-use-ls-dired 'unspecified)
      (setq dired-use-ls-dired
            (eq 0 (call-process insert-directory-program
                                nil nil nil "--dired")))))

  (add-hook 'dired-mode-hook #'radian--autorevert-silence)

  ;; Disable the prompt about whether I want to kill the Dired buffer
  ;; for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all. Note that this variable, for reasons unknown
  ;; to me, is defined in `dired-x', but only affects the behavior of
  ;; functions defined in `dired'.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message. (A message is shown if insta-revert is either disabled
  ;; or determined dynamically by setting this variable to a
  ;; function.)
  (setq dired-auto-revert-buffer t))

(use-feature dired-x
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil)

  (radian-with-operating-system macOS
    (radian-defadvice radian--advice-dired-guess-open-on-macos
        (orig-fun &rest args)
      :override dired-guess-default
      "Cause Dired's '!' command to use open(1).
This advice is only activated on macOS, where it is helpful since
most of the Linux utilities in `dired-guess-shell-alist-default'
are probably not going to be installed."
      "open")))

;;;; Terminal emulator

;; Feature `term' provides a workable, though slow, terminal emulator
;; within Emacs.
(use-feature term
  :bind (;; Allow usage of more commands from within the terminal.
         :map term-raw-map
         ("M-x" . execute-extended-command)
         ("C-h" . help-command)))

;;;; Version control

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-feature vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-feature smerge-mode
  :blackout t)

;; Package `with-editor' provides infrastructure for using Emacs as an
;; external editor for programs like Git. It is used by Magit.
(use-package with-editor
  :config/el-patch

  ;; Make sure that `with-editor' always starts a server with a
  ;; nonstandard name, instead of using the default one, so that
  ;; emacsclient from a tty never picks it up (which messes up the
  ;; color theme).
  (defun with-editor--setup ()
    (if (or (not with-editor-emacsclient-executable)
            (file-remote-p default-directory))
        (push (concat with-editor--envvar "=" with-editor-sleeping-editor)
              process-environment)
      ;; Make sure server-use-tcp's value is valid.
      (unless (featurep 'make-network-process '(:family local))
        (setq server-use-tcp t))
      ;; Make sure the server is running.
      (unless (process-live-p server-process)
        (el-patch-splice 2
          (when (server-running-p server-name)
            (setq server-name (format "server%s" (emacs-pid)))
            (when (server-running-p server-name)
              (server-force-delete server-name))))
        (server-start))
      ;; Tell $EDITOR to use the Emacsclient.
      (push (concat with-editor--envvar "="
                    (shell-quote-argument with-editor-emacsclient-executable)
                    ;; Tell the process where the server file is.
                    (and (not server-use-tcp)
                         (concat " --socket-name="
                                 (shell-quote-argument
                                  (expand-file-name server-name
                                                    server-socket-dir)))))
            process-environment)
      (when server-use-tcp
        (push (concat "EMACS_SERVER_FILE="
                      (expand-file-name server-name server-auth-dir))
              process-environment))
      ;; As last resort fallback to the sleeping editor.
      (push (concat "ALTERNATE_EDITOR=" with-editor-sleeping-editor)
            process-environment))))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       magit-git-executable
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook 'magit-credential-hook
                                #'magit-maybe-start-credential-cache-daemon)))))))


  :config

  ;; Enable C-c M-g as a shortcut to go to a popup of Magit commands
  ;; relevant to the current file.
  (global-magit-file-mode +1)

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix 'magit-merge "-s"
    '("-u" "Allow unrelated" "--allow-unrelated-histories")))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(use-feature git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `gh' provides an Elisp interface to the GitHub API.
(use-package gh
  ;; Disable autoloads because this package autoloads *way* too much
  ;; code. See https://github.com/sigma/gh.el/issues/95.
  :straight (:host github :repo "sigma/gh.el"
                   :no-autoloads t))

;; Package `magit-popup' is a dependency of `magit-gh-pulls' that is
;; not declared properly, see
;; <https://github.com/sigma/magit-gh-pulls/issues/126>. The reason
;; for this issue is that `magit-gh-pulls' was previously a dependency
;; of `magit', so that hid the error.
(use-package magit-popup
  :after magit-gh-pulls)

;; Package `magit-gh-pulls' adds a section to Magit which displays
;; open pull requests on a corresponding GitHub repository, if any,
;; and allows you to check them out locally.
(use-package magit-gh-pulls
  :demand t
  :after magit
  :config

  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

  (radian-defadvice radian--advice-hide-magit-gh-pulls-when-not-cached ()
    :before-while magit-gh-pulls-insert-gh-pulls
    "Hide the \"Pull Requests\" section when the list is not cached."
    (when-let ((repo (magit-gh-pulls-guess-repo)))
      (magit-gh-pulls-requests-cached-p
       (magit-gh-pulls-get-api) (car repo) (cdr repo))))

  :blackout t)

;;;; External commands

;; Feature `compile' provides a way to run a shell command from Emacs
;; and view the output in real time, with errors and warnings
;; highlighted and hyperlinked.
(use-feature compile
  :init

  (radian-bind-key "m" #'compile)

  :config

  ;; Automatically scroll the Compilation buffer as output appears,
  ;; but stop at the first error.
  (setq compilation-scroll-output 'first-error)

  ;; Don't ask about saving buffers when invoking `compile'. Try to
  ;; save them all immediately using `save-some-buffers'.
  (setq compilation-ask-about-save nil)

  ;; Actually, don't bother saving buffers at all. That's dumb. We
  ;; know to save our buffers if we want them to be updated on disk.
  (setq compilation-save-buffers-predicate
        (lambda ()))

  (radian-defadvice radian--advice-compile-pop-to-buffer (buf)
    :filter-return compilation-start
    "Pop to compilation buffer on \\[compile]."
    (prog1 buf
      (select-window (get-buffer-window buf)))))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(use-package rg)

;;;; Internet applications

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(use-feature browse-url
  :init

  (defun radian--browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p 'markdown-mode 'org-mode 'org-agenda-mode)))

  :bind* (:filter (radian--browse-url-predicate)
                  ("C-c C-o" . browse-url-at-point)))

;; Feature `bug-reference' provides a mechanism for hyperlinking issue
;; tracker references (like #20), so that you can open them in a web
;; browser easily.
(use-feature bug-reference
  :config

  (bind-key "C-c C-o" #'bug-reference-push-button bug-reference-map))

;; Package `git-link' provides a simple function M-x git-link which
;; copies to the kill ring a link to the current line of code or
;; selection on GitHub, GitLab, etc.
(use-package git-link
  :config

  ;; Link to a particular revision of a file rather than using the
  ;; branch name in the URL.
  (setq git-link-use-commit t))

;; Package `atomic-chrome' provides a way for you to edit textareas in
;; Chrome or Firefox using Emacs. See
;; https://chrome.google.com/webstore/detail/atomic-chrome/lhaoghhllmiaaagaffababmkdllgfcmc
;; for the Chrome extension.
(use-package atomic-chrome
  ;; Use my fork until
  ;; https://github.com/alpha22jp/atomic-chrome/issues/42 is fixed.
  :straight (:host github :repo "alpha22jp/atomic-chrome"
                   :fork (:repo "raxod502/atomic-chrome" :branch "fork/1"))
  :defer 5
  :config

  (defvar-local radian-atomic-chrome-url nil
    "The URL of the text area being edited.")

  (defcustom radian-atomic-chrome-setup-hook nil
    "Hook run while setting up an `atomic-chrome' buffer."
    :type 'hook)

  (radian-defadvice radian--advice-atomic-chrome-setup (url)
    :after atomic-chrome-set-major-mode
    "Save the URL in `radian-atomic-chrome-url'.
Also run `radian-atomic-chrome-setup-hook'."
    (setq radian-atomic-chrome-url url)
    (run-hooks 'radian-atomic-chrome-setup-hook))

  ;; Edit in Markdown by default, because many sites support it and
  ;; it's not a big deal if the text area doesn't actually support
  ;; Markdown.
  (setq atomic-chrome-default-major-mode 'markdown-mode)

  (radian-defhook radian--atomic-chrome-switch-back ()
    atomic-chrome-edit-done-hook
    "Switch back to the browser after finishing with `atomic-chrome'."
    (when-let* ((conn (websocket-server-conn
                       (atomic-chrome-get-websocket (current-buffer))))
                (browser
                 (cond
                  ((eq conn atomic-chrome-server-ghost-text)
                    "Firefox")
                   ((eq conn atomic-chrome-server-atomic-chrome)
                    "Google Chrome"))))
      (cond
       ((radian-operating-system-p macOS)
        (call-process "open" nil nil nil "-a" browser))
       ((radian-operating-system-p linux)
        (call-process "wmctrl" nil nil nil "-a" browser)))))

  ;; Listen for requests from the Chrome/Firefox extension.
  (atomic-chrome-start-server))

;; Package `webpaste' provides Emacs support for many different
;; command-line pastebins.
(use-package webpaste)

;; Package `sx' allows you to browse Stack Overflow from within Emacs.
;; First, run `sx-authenticate' in order to provide your username and
;; password. After that, you can use any of the autoloaded entry
;; points. Navigation is keyboard-centric.
(use-package sx)

;;;; Emacs profiling

;; Package `esup' allows you to run a child Emacs process with special
;; profiling functionality, and to collect timing results for each
;; form in your init-file.
(use-package esup
  :config

  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0)

  (radian-defadvice radian--advice-esup-unwrap-init-file
      (esup &optional init-file)
    :around esup
    "Help `esup' to work with the Radian init-file."
    (if init-file
        (funcall esup init-file)
      (let ((fname (expand-file-name "esup-init.el" temporary-file-directory)))
        (with-temp-file fname
          (print
           `(progn
              ;; We need this for `string-trim', but it's not
              ;; `require'd until the beginning of radian.el.
              (require 'subr-x)

              ;; Prevent indentation from being lost in the profiling
              ;; results.
              (advice-add #'esup-child-chomp :override #'string-trim)

              ;; esup does not set `user-init-file'.
              (setq user-init-file ,radian-lib-file)

              ;; If there's an error, let me see where it is.
              (setq debug-on-error t)

              ;; Make it possible to detect whether the init-file is
              ;; being profiled.
              (defvar radian--currently-profiling-p t)

              ;; Abbreviated (and flattened) version of init.el.
              (defvar radian-minimum-emacs-version "26.1")
              (defvar radian-local-init-file
                (expand-file-name "init.local.el" user-emacs-directory))
              (setq package-enable-at-startup nil)
              (setq custom-file (expand-file-name
                                 (format "custom-%d-%d.el" (emacs-pid) (random))
                                 temporary-file-directory))
              (defvar radian-lib-file ,radian-lib-file)
              (defvar radian--finalize-init-hook nil))
           (current-buffer))
          (insert-file-contents-literally radian-lib-file)
          (goto-char (point-max))
          (print
           '(run-hooks 'radian--finalize-init-hook)
           (current-buffer)))
        (funcall esup fname)))))

;;; Startup

(radian-defadvice radian--advice-inhibit-startup-message (&rest _)
  :override display-startup-echo-area-message
  "Unconditionally inhibit the startup message in the echo area.
This is the message that reads \"For more information about GNU
Emacs...\". Emacs suggests setting
`inhibit-startup-echo-area-message' to your username so that
other people using your configuration will still get to see this
spam. This advice, however, inhibits the message for everyone.")

;; Disable the *About GNU Emacs* buffer at startup, and go straight
;; for the scratch buffer.
(setq inhibit-startup-screen t)

;; Remove the initial *scratch* message. Start with a blank screen, we
;; know what we're doing.
(setq initial-scratch-message nil)

;;; Shutdown

(defun radian-really-kill-emacs ()
  "Kill Emacs immediately, bypassing `kill-emacs-hook'."
  (interactive)
  (let ((kill-emacs-hook nil))
    (kill-emacs)))

;; Package `restart-emacs' provides an easy way to restart Emacs from
;; inside of Emacs, both in the terminal and in windowed mode.
(use-package restart-emacs
  :commands (radian-new-emacs)
  :init

  (defvar radian--restart-in-progress nil
    "Used to prevent infinite recursion.
This is non-nil if `radian--advice-kill-emacs-dispatch' has called
`restart-emacs'.")

  (radian-defadvice radian--advice-kill-emacs-dispatch
      (save-buffers-kill-emacs &optional arg)
    :around save-buffers-kill-emacs
    "Allow restarting Emacs or starting a new session on shutdown."
    (if radian--restart-in-progress
        (funcall save-buffers-kill-emacs arg)
      (let ((prompt "Really exit (or restart, or start new) Emacs? (y/n/r/e) ")
            (key nil))
        (while (null key)
          (let ((cursor-in-echo-area t))
            (when minibuffer-auto-raise
              (raise-frame (window-frame (minibuffer-window))))
            (pcase (setq key
                         (read-key (propertize prompt
                                               'face 'minibuffer-prompt)))
              ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
              ((or ?n ?N))
              ((or ?r ?R) (let ((radian--restart-in-progress t))
                            (restart-emacs arg)))
              ((or ?e ?E) (radian-new-emacs arg))
              (?\C-g (signal 'quit nil))
              (_ (setq key nil)))))
        (message "%s%c" prompt key))))

  :config/el-patch

  (defun (el-patch-swap restart-emacs radian-new-emacs)
      (&optional args)
    (el-patch-concat
      (el-patch-swap
        "Restart Emacs."
        "Start a new Emacs session without killing the current one.")
      "

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      "
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      " with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be "
      (el-patch-swap "restarted" "started")
      ".")
    (interactive "P")
    ;; Do not trigger a restart unless we are sure, we can restart emacs
    (restart-emacs--ensure-can-restart)
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (let* ((default-directory (restart-emacs--guess-startup-directory))
           (translated-args (if (called-interactively-p 'any)
                                (restart-emacs--translate-prefix-to-args args)
                              args))
           (restart-args (append translated-args
                                 ;; When Emacs is started with a -Q
                                 ;; restart-emacs's autoloads would not be present
                                 ;; causing the the --restart-emacs-desktop
                                 ;; argument to be unhandled
                                 (unless (member "-Q" translated-args)
                                   (restart-emacs--frame-restore-args))))
           (el-patch-remove
             (kill-emacs-hook (append kill-emacs-hook
                                      (list (apply-partially #'restart-emacs--launch-other-emacs
                                                             restart-args))))))
      (el-patch-swap
        (save-buffers-kill-emacs)
        (restart-emacs--launch-other-emacs restart-args)))))

;;; Miscellaneous

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
(setq ad-redefinition-action 'accept)

;;; Appearance

(defcustom radian-font nil
  "Default font, as a string. Nil means use the default.
This is passed to `set-frame-font'."
  :type '(choice string (const :tag "Default" nil)))

(defcustom radian-font-size nil
  "Default font size, in pixels. Nil means use the default."
  :type '(choice integer (const :tag "Default" nil)))

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

;; Disable the contextual menu that pops up when you right-click.
(unbind-key "<C-down-mouse-1>")

(when (display-graphic-p)

  ;; Disable the scroll bars.
  (scroll-bar-mode -1)

  ;; Disable the tool bar.
  (tool-bar-mode -1)

  ;; Prevent the cursor from blinking.
  (blink-cursor-mode -1)

  ;; Set the default font size.
  (when radian-font-size
    (set-face-attribute 'default nil :height radian-font-size))

  ;; Set the default font.
  (when radian-font
    (set-frame-font radian-font 'keep-size t))

  ;; Use the same font for fixed-pitch text as the rest of Emacs (you
  ;; *are* using a monospace font, right?).
  (set-face-attribute 'fixed-pitch nil :family 'unspecified)

  ;; On macOS, set the title bar to match the frame background.
  (when (eq window-system 'ns)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

;; Feature `menu-bar' provides the annoying menu bar which we will
;; disable immediately.
(use-feature menu-bar
  :config

  (menu-bar-mode -1))

;;;; Mode line

;; The following code customizes the mode line to something like:
;; [*] radian.el   18% (18,0)     [radian:develop*]  (Emacs-Lisp)

(defun radian-mode-line-buffer-modified-status ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "[*]"
     "   ")
   'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defvar-local radian-mode-line-project-and-branch nil
  "Mode line construct showing Projectile project and Git status.
The format is [project:branch*], where the * is shown if the
working directory is dirty. Either component can be missing; this
might happen if Projectile is not available or if the project is
not version-controlled with Git. If nothing should be displayed,
this variable is set to nil.

This variable is actually only a cached value; it is set by
`radian-mode-line-compute-project-and-branch' for performance
reasons.

See also `radian-show-git-mode'.")

;; Don't clear the cache when switching major modes (or using M-x
;; normal-mode).
(put 'radian-mode-line-project-and-branch 'permanent-local t)

(defun radian--mode-line-recompute-project-and-branch ()
  "Recalculate and set `radian-mode-line-project-and-branch'.
Force a redisplay of the mode line if necessary. This is
buffer-local."
  (unless (file-remote-p default-directory)
    (condition-case-unless-debug err
        (let ((old radian-mode-line-project-and-branch)
              (new
               (let* (;; Don't insist on having Projectile loaded.
                      (project-name (when (featurep 'projectile)
                                      (projectile-project-name)))
                      ;; Projectile returns "-" to mean "no project".
                      ;; I'm still wondering what happens if someone
                      ;; makes a project named "-".
                      (project-name (unless (equal project-name "-")
                                      project-name))
                      ;; Check if we are actually in a Git repo, and Git
                      ;; is available, and we want to show the Git
                      ;; status.
                      (git (and
                            radian-show-git-mode
                            (executable-find "git")
                            (locate-dominating-file default-directory ".git")))
                      (branch-name
                       (when git
                         ;; Determine a reasonable string to show for
                         ;; the current branch. This is actually more or
                         ;; less the same logic as we use for the Radian
                         ;; Zsh prompt.
                         (with-temp-buffer
                           ;; First attempt uses symbolic-ref, which
                           ;; returns the branch name if it exists.
                           (call-process "git" nil '(t nil) nil
                                         "symbolic-ref" "HEAD")
                           (if (> (buffer-size) 0)
                               ;; It actually returns something like
                               ;; refs/heads/master, though, so let's
                               ;; try to trim it if possible.
                               (let ((regex "^\\(refs/heads/\\)?\\(.+\\)$")
                                     (str (string-trim (buffer-string))))
                                 (if (string-match regex str)
                                     (match-string 2 str)
                                   ;; If it's something weird then just
                                   ;; show it literally.
                                   str))
                             ;; If symbolic-ref didn't return anything
                             ;; on stdout (we discarded stderr), we
                             ;; probably have a detached head and we
                             ;; should show the abbreviated commit hash
                             ;; (e.g. b007692).
                             (erase-buffer)
                             (call-process "git" nil '(t nil) nil
                                           "rev-parse" "--short" "HEAD")
                             (if (> (buffer-size) 0)
                                 (string-trim (buffer-string))
                               ;; We shouldn't get here. Unfortunately,
                               ;; it turns out that we do every once in
                               ;; a while. (I have no idea why.)
                               "???")))))
                      (dirty (when git
                               (with-temp-buffer
                                 (call-process "git" nil t nil
                                               "status" "--porcelain")
                                 (if (> (buffer-size) 0)
                                     "*" "")))))
                 (cond
                  ((and project-name git)
                   (format "  [%s:%s%s]" project-name branch-name dirty))
                  (project-name
                   (format "  [%s]" project-name))
                  ;; This should never happen unless you do something
                  ;; perverse like create a version-controlled
                  ;; Projectile project whose name is a hyphen, but we
                  ;; want to handle it anyway.
                  (git
                   (format "  [%s%s]" branch-name dirty))))))
          (unless (equal old new)
            (setq radian-mode-line-project-and-branch new)
            (force-mode-line-update)))
      (error
       ;; We should not usually get an error here. In the case that we
       ;; do, however, let's try to avoid displaying garbage data, and
       ;; instead delete the construct entirely from the mode line.
       (unless (null radian-mode-line-project-and-branch)
         (setq radian-mode-line-project-and-branch nil)
         (force-mode-line-update))))))

;; We will make sure this information is updated after some time of
;; inactivity, for the current buffer.

(defcustom radian-mode-line-update-delay 1
  "Seconds of inactivity before updating the mode line.
Specifically, this entails updating the Projectile project, Git
branch, and dirty status, which are the most computationally
taxing elements."
  :type 'number)

;; We only need one global timer pair for all the buffers, since we
;; will only be updating the cached mode line value for the current
;; buffer.
;;
;; The way this is set up, the main idle timer runs each time that
;; Emacs is idle for exactly one second. That triggers a recomputation
;; of the mode line, and also schedules the repeat timer, which
;; reschedules itself repeatedly. Why do we need two timers? If we
;; tried to use just the idle timer, then the recomputation would only
;; get scheduled once per idle session, one second in, instead of
;; going once per second after one second of initial idleness. If we
;; tried to use just the repeat timer, then we would get
;; ever-increasing delays before it would fire, in each new idle
;; session. Why? Because the pattern for scheduling an idle timer
;; repeatedly is to increase the idle delay, since the idle time is
;; not re-set just because a timer fired. And if the idle session ends
;; between timer fires, then the repeat timer will be stuck with a
;; really long idle delay, and won't fire again.

(defun radian--mode-line-recompute-and-reschedule ()
  "Compute mode line data and re-set timers.
The delay is `radian-mode-line-update-delay'. The timers are
`radian--mode-line-idle-timer' and
`radian--mode-line-repeat-timer'."

  ;; Cancel any existing timer (we wouldn't want to introduce
  ;; duplicate timers!), and do it early in a half-hearted attempt to
  ;; avoid race conditions.
  (when radian--mode-line-repeat-timer
    (cancel-timer radian--mode-line-repeat-timer))

  ;; Do the computation.
  (radian--mode-line-recompute-project-and-branch)

  ;; If Emacs is already idle (meaning that the main idle timer has
  ;; already been triggered, and won't go again), then we need to
  ;; schedule the repeat timer. Otherwise, the main idle timer will be
  ;; triggered when Emacs does become idle, and we don't need to
  ;; schedule anything. There's no need to clear an old repeat timer,
  ;; since the idle timer will always get called before the repeat
  ;; timer and that will cause the repeat timer to be re-set as below.
  (when (current-idle-time)
    (setq radian--mode-line-repeat-timer
          (run-with-idle-timer
           (time-add (current-idle-time) radian-mode-line-update-delay)
           nil #'radian--mode-line-recompute-and-reschedule))))

(defvar radian--mode-line-idle-timer
  (run-with-idle-timer
   radian-mode-line-update-delay 'repeat
   #'radian--mode-line-recompute-and-reschedule)
  "Timer that recomputes information for the mode line, or nil.
This runs once each time Emacs is idle.
Future recomputations are scheduled under
`radian--mode-line-repeat-timer'. See also
`radian--mode-line-recompute-and-reschedule' and
`radian--mode-line-recompute-project-and-branch'.")

(defvar radian--mode-line-repeat-timer nil
  "Timer that recomputes information for the mode line, or nil.
This is scheduled repeatedly at intervals after
`radian--mode-line-idle-timer' runs once. See also
`radian--mode-line-recompute-and-reschedule' and
`radian--mode-line-recompute-project-and-branch'.")

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

(define-minor-mode radian-show-git-mode
  "Minor mode for showing Git status in mode line.

If enabled, then both the current Projectile project and the
current Git branch are shown in the mode line. Otherwise, only
the former is shown.")

(define-globalized-minor-mode radian-show-git-global-mode
  radian-show-git-mode radian-show-git-mode)

(radian-show-git-global-mode +1)

;; https://emacs.stackexchange.com/a/7542/12534
(defun radian--mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

(defcustom radian-mode-line-left
  '(;; Show [*] if the buffer is modified.
    (:eval (radian-mode-line-buffer-modified-status))
    " "
    ;; Show the name of the current buffer.
    mode-line-buffer-identification
    "   "
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the current Projectile project and Git branch.
    radian-mode-line-project-and-branch
    ;; Show the active major and minor modes.
    "  "
    mode-line-modes)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right nil
  "Composite mode line construct to be shown right-aligned."
  :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (radian--mode-line-align
                        (format-mode-line radian-mode-line-left)
                        (format-mode-line radian-mode-line-right))
                       'fixedcase 'literal)))

;;;; Color theme

(defcustom radian-color-theme-enable t
  "Non-nil means to load the default Radian color theme.
Set this to nil if you wish to load a different color theme in
your local configuration."
  :type 'boolean)

;; Package `zerodark-theme' provides a good-looking color theme that
;; works in both windowed and tty Emacs.
(straight-register-package
 '(zerodark-theme :host github :repo "NicolasPetton/zerodark-theme"))
(when radian-color-theme-enable
  (use-package zerodark-theme))

;;; Closing

(radian--run-hook 'radian-after-init-hook)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(unless (bound-and-true-p radian--currently-profiling-p)
  (when (= 0 (random 100))
    (straight-prune-build-directory)))

;; Enable color theme as late as is humanly possible. This reduces
;; frame flashing and other artifacts during startup.
(when radian-color-theme-enable
  (use-feature zerodark-theme
    :demand t
    :config

    (load-theme 'zerodark 'no-confirm)))

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:
