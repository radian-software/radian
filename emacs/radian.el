;; -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Detect stale bytecode

;; If Emacs version changed, the bytecode is no longer valid and we
;; must recompile. Also, if the location of Radian changed, our
;; dotfile-finding functions are defined incorrectly and we must
;; recompile.
(eval
 `(unless (equal
           (list
            (emacs-version)
            radian-lib-file)
           ',(eval-when-compile
               (list
                (emacs-version)
                radian-lib-file)))
    (throw 'stale-bytecode nil)))

;;; Load built-in utility libraries

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;; Define Radian customization groups

(defgroup radian-hooks nil
  "Startup hooks for Radian Emacs."
  :group 'radian
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/radian"))

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

(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
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
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defmacro radian-operating-system-p (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro radian-with-operating-system (os &rest body)
  "If OS corresponds to the current operating system, eval and return BODY.
If not, return nil.

Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (declare (indent 1))
  `(when (radian-operating-system-p ,os)
     ,@body))

(defmacro radian-if-compiletime (cond then else)
  "Like `if', but COND is evaluated at compile time.
The macro expands directly to either THEN or ELSE, and the other
branch is not compiled. This can be helpful to deal with code
that uses functions only defined in a specific Emacs version."
  (declare (indent 2))
  (if (eval cond)
      then
    else))

(defmacro radian-when-compiletime (cond &rest body)
  "Like `when', but COND is evaluated at compile time.
BODY is only compiled if COND evaluates to non-nil. This can be
helpful to deal with code that uses functions only defined in a
specific Emacs version."
  (declare (indent 1))
  (when (eval cond)
    `(progn ,@body)))

(defun radian-managed-p (filename)
  "Return non-nil if FILENAME is managed by Radian.
This means that FILENAME is a symlink whose target is inside
`radian-directory'."
  (let ((truename (file-truename filename)))
    (string-prefix-p radian-directory truename
                     (when (if (fboundp 'file-name-case-insensitive-p)
                               (file-name-case-insensitive-p truename)
                             (radian-with-operating-system macOS
                               t))
                       'ignore-case))))

(defmacro radian--with-silent-load (&rest body)
  "Execute BODY, with the function `load' made silent."
  (declare (indent 0))
  `(radian-flet ((defun load (file &optional noerror _nomessage &rest args)
                   (apply load file noerror 'nomessage args)))
     ,@body))

(defmacro radian--with-silent-write (&rest body)
  "Execute BODY, with the function `write-region' made silent."
  (declare (indent 0))
  `(radian-flet ((defun write-region
                     (start end filename &optional append visit lockname
                            mustbenew)
                   (funcall write-region start end filename append 0
                            lockname mustbenew)
                   (when (or (stringp visit) (eq visit t))
                     (setq buffer-file-name
                           (if (stringp visit)
                               visit
                             filename))
                     (set-visited-file-modtime)
                     (set-buffer-modified-p nil))))
     (cl-letf (((symbol-function #'message) #'ignore))
       ,@body)))

(defmacro radian--with-silent-message (regexps &rest body)
  "Silencing any messages that match REGEXPS, execute BODY.
REGEXPS is a list of strings; if `message' would display a
message string (not including the trailing newline) matching any
element of REGEXPS, nothing happens. The REGEXPS need not match
the entire message; include ^ and $ if necessary. REGEXPS may
also be a single string."
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
         (setq ,regexps-sym (list ,regexps-sym)))
       (radian-flet ((defun message (format &rest args)
                       (let ((str (apply #'format format args)))
                         ;; Can't use an unnamed block because during
                         ;; byte-compilation, some idiot loads `cl', which
                         ;; sticks an advice onto `dolist' that makes it
                         ;; behave like `cl-dolist' (i.e., wrap it in
                         ;; another unnamed block) and therefore breaks
                         ;; this code.
                         (cl-block done
                           (dolist (regexp ,regexps-sym)
                             (when (or (null regexp)
                                       (string-match-p regexp str))
                               (cl-return-from done)))
                           (funcall message "%s" str)))))
         ,@body))))

(defun radian--advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:override' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defun radian--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun radian--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

(defun radian--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

;;; Define hooks and load local configuration

;; Reset the value of this variable so that stale functions don't
;; stick around.
(setq radian--finalize-init-hook nil)

(defcustom radian-before-straight-hook nil
  "Hook run just before Radian bootstraps straight.el.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-after-init-hook nil
  "Hook run after at the very end of init.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defvar radian--hook-contents nil
  "Alist mapping local init hooks to lists of forms.
This is used to embed local init hook code directly into the
init-file at the appropriate places during byte-compilation,
without breaking macro-expansion.")

;; Idempotency.
(setq radian--hook-contents nil)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)

(defmacro radian--load-local-init-file ()
  "Load local init-file, with crazy hacks for byte-compilation.
In particular, if we are byte-compiling, actually macroexpand to
the entire contents of the local init-file, except that the
bodies of invocations to `radian-local-on-hook' are recorded in
`radian--hook-contents'. Otherwise just load the file like
usual."
  (if byte-compile-current-file
      (let ((forms nil))
        (with-temp-buffer
          (ignore-errors
            ;; Can't do this literally because it breaks Unicode
            ;; characters.
            (insert-file-contents radian-local-init-file))
          (condition-case _
              (while t
                (let ((form (read (current-buffer))))
                  (if (and (listp form)
                           (eq (nth 0 form) #'radian-local-on-hook)
                           (nth 1 form)
                           (symbolp (nth 1 form))
                           (nthcdr 2 form))
                      (let* ((name (nth 1 form))
                             (body (nthcdr 2 form))
                             (hook (intern (format "radian-%S-hook" name)))
                             (link (assq hook radian--hook-contents)))
                        (unless link
                          (setq link (cons hook nil))
                          (push link radian--hook-contents))
                        (dolist (subform body)
                          (push subform (cdr link))))
                    (push form forms))))
            (end-of-file)))
        (setq forms (nreverse forms))
        (dolist (link radian--hook-contents)
          (setf (cdr link)
                (nreverse (cdr link))))
        `(progn ,@forms))
    `(load radian-local-init-file 'noerror 'nomessage)))

(defmacro radian-local-on-hook (name &rest body)
  "Register some code to be run on one of Radian's hooks.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol, and the code which is added is BODY wrapped in a `progn'.
See \\[customize-group] RET radian-hooks RET for a list of hooks
which you can use with this macro in your local init-file.

Using this macro instead of defining functions and adding them to
Radian's hooks manually means that a lot of magic happens which
allows Radian to embed your entire local init-file into Radian
during byte-compilation without breaking macroexpansion in
unexpected ways."
  (declare (indent 1))
  (let ((func-name (intern (format "radian-local--%S" name)))
        (hook (intern (format "radian-%S-hook" name))))
    `(progn
       (radian-defhook ,func-name ()
         ,hook
         "Automatically-generated local hook function."
         (radian-protect-macros
           ,@body)))))

(defmacro radian--run-hook (name)
  "Run the given local init HOOK.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol. This binds `straight-current-profile', and also has some
gnarly hacks to allow Radian to embed the entire contents of the
hook directly into the init-file during byte-compilation."
  (declare (indent 0))
  (let ((hook (intern (format "radian-%S-hook" name))))
    `(let ((straight-current-profile 'radian-local))
       (run-hooks ',hook)
       ,@(when byte-compile-current-file
           (alist-get hook radian--hook-contents)))))

;; Allow to disable local customizations with a
;; command-line argument.
(if (member "--no-local" command-line-args)

    ;; Make sure to delete --no-local from the list, because
    ;; otherwise Emacs will issue a warning about the unknown
    ;; argument.
    (setq command-line-args
          (delete "--no-local" command-line-args))

  ;; Load local customizations.
  (radian--load-local-init-file))

;;; Startup optimizations

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

(setq gc-cons-threshold (* 50 1000 1000))

;; After we enabled `load-prefer-newer' in init.el, disable it again
;; for the duration of init. Presumably, it slows things down, and we
;; shouldn't need it for anything but loading radian.el itself.
(setq load-prefer-newer nil)

;;; Networking

;; Use `with-eval-after-load' instead of `use-feature' because we have
;; not yet set up package management.

;; Feature `gnutls' provides support for SSL/TLS connections, using
;; the GnuTLS library.
(with-eval-after-load 'gnutls

  ;; `use-package' does this for us normally.
  (eval-when-compile
    (require 'gnutls))

  ;; Do not allow insecure TLS connections.
  (setq gnutls-verify-error t)

  ;; Bump the required security level for TLS to an acceptably modern
  ;; value.
  (setq gnutls-min-prime-bits 3072))

;; Feature `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http

  (eval-when-compile
    (require 'url-http))

  (radian-defadvice radian--no-query-on-http-kill
      (buffer)
    :filter-return #'url-http
    "Disable query-on-exit for all network connections.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request."
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

;; Use the develop branch of straight.el on Radian's develop branch.
;; (On Radian's master branch, we use the master branch of
;; straight.el.)
(setq straight-repository-branch "develop")

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

;; Clear out recipe overrides (in case of re-init).
(setq straight-recipe-overrides nil)

(radian--run-hook before-straight)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(defun radian--remove-sharp-quotes (form)
  "Remove sharp quotes in all sub-forms of FORM."
  (pcase form
    (`(function ,x) (radian--remove-sharp-quotes x))
    (`(,x . ,y) (cons (radian--remove-sharp-quotes x)
                      (radian--remove-sharp-quotes y)))
    ((pred vectorp)
     (apply #'vector (mapcar #'radian--remove-sharp-quotes form)))
    (x x)))

(radian-defadvice radian--advice-use-package-bind-handle-sharp-quotes
    (args)
  :filter-args #'use-package-normalize-binder
  "Make `use-package' handle sharp-quoted functions correctly in `:bind'.
It is unclear to me why this is needed, as JW said explicitly to
the contrary in
<https://github.com/jwiegley/use-package/issues/461#issuecomment-348045772>.
Nevertheless we hack around the issue by simply doing a recursive
find-and-replace on sharp quotes in the arguments, because that's
the simple solution and the performance overhead is unimportant
since it happens during compilation anyway. (No, I'm not willing
to give up my sharp quotes; having autocompletion is really
nice.)"
  (radian--remove-sharp-quotes args))

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

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use my mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking Radian CI.
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop"))

;; Only needed at compile time, thanks to Jon
;; <https://github.com/raxod502/el-patch/pull/11>.
(eval-when-compile
  (require 'el-patch))

;;; Keybindings

;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*' and `unbind-key') which provides a much prettier API
;; for manipulating keymaps than `define-key' and `global-set-key' do.
;; It's also the same API that `:bind' and similar keywords in
;; `use-package' use.
(use-package bind-key
  :demand t)

(defvar radian-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a prefix.
This keymap is bound under \\[radian-keymap].")

(bind-key* "M-P" radian-keymap)

(defmacro radian-bind-key (key-name command &optional predicate)
  "Bind a key in `radian-keymap'.
KEY-NAME, COMMAND, and PREDICATE are as in `bind-key'."
  `(bind-key ,key-name ,command radian-keymap ,predicate))

(defun radian-join-keys (&rest keys)
  "Join key sequences KEYS. Empty strings and nils are discarded.
\(radian--join-keys \"\\[radian-keymap] e\" \"e i\")
  => \"\\[radian-keymap] e e i\"
\(radian--join-keys \"\\[radian-keymap]\" \"\" \"e i\")
  => \"\\[radian-keymap] e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

(radian-defadvice radian--quoted-insert-allow-quit (quoted-insert &rest args)
  :around #'quoted-insert
  "Allow quitting out of \\[quoted-insert] with \\[keyboard-quit]."
  (radian-flet ((defun insert-and-inherit (&rest args)
                  (dolist (arg args)
                    (when (equal arg ?\C-g)
                      (signal 'quit nil)))
                  (apply insert-and-inherit args)))
    (apply quoted-insert args)))

;;; Environment
;;;; Environment variables

(defvar radian--env-setup-p nil
  "Non-nil if `radian-env-setup' has completed at least once.")

(defun radian-env-setup (&optional again)
  "Load ~/.profile and set environment variables exported therein.
Only do this once, unless AGAIN is non-nil."
  (interactive (list 'again))
  ;; No need to worry about race conditions because Elisp isn't
  ;; concurrent (yet).
  (unless (and radian--env-setup-p (not again))
    (let (;; Current directory may not exist in certain horrifying
          ;; circumstances (yes, this has happened in practice).
          (default-directory "/")
          (profile-file "~/.profile")
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
                      (progn
                        (cl-loop for (var value) on results by #'cddr do
                                 (setenv var value)
                                 (when (string= var "PATH")
                                   (setq exec-path (append
                                                    (parse-colon-path value)
                                                    (list exec-directory)))))
                        (setq radian--env-setup-p t))
                    (message
                     "Loading %s produced malformed result; see buffer %S"
                     profile-file
                     buf-name)))
              (message "Failed to load %s; see buffer %S"
                       profile-file
                       buf-name))))))))

(defvar radian--env-setup-timer
  (run-at-time 1 nil #'radian-env-setup)
  "Timer used to run `radian-env-setup'.
We (mostly) don't need environment variables to be set correctly
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

    (eval-and-compile
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
          ;; yanked the first time you run `yank-pop'.
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
        (setq radian--clipboard-last-copy text)))

    (setq interprogram-paste-function #'radian--clipboard-paste)
    (setq interprogram-cut-function #'radian--clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(radian-defadvice radian--advice-gui-get-selection-quietly (func &rest args)
  :around #'gui-selection-value
  "Disable an annoying message emitted when Emacs can't yank something.
In particular, if you have an image on your system clipboard and
you either yank or kill (as `save-interprogram-paste-before-kill'
means Emacs will try to put the system clipboard contents into
the kill ring when you kill something new), you'll get the
message 'gui-get-selection: (error \"Selection owner couldn't
convert\" UTF8_STRING)'. Disable that."
  (radian--with-silent-message "Selection owner couldn't convert"
    (apply func args)))

;;;; Mouse integration

;; Scrolling is way too fast on macOS with Emacs 27 and on Linux in
;; general. Decreasing the number of lines we scroll per mouse event
;; improves the situation. Normally, holding shift allows this slower
;; scrolling; instead, we make it so that holding shift accelerates
;; the scrolling.
(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 5) ((control))))

;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
(unless (display-graphic-p)

  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode t)

  ;; Note that the reason for the next two functions is that
  ;; `scroll-down' and `scroll-up' scroll by a "near full screen"
  ;; by default, whereas we want a single line.

  (eval-and-compile
    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  ;; Enable scrolling with the mouse wheel.
  (bind-key "<mouse-4>" #'radian-scroll-down)
  (bind-key "<mouse-5>" #'radian-scroll-up))

;;; Candidate selection

(radian-defadvice radian--advice-eval-expression-save-garbage
    (func prompt &optional initial-contents keymap read &rest args)
  :around #'read-from-minibuffer
  "Save user input in history even if it's not a valid sexp.
We do this by forcing `read-from-minibuffer' to always be called
with a nil value for READ, and then handling the effects of READ
ourselves."
  (let ((input (apply func prompt initial-contents keymap nil args)))
    (when read
      ;; This is based on string_to_object in minibuf.c.
      (let ((result (read-from-string input)))
        (unless (string-match-p
                 "\\`[ \t\n]*\\'" (substring input (cdr result)))
          (signal
           'invalid-read-syntax
           '("Trailing garbage following expression")))
        (setq input (car result))))
    input))

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

;;; Window management

(radian-defadvice radian--advice-keyboard-quit-minibuffer-first
    (keyboard-quit)
  :around #'keyboard-quit
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

(radian-defadvice radian--advice-kill-buffer-maybe-kill-window
    (func &optional buffer-or-name kill-window-too)
  :around #'kill-buffer
  "Make it so \\[universal-argument] \\[kill-buffer] kills the window too."
  (interactive
   (lambda (spec)
     (append (or (advice-eval-interactive-spec spec) '(nil))
             current-prefix-arg)))
  (if kill-window-too
      (with-current-buffer buffer-or-name
        (kill-buffer-and-window))
    (funcall func buffer-or-name)))

;; Feature `windmove' provides keybindings S-left, S-right, S-up, and
;; S-down to move between windows. This is much more convenient and
;; efficient than using the default binding, C-x o, to cycle through
;; all of them in an essentially unpredictable order.
(use-feature windmove
  :demand t
  :config

  (windmove-default-keybindings)

  ;; Introduced in Emacs 27:

  (when (fboundp 'windmove-display-default-keybindings)
    (windmove-display-default-keybindings))

  (when (fboundp 'windmove-delete-default-keybindings)
    (windmove-delete-default-keybindings)))

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
  :bind (([remap list-buffers] . #'ibuffer)))

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

  (radian-defadvice radian--advice-save-place-quickly-and-silently
      (func &rest args)
    :around #'save-place-alist-to-file
    "Make `save-place' save more quickly and silently."
    (radian--with-silent-write
      (cl-letf (((symbol-function #'pp) #'prin1))
        (apply func args)))))

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.
(use-package projectile
  :defer 1
  :bind-keymap* (("C-c p" . projectile-command-map))
  :config

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  (setq projectile-switch-project-action 'projectile-commander)

  (def-projectile-commander-method ?\C-m
    "Find file in project."
    (call-interactively #'find-file))

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  (defun radian--projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))

  (put 'projectile-indexing-method 'safe-local-variable
       #'radian--projectile-indexing-method-p)

  ;; Can't bind M-r because some genius bound ESC. *Never* bind ESC!
  (dolist (key '("C-r" "R"))
    (bind-key key #'projectile-replace-regexp projectile-command-map))

  :blackout t)

(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
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
sequence after it is prefixed by \"\\[radian-keymap] e\", and the
second function is bound to the same key sequence, but prefixed
instead by \"\\[radian-keymap] o\".

This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\" and KEYBINDING is \"e i\". Then
`radian-register-dotfile' will create the interactive functions
`radian-find-init-el' and `radian-find-init-el-other-window', and
it will bind them to the key sequences \"\\[radian-keymap] e e
i\" and \"\\[radian-keymap] o e i\" respectively.

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
                             (downcase
                              bare-filename)))))))
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
                        (when (or (file-exists-p ,full-filename)
                                  (yes-or-no-p
                                   ,(format
                                     "Does not exist, really visit %s? "
                                     (file-name-nondirectory
                                      full-filename))))
                          (find-file ,full-filename))))
         (defun-other-window-form
           `(defun ,defun-other-window-name ()
              ,docstring-other-window
              (interactive)
              (when (or (file-exists-p ,full-filename)
                        (yes-or-no-p
                         ,(format
                           "Does not exist, really visit %s? "
                           (file-name-nondirectory
                            full-filename))))
                (find-file-other-window ,full-filename))))
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
 ,(expand-file-name "early-init.el" user-emacs-directory)
 "e e")
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

;; Shell
(radian-register-dotfile ".profile" "p r")
(radian-register-dotfile ".profile.local" "p l")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.local" "z l")

;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.
(use-feature auth-source
  :config

  (defvar radian--auth-source-blacklist-file
    (no-littering-expand-var-file-name "auth-source/blacklist.el")
    "File to store `auth-source' user blacklist.
The contents are a list of MD5 hashes, one for each potential
password that the user has decided not to save.")

  (radian-defadvice radian--advice-auth-source-persist-blacklist
      (func file add)
    :around #'auth-source-netrc-saver
    "Allow user to permanently disable prompt to save credentials."
    (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
           (blacklist
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents radian--auth-source-blacklist-file)
                (read (current-buffer))))))
      (unless (listp blacklist)
        (setq blacklist nil))
      (if (member key blacklist)
          ?n
        (radian-flet ((defun auth-source-read-char-choice (prompt choices)
                        (let ((choice (funcall auth-source-read-char-choice
                                               prompt choices)))
                          (when (= choice ?N)
                            (push key blacklist)
                            (make-directory
                             (file-name-directory
                              radian--auth-source-blacklist-file)
                             'parents)
                            (with-temp-file radian--auth-source-blacklist-file
                              (print blacklist (current-buffer)))
                            (setq choice ?n))
                          choice)))
          (funcall func file add))))))

;;; Saving files

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

;;; Editing
;;;; Text formatting

;; When region is active, make `capitalize-word' and friends act on
;; it.
(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-l" #'downcase-dwim)
(bind-key "M-u" #'upcase-dwim)

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
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; We could maybe use the variable `comment-auto-fill-only-comments'
;; for this, but I wrote this code before I knew about it. Also, I'm
;; not sure how well it handles the edge cases for docstrings and
;; such.
(radian-when-compiletime (version<= "26" emacs-version)
  (radian-defadvice radian--advice-auto-fill-only-text (func &rest args)
    :around #'internal-auto-fill
    "Only perform auto-fill in text, comments, or docstrings."
    (cl-block nil
      ;; Don't auto-fill on the first line of a docstring, since it
      ;; shouldn't be wrapped into the body.
      (when (and (derived-mode-p #'emacs-lisp-mode)
                 (eq (get-text-property (point) 'face) 'font-lock-doc-face)
                 (save-excursion
                   (beginning-of-line)
                   (looking-at-p "[[:space:]]*\"")))
        (cl-return))
      (when (and (derived-mode-p 'text-mode)
                 (not (derived-mode-p 'yaml-mode)))
        (apply func args)
        (cl-return))
      ;; Inspired by <https://emacs.stackexchange.com/a/14716/12534>.
      (when-let ((faces (save-excursion
                          ;; In `web-mode', the end of the line isn't
                          ;; fontified, so we have to step backwards
                          ;; by one character before checking the
                          ;; properties.
                          (ignore-errors
                            (backward-char))
                          (get-text-property (point) 'face))))
        (unless (listp faces)
          (setq faces (list faces)))
        (when (cl-some
               (lambda (face)
                 (memq face '(font-lock-comment-face
                              font-lock-comment-delimiter-face
                              font-lock-doc-face
                              web-mode-javascript-comment-face)))
               faces)
          ;; Fill Elisp docstrings to the appropriate column. Why
          ;; docstrings are filled to a different column, I don't know.
          (let ((fill-column (if (and
                                  (derived-mode-p #'emacs-lisp-mode)
                                  (memq 'font-lock-doc-face faces))
                                 emacs-lisp-docstring-fill-column
                               fill-column)))
            (apply func args)))))))

(blackout 'auto-fill-mode)

(defun radian--do-auto-fill ()
  "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
The reason we need this is that in order to enable auto-fill
globally, we are supposed to set the default value of variable
`auto-fill-function'. However, some major modes set
`normal-auto-fill-function' (itself normally set to
`do-auto-fill', which is what we generally set the default value
of variable `auto-fill-function' to), expecting `auto-fill-mode'
to be enabled afterwards (which copies the value of
`normal-auto-fill-function' into variable `auto-fill-function').
However, since we enable auto-fill globally by means of setting
variable `auto-fill-function' directly, this setting gets lost.
The workaround is to set variable `auto-fill-function' globally
to a function which looks up the value of
`normal-auto-fill-function' \(generally just `do-auto-fill') and
calls that. This is a slight inversion of the usual flow of
control and might make you slightly uncomfortable, but we'll just
have to live with it :3"
  (funcall normal-auto-fill-function))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function #'radian--do-auto-fill)

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

;; Feature `newcomment' provides commands for commenting and
;; uncommenting code, and editing comments.
(use-feature newcomment
  :bind (([remap default-indent-new-line] . #'radian-continue-comment))
  :config

  (defun radian-continue-comment ()
    "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:

If you have a comment like \";; Some text\" with point at the end
of the line, then running `default-indent-new-line' will get you
a new line with \";; \", but running it again will get you a line
with only \";;\" (no trailing whitespace). This is annoying for
inserting a new paragraph in a comment. With this command, the
two inserted lines are the same."
    (interactive)
    ;; `default-indent-new-line' uses `delete-horizontal-space'
    ;; because in auto-filling we want to avoid the space character at
    ;; the end of the line from being put at the beginning of the next
    ;; line. But when continuing a comment it's not desired.
    (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
      (default-indent-new-line))))

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
  :around #'kill-line
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
  :around #'read-passwd
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

;; Feature `warnings' allows us to enable and disable warnings.
(use-feature warnings
  :config

  ;; Ignore the warning we get when a huge buffer is reverted and the
  ;; undo information is too large to be recorded.
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

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
         ("M-/" . #'undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  (radian-defadvice radian--advice-suppress-undo-tree-buffer-modified-message
      (undo-tree-load-history &rest args)
    :around #'undo-tree-load-history
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
  :around #'set-mark-command
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
  :around #'pop-global-mark
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
        (while (and global-mark-ring
                    (not (marker-buffer (car (last global-mark-ring)))))
          (setq global-mark-ring (butlast global-mark-ring)))
        (let* ((marker (car (last global-mark-ring)))
               (buffer (marker-buffer marker))
               (position (marker-position marker)))
          (set-buffer buffer)
          (or (and (>= position (point-min))
                   (<= position (point-max)))
              (if widen-automatically
                  (widen)
                (error
                 "Global mark position is outside accessible part of buffer")))
          (goto-char position)
          (switch-to-buffer buffer)))
    (funcall pop-global-mark)))

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(use-feature bookmark
  :config

  (dolist (func '(bookmark-load bookmark-write-file))
    (advice-add func :around #'radian--advice-silence-messages)))

;;;; Find and replace

(radian-bind-key "c" #'toggle-case-fold-search)

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (([remap query-replace] . #'vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'radian-query-replace-literal))
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)

  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))

;; Feature `isearch' provides a basic and fast mechanism for jumping
;; forward or backward to occurrences of a given search string.
(use-feature isearch
  :config

  ;; Eliminate the 0.25s idle delay for isearch highlighting, as in my
  ;; opinion it usually produces a rather disjointed and distracting
  ;; UX.
  (setq lazy-highlight-initial-delay 0))

;;; Electricity: automatic things
;;;; Autorevert

;; On macOS, Emacs has a nice keybinding to revert the current buffer.
;; On other platforms such a binding is missing; we re-add it here.
(bind-key "s-u" #'revert-buffer)

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

  (radian-if-compiletime (version< emacs-version "27")
      (radian-defadvice radian--autorevert-only-visible
          (auto-revert-buffers &rest args)
        :around #'auto-revert-buffers
        "Inhibit `autorevert' for buffers not displayed in any window."
        (radian-flet ((defun buffer-list (&rest args)
                        (cl-remove-if-not
                         #'get-buffer-window (apply buffer-list args))))
          (apply auto-revert-buffers args)))
    (radian-defadvice radian--autorevert-only-visible (bufs)
      :filter-return #'auto-revert--polled-buffers
      "Inhibit `autorevert' for buffers not displayed in any window."
      (cl-remove-if-not #'get-buffer-window bufs)))

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

  (use-feature org
    :config

    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (use-feature org-agenda
    :config

    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (radian--smartparens-pair-setup #'prog-mode "(")
  (radian--smartparens-pair-setup #'prog-mode "[")
  (radian--smartparens-pair-setup #'prog-mode "{")
  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'latex-mode "\\[")
  (radian--smartparens-pair-setup #'markdown-mode "```")

  ;; It's unclear to me why any of this is needed.
  (radian--smartparens-pair-setup #'json-mode "[")
  (radian--smartparens-pair-setup #'json-mode "{")
  (radian--smartparens-pair-setup #'tex-mode "{")

  ;; Deal with `protobuf-mode' not using `define-minor-mode'.
  (radian--smartparens-pair-setup #'protobuf-mode "{")

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  :blackout t)

;;;; Code reformatting

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :init

  (apheleia-global-mode +1)

  (radian-defadvice radian--save-buffer-reformat-maybe (func &optional arg)
    :around #'save-buffer
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  ;; We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice.
  (blackout 'apheleia-mode)

  :blackout t)

;;;; Snippet expansion

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations. We prefer to use `yasnippet' instead, though.
(use-feature abbrev
  :blackout t)

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. The only reason we have it
;; here is because it gets pulled in by LSP, and we need to unbreak
;; some stuff.
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
      :around #'yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;; IDE features
;;;; Virtual environments
;;;;; Python

;; Package `pyvenv' provides functions for activating and deactivating
;; Python virtualenvs within Emacs. It's mostly not needed anymore now
;; that `lsp-python-ms' is configured to discover the appropriate
;; Pipenv or Poetry virtualenv, but maybe it will come in handy
;; someday.
(use-package pyvenv)

;;;; Language servers

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.
(use-package lsp-mode
  :init

  (radian-defhook radian--lsp-enable ()
    after-change-major-mode-hook
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   #'clojure-mode
                   #'ruby-mode
                   #'rust-mode))
        (lsp))))

  :config

  ;; We use Flycheck, not Flymake.
  (setq lsp-prefer-flymake nil)

  (defun radian--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.

This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
     ;; Messages we get when trying to start LSP (happens every time
     ;; we open a buffer).
     (member format `("No LSP server for %s(check *lsp-log*)."
                      "Connected to %s."
                      ,(concat
                        "Unable to calculate the languageId for current "
                        "buffer. Take a look at "
                        "lsp-language-id-configuration.")))
     ;; Errors we get from gopls for no good reason (I can't figure
     ;; out why). They don't impair functionality.
     (and (stringp (car args))
          (or (string-match-p "^no object for ident .+$" (car args))
              (string-match-p "^no identifier found$" (car args))))))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (radian-defadvice radian--lsp-run-from-node-modules (command)
    :filter-return #'lsp-resolve-final-function
    "Find LSP executables inside node_modules/.bin if present."
    (cl-block nil
      (prog1 command
        (when-let ((project-dir
                    (locate-dominating-file default-directory "node_modules"))
                   (binary
                    (radian--path-join
                     project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (radian-defhook radian--lsp-teardown ()
    kill-emacs-hook
    "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
    (setq lsp-restart nil))

  ;; Looks like `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; Also, it has a bunch of regexps which are completely wrong.
  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration))

  :blackout " LSP")

;; Feature `lsp-clients' from package `lsp-mode' defines how to
;; interface with the various popular LSP servers.
(use-feature lsp-clients
  :config

  ;; We want to make sure the PATH is set up correctly by now, since
  ;; otherwise we might not be able to find the LSP server binaries.
  (radian-env-setup))

;;;; Indentation

;; Don't use tabs for indentation. Use only spaces. Otherwise,
;; whenever the indent level does not equal the tab width (e.g. in
;; Emacs Lisp code, the indent level is 2 and the tab width is 8),
;; *both* tabs and spaces will be used for indentation. Disgusting.
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

(radian-defadvice radian--advice-indent-region-quietly (func &rest args)
  :around #'indent-region
  "Make `indent-region' shut up about its progress."
  (radian--with-silent-message "Indenting region"
    (apply func args)))

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 0.5
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
         ([remap completion-at-point] . #'company-manual-begin)
         ([remap complete-symbol] . #'company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . #'company-complete-selection)
         ("TAB" . #'company-complete-selection)

         ;; When was the last time you used the C-s binding for
         ;; searching candidates? It conflicts with buffer search,
         ;; anyway.
         ("C-s" . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . #'company-complete-selection)
         ("RET" . #'company-complete-selection)

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
         ("<up>" . #'company-select-previous)
         ("<down>" . #'company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . #'company-manual-begin))

  :config

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 1)

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

  (defvar-local radian--company-buffer-modified-counter nil
    "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

  (radian-defadvice radian--advice-company-complete-on-change ()
    :override #'company--should-begin
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 radian--company-buffer-modified-counter
          (setq radian--company-buffer-modified-counter tick)))))

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

;; Package `company-lsp' provides a Company backend for `lsp-mode'.
;; It's configured automatically by `lsp-mode'.
(use-package company-lsp
  :init

  (use-feature lsp
    :config

    (radian-defadvice radian--company-lsp-setup (&rest _)
      :after #'lsp
      "Disable `company-prescient' sorting by length in some contexts.
Specifically, disable sorting by length if the LSP Company
backend returns fuzzy-matched candidates, which implies that the
backend has already sorted the candidates into a reasonable
order."
      (setq-local company-prescient-sort-length-enable
                  (cl-dolist (w lsp--buffer-workspaces)
                    (when (thread-first w
                            (lsp--workspace-client)
                            (lsp--client-server-id)
                            (memq '(jsts-ls mspyls bash-ls texlab))
                            (not))
                      (cl-return t)))))))

;;;; Definition location

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(use-package dumb-jump
  :init/el-patch

  (defvar dumb-jump-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-M-g") 'dumb-jump-go)
      (define-key map (kbd "C-M-p") 'dumb-jump-back)
      (define-key map (kbd "C-M-q") 'dumb-jump-quick-look)
      map))

  (define-minor-mode dumb-jump-mode
    "Minor mode for jumping to variable and function definitions"
    :global t
    :keymap dumb-jump-mode-map)

  :init

  (dumb-jump-mode +1)

  :bind (:map dumb-jump-mode-map
              ("M-Q" . #'dumb-jump-quick-look))
  :bind* (("C-M-d" . #'dumb-jump-go-prompt)
          ("C-x 4 g" . #'dumb-jump-go-other-window)
          ("C-x 4 d" . #'radian-dumb-jump-go-prompt-other-window))
  :config

  (defun radian-dumb-jump-go-prompt-other-window ()
    "Like `dumb-jump-go-prompt' but use a different window."
    (interactive)
    (let ((dumb-jump-window 'other))
      (dumb-jump-go-prompt))))

;;;; Display contextual metadata

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
(use-feature eldoc
  :demand t
  :config

  (radian-when-compiletime (version<= "27" emacs-version)
    (el-patch-defun eldoc-print-current-symbol-info ()
      (el-patch-concat
        "Print the text produced by `eldoc-documentation-function'."
        (el-patch-add "\nDon't trample on existing messages."))
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (if (not (eldoc-display-message-p))
            ;; Erase the last message if we won't display a new one.
            (when eldoc-last-message
              (el-patch-swap
                (eldoc-message nil)
                (setq eldoc-last-message nil)))
          (let ((non-essential t))
            ;; Only keep looking for the info as long as the user
            ;; hasn't requested our attention.  This also locally
            ;; disables inhibit-quit.
            (while-no-input
              (eldoc-message (funcall eldoc-documentation-function))))))))

  (radian-when-compiletime (and (version< emacs-version "27")
                                (version<= "26" emacs-version))
    (el-patch-defun eldoc-print-current-symbol-info ()
      (el-patch-concat
        "Print the text produced by `eldoc-documentation-function'."
        (el-patch-add "\nDon't trample on existing messages."))
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (and (or (eldoc-display-message-p)
                 ;; Erase the last message if we won't display a new one.
                 (when eldoc-last-message
                   (el-patch-swap
                     (eldoc-message nil)
                     (setq eldoc-last-message nil))
                   nil))
             (eldoc-message (funcall eldoc-documentation-function))))))

  (radian-when-compiletime (version< emacs-version "26")
    (el-patch-defun eldoc-print-current-symbol-info ()
      ;; This is run from post-command-hook or some idle timer thing,
      ;; so we need to be careful that errors aren't ignored.
      (with-demoted-errors "eldoc error: %s"
        (and (or (eldoc-display-message-p)
                 ;; Erase the last message if we won't display a new one.
                 (when eldoc-last-message
                   (el-patch-swap
                     (eldoc-message nil)
                     (setq eldoc-last-message nil))
                   nil))
             (eldoc-message (funcall eldoc-documentation-function))))))

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!

  (use-feature flycheck
    :config

    (radian-defadvice radian--advice-disable-eldoc-on-flycheck
        (&rest _)
      :after-while #'eldoc-display-message-no-interference-p
      "Disable ElDoc when point is on a Flycheck overlay.
This prevents ElDoc and Flycheck from fighting over the echo
area."
      (not (flycheck-overlay-errors-at (point)))))

  (radian-defadvice radian--advice-eldoc-better-display-message-p (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    (member (current-message) (list nil eldoc-last-message)))

  :blackout t)

;;;; Syntax checking and code linting

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

  :bind-keymap (("C-c !" . #'flycheck-command-map))

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

;; Package `lsp-ui' provides Flycheck integration for `lsp-mode', as
;; well as various other UI elements that integrate with `lsp-mode'.
;; It's configured automatically by `lsp-mode'.
(use-package lsp-ui
  :bind (("C-c f" . #'lsp-ui-sideline-apply-code-actions))
  :config

  (radian-defadvice radian--advice-lsp-ui-apply-single-fix
      (orig-fun &rest args)
    :around #'lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (radian-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                        (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  ;; Don't show symbol definitions in the sideline. They are pretty
  ;; noisy, and there appears to currently be a bug where they prevent
  ;; Flycheck errors from being shown (the errors flash briefly and
  ;; then disappear).
  (setq lsp-ui-sideline-show-hover nil)

  (use-feature lsp-mode
    :config

    ;; With `lsp-ui', there's no need for the ElDoc integration
    ;; provided by `lsp-mode', and in fact for Bash it is very
    ;; annoying since all the hover information is multiline.
    (setq lsp-eldoc-enable-hover nil)))

;; Feature `lsp-ui-doc' from package `lsp-ui' displays documentation
;; in a child frame when point is on a symbol.
(use-feature lsp-ui-doc
  :config

  (radian-defadvice radian--advice-lsp-ui-doc-allow-multiline (func &rest args)
    :around #'lsp-ui-doc--render-buffer
    "Prevent `lsp-ui-doc' from removing newlines from documentation."
    (radian-flet ((defun replace-regexp-in-string
                      (regexp rep string &rest args)
                    (if (equal regexp "`\\([\n]+\\)")
                        string
                      (apply replace-regexp-in-string
                             regexp rep string args))))
      (apply func args))))

;;; Language support
;;;; Text-based languages

;; Feature `text-mode' provides a major mode for editing plain text.
(use-feature text-mode
  :config

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
    :override #'c-update-modeline
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

  (put 'c-default-style 'safe-local-variable #'stringp))

;;;; Clojure
;; https://clojure.org/

;; Package `clojure-mode' provides a major mode for Clojure.
(use-package clojure-mode)

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

  :blackout t)

;; Feature `cider-doc' from package `cider' handles rendering Clojure
;; function and variable docstrings, etc.
(use-feature cider-doc
  :init

  ;; Here we deal with a really weird and dumb bug
  ;; <https://github.com/raxod502/radian/issues/446>. The problem is
  ;; fundamentally that CIDER wants to do some color calculations when
  ;; it's loaded, whereas in fact there's no reason to do this until
  ;; something is actually rendered.

  (setq cider-docview-code-background-color nil)

  :config

  ;; Wherein we hope nobody else is relying on sticking obsolete
  ;; advices onto these functions.
  (ad-deactivate 'enable-theme)
  (ad-deactivate 'disable-theme)

  (radian-defadvice radian--advice-cider-hack-color-calculation (&rest _)
    :before #'cider-docview-fontify-code-blocks
    "Set `cider-docview-code-background-color'.
This is needed because we have ripped out the code that would
normally set it (since that code will run during early init,
which is a problem)."
    (setq cider-docview-code-background-color (cider-scale-background-color))))

;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.
(use-package go-mode
  :config

  (defvar radian--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun radian--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward radian--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (radian--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (radian--go-beginning-of-defun)
        (radian--go-end-of-defun)
        (radian--go-end-of-defun))
      (radian--go-beginning-of-defun))))

  (defun radian--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward radian--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (radian-defhook radian--go-defun-setup ()
    go-mode-hook
    "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
    (setq-local beginning-of-defun-function #'radian--go-beginning-of-defun)
    (setq-local end-of-defun-function #'radian--go-end-of-defun))

  (use-feature lsp-ui
    :config

    (radian-defadvice radian--advice-lsp-ui-organize-imports-more-cleanly
        (func actions &rest args)
      :around #'lsp-ui-sideline--code-actions
      "Clean up the \"Organize Imports\" code actions for Go.
Firstly, don't display \"Organize Imports\" or \"Organize All
Imports\" in the sideline, as gopls sometimes reports these code
actions when the indentation is wrong (rather than when imports
need to be changed). Secondly, filter out \"Organize All
Imports\" internally, so that applying a code action will default
to \"Organize Imports\" instead of prompting you to decide
between that and \"Organize All Imports\" (which does the same
thing as far as I can tell)."
      (let ((actions-to-keep nil)
            (actions-to-render nil))
        (dolist (action actions)
          (unless (equal "Organize All Imports" (gethash "title" action))
            (push action actions-to-keep)
            (unless (equal "Organize Imports" (gethash "title" action))
              (push action actions-to-render))))
        (setq actions-to-keep (nreverse actions-to-keep))
        (setq actions-to-render (nreverse actions-to-render))
        (when actions-to-render
          (apply func actions-to-render args))
        (setq lsp-ui-sideline--code-actions actions-to-keep)))))

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
    :around #'back-to-indentation
    "Fix `back-to-indentation' in `literate-haskell-mode'.
Otherwise, it just moves point to column 0, which is wrong.

This works around an upstream bug; see
<https://github.com/haskell/haskell-mode/issues/1594>."
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

  ;; Allow `haskell-mode' to use Stack with the global project instead
  ;; of trying to invoke GHC directly, if not inside any sort of
  ;; project.
  (setq haskell-process-type 'stack-ghci))

;; Package `lsp-haskell' configures the HIE Haskell language server
;; for use with `lsp-mode'.
(use-package lsp-haskell
  :demand t
  :after (:all lsp-clients haskell-mode))

;;;; Lua
;; <http://www.lua.org/>

;; Package `lua-mode' provides a major mode for Lua code.
(use-package lua-mode)

;;;; Makefile

;; Feature `make-mode' provides major modes for editing Makefiles.
(use-feature make-mode
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(use-package markdown-mode
  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         :map markdown-mode-style-map
         ("C-p" . #'markdown-insert-pre)
         ("C-q" . #'markdown-insert-blockquote)
         :map markdown-mode-map
         ("TAB" . #'radian-markdown-tab)
         ;; Try to override all the bindings in
         ;; `markdown-mode-map'...
         ("<S-iso-lefttab>" . #'radian-markdown-shifttab)
         ("<S-tab>" . #'radian-markdown-shifttab)
         ("<backtab>" . #'radian-markdown-shifttab))
  :config

  (defun radian-markdown-tab ()
    "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-forward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-demote-list-item))
     (t
      ;; Ew. But `markdown-indent-line' checks to see if
      ;; `this-command' is `markdown-cycle' before doing something
      ;; useful, so we have to.
      (let ((this-command 'markdown-cycle))
        (indent-for-tab-command)))))

  (defun radian-markdown-shifttab ()
    "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-backward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-promote-list-item))))

  (radian-defhook radian--flycheck-markdown-setup ()
    markdown-mode-hook
    "Disable some Flycheck checkers for Markdown."
    (radian--flycheck-disable-checkers
     'markdown-markdownlint-cli
     'markdown-mdl
     'proselint))

  (radian-defadvice radian--disable-markdown-metadata-fontification (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))

;;;; Protobuf

;; Package `protobuf-mode' provides a major mode for Protobuf.
(use-package protobuf-mode)

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

  (radian-defhook radian--python-use-correct-executables ()
    python-mode-hook
    "Use the correct Python executables for tooling."
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
      (setq-local flycheck-python-flake8-executable "flake8")
      ;; Use the correct executable for the language server.
      (setq-local lsp-python-executable-cmd executable)))

  ;; I honestly don't understand why people like their packages to
  ;; spew so many messages.
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun radian--python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv))))))))

  (radian-defhook radian--flycheck-python-setup ()
    python-mode-hook
    "Disable some Flycheck checkers for Python."
    (radian--flycheck-disable-checkers 'python-flake8)))

;; Package `lsp-python-ms' downloads Microsoft's LSP server for Python
;; and configures it with `lsp-mode'. Microsoft's server behaves
;; better than Palantir's in my opinion.
(use-package lsp-python-ms
  :demand t
  :after (:all lsp-clients python)
  :config

  (radian-defadvice radian--lsp-python-ms-silence (func &rest args)
    :around #'lsp-python-ms--language-server-started-callback
    "Inhibit a silly message."
    (radian--with-silent-message "Python language server started"
      (apply func args)))

  (radian-defadvice radian--lsp-python-ms-discover-virtualenvs
      (func &rest args)
    :around #'lsp-python-ms--extra-init-params
    "Automatically discover Pipenv and Poetry virtualenvs."
    (let ((lsp-python-ms-extra-paths lsp-python-ms-extra-paths)
          (exec-path exec-path))
      (when-let ((venv (radian--python-find-virtualenv)))
        (setq lsp-python-ms-extra-paths
              (file-expand-wildcards
               (expand-file-name
                "lib/python*/site-packages" venv)))
        (push (expand-file-name "bin" venv) exec-path))
      (apply func args))))

;;;; ReST
;; http://docutils.sourceforge.net/rst.html

;; Feature `rst' provides a major mode for ReST.
(use-feature rst
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
      (define-key
        map [remap delete-backward-char] 'ruby-electric-delete-backward-char)
      (define-key map [remap newline] 'ruby-electric-space/return)
      (define-key map [remap newline-and-indent] 'ruby-electric-space/return)
      (define-key
        map [remap electric-newline-and-maybe-indent]
        'ruby-electric-space/return)
      (define-key
        map [remap reindent-then-newline-and-indent]
        'ruby-electric-space/return)
      (el-patch-remove
        (dolist (x ruby-electric-delimiters-alist)
          (let* ((delim   (car x))
                 (plist   (cdr x))
                 (name    (plist-get plist :name))
                 (func    (plist-get plist :handler))
                 (closing (plist-get plist :closing)))
            (define-key map (char-to-string delim) func)
            (if closing
                (define-key
                  map (char-to-string closing) 'ruby-electric-closing-char)))))
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
  :config

  (dolist (func '(sh-set-shell sh-make-vars-local))
    (advice-add func :around #'radian--advice-silence-messages))

  (radian-defhook radian--sh-prettify-mode-line ()
    sh-mode-hook
    "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
    ;; Only do this for `sh-mode', not derived modes such as
    ;; `pkgbuild-mode'.
    (setq mode-line-process nil)
    (when (eq major-mode 'sh-mode)
      (setq mode-name (capitalize (symbol-name sh-shell)))))

  (use-feature lsp-clients
    :config

    ;; Only activate the Bash LSP server in Bash code, not all shell
    ;; script code. It's not very helpful to get Bash syntax errors
    ;; while editing Zsh code.
    (radian-protect-macros
      (setf (lsp--client-activation-fn (gethash 'bash-ls lsp-clients))
            (lambda (&rest _)
              (memq sh-shell '(sh bash)))))))

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
    :around #'TeX-load-style-file
    "Inhibit the \"Loading **/auto/*.el (source)...\" messages."
    (radian-flet ((defun load (file &optional
                                    noerror _nomessage
                                    nosuffix must-suffix)
                    (funcall
                     load file noerror 'nomessage nosuffix must-suffix)))
      (funcall TeX-load-style-file file)))

  (radian-defadvice radian--advice-inhibit-tex-removing-duplicates-message
      (TeX-auto-list-information name)
    :around #'TeX-auto-list-information
    "Inhibit the \"Removing duplicates...\" messages."
    (let ((inhibit-message t))
      (funcall TeX-auto-list-information name)))

  (radian-defhook radian--flycheck-tex-setup ()
    TeX-mode-hook
    "Disable some Flycheck checkers in TeX buffers."
    (radian--flycheck-disable-checkers 'tex-chktex 'tex-lacheck))

  (radian-defadvice radian--advice-tex-simplify-mode-name (&rest _)
    :after #'TeX-set-mode-name
    "Remove frills from the `mode-name' in TeX modes.
In practice, this means removing the stuff that comes after the
slash, e.g. \"LaTeX/P\" becomes just \"LaTeX\"."
    (setq mode-name TeX-base-mode-name)))

;; Feature `tex-buf' from package `auctex' provides support for
;; running TeX commands and displaying their output.
(use-feature tex-buf
  :config

  ;; Save buffers automatically when compiling, instead of prompting.
  (setq TeX-save-query nil)

  (radian-defadvice radian--advice-hide-tex-compilation-buffers (name)
    :filter-return #'TeX-process-buffer-name
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

  (radian-defadvice radian--latex-environment-kill-extra-newline
      (func &rest args)
    :around #'LaTeX-insert-environment
    "Prevent inserting a new environment from adding an unnecessary newline.
Specifically, this fixes a bug that happens when you insert a new
environment with point at the end of a non-empty line of text."
    (let ((needs-fixup (save-excursion
                         (beginning-of-line)
                         (re-search-forward
                          "[^[:space:]]" (point-at-eol) 'noerror))))
      (prog1 (apply func args)
        (when needs-fixup
          (save-excursion
            (forward-line 2)
            (when (looking-at "\n")
              (delete-char 1)))))))

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

;; Package `lsp-latex' provides an `lsp-mode' client for LaTeX.
(use-package lsp-latex
  :straight (:host github :repo "ROCKTAKEY/lsp-latex")
  :demand t
  :after (:all lsp-clients tex))

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

;;;; Web
;; https://developer.mozilla.org/en-US/docs/web/HTML
;; https://developer.mozilla.org/en-US/docs/Web/CSS
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript. We
;; don't use it (because `web-mode' is better), but we still configure
;; some of its variables because `json-mode' uses them.
(use-feature js
  :config

  ;; Default is 4, and nobody should indent JSON with four spaces.
  (setq js-indent-level 2))

;; Package `web-mode' provides a major mode for HTML, CSS, JavaScript,
;; and every conceivable thing adjacent (TypeScript, JSX, TSX, PSP,
;; ASP, Handlebars, etc.) all at once.
(use-package web-mode
  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at <http://web-mode.org/> as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;; My additions.
         ("\\.ejs\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  ;; Use `web-mode' rather than `js-mode' for scripts.
  :interpreter (("js" . web-mode)
                ("node" . web-mode))
  :config

  ;; Indent by two spaces by default. Compatibility with Prettier.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t)

  ;; Insert matching tags automatically. Why this is "mode 2", I have
  ;; not the slightest idea.
  (setq web-mode-auto-close-style 2)

  ;; Don't insert quotes automatically. It messes with JSX.
  (setq web-mode-enable-auto-quoting nil)

  ;; When using `web-mode' to edit JavaScript files, support JSX tags.
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.js[x]?\\'"))

  ;; Create line comments instead of block comments by default in
  ;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (radian-defhook radian--web-js-fix-comments ()
    web-mode-hook
    "Fix comment handling in `web-mode' for JavaScript."
    (when (member web-mode-content-type '("javascript" "jsx"))

      ;; For some reason the default is to insert HTML comments even
      ;; in JavaScript.
      (setq-local comment-start "//")
      (setq-local comment-end "")

      ;; Needed since otherwise the default value generated by
      ;; `comment-normalize-vars' will key off the syntax and think
      ;; that a single "/" starts a comment, which completely borks
      ;; auto-fill.
      (setq-local comment-start-skip "// *")))

  (use-feature apheleia
    :config

    (radian-defhook radian--web-highlight-after-formatting ()
      apheleia-post-format-hook
      "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
      (setq web-mode-fontification-off nil)
      (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
        (save-excursion
          (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end))))))

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
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config

  (radian-defhook radian--fix-json-indentation ()
    json-mode-hook
    "Set the tab width to 2 for JSON."
    (setq-local tab-width 2))

  (use-feature flycheck
    :config

    ;; Handle an error message that occurs when the buffer has only
    ;; whitespace, and in some other circumstances, which for some
    ;; bizarre reason still isn't handled correctly by Flycheck.
    (radian-protect-macros
      (cl-pushnew
       (cons
        (flycheck-rx-to-string
         `(and
           line-start
           (message "No JSON object could be decoded")
           line-end)
         'no-group)
        'error)
       (flycheck-checker-get 'json-python-json 'error-patterns)
       :test #'equal))))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(use-package pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(use-package pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(use-package ssh-config-mode
  :blackout "SSH-Config")

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.
(use-package terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.
(use-package toml-mode
  :mode "Pipfile\\'"
  ;; Correct the capitalization from "Toml" to "TOML".
  :blackout "TOML")

;; Package `yaml-mode' provides a major mode for YAML.
(use-package yaml-mode)

;;; Introspection
;;;; Help

;; Feature `help' powers the *Help* buffer and related functionality.
(use-feature help
  :bind (:map help-map
              ("M-k" . #'radian-describe-keymap))
  :config

  (radian-defadvice radian--advice-help-inhibit-hints (&rest _)
    :override #'help-window-display-message
    "Inhibit the \"Type q in help window to delete it\" hints.
Normally these are printed in the echo area whenever you open a
help buffer.")

  (radian-defadvice radian--advice-help-disable-revert-prompt
      (help-mode-revert-buffer ignore-auto _noconfirm)
    :around #'help-mode-revert-buffer
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
        'require-match
        nil nil (thing-at-point 'symbol)))))
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
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-symbol]   . #'helpful-symbol)
         ([remap describe-key]      . #'helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F" . #'helpful-function)
         ("M-f" . #'helpful-macro)
         ("C" . #'helpful-command)

         :map global-map
         ("C-c C-d" . #'helpful-at-point))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode radian-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if radian-universal-keyboard-quit-mode
        (radian-defadvice radian--advice-helpful-key-allow-keyboard-quit
            (&rest _)
          :before #'helpful-key
          "Make C-g work in `helpful-key'."
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'radian--advice-helpful-key-allow-keyboard-quit)))

  (radian-universal-keyboard-quit-mode +1)

  (radian-defadvice radian--advice-helpful-clone-emacs-source (library-name)
    :before #'helpful--library-path
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    (when (member (file-name-extension library-name) '("c" "rs"))
      (radian-clone-emacs-source-maybe))))

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
    :around #'elisp--company-doc-buffer
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  (radian-defadvice radian--advice-fill-elisp-docstrings-correctly (&rest _)
    :before-until #'fill-context-prefix
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
               (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  ;; The default mode lighter has a space instead of a hyphen.
  ;; Disgusting!
  :blackout (lisp-interaction-mode . "Lisp-Interaction"))

(defun radian-reload-init ()
  "Reload the init-file."
  (interactive)
  (message "Reloading init-file...")
  ;; Total hack here. I have no idea why it's needed. But, probably
  ;; due to some kind of disgusting Gilardi scenario, if we don't
  ;; explicitly load it here, the autoloading does not quite suffice
  ;; to make everything work out. Specifically, if we byte-compile the
  ;; init-file, start up using that init-file, then make a
  ;; modification to the init-file and reload using
  ;; `radian-reload-init', then all the `use-package' declarations
  ;; fail to recognize `:straight' as a supported keyword, strongly
  ;; suggesting there is some kind of eager macroexpansion that fails
  ;; because straight.el has not yet installed the `use-package'
  ;; integration. I would have thought that putting a `require'
  ;; statement inside `eval-when-compile' (or even a bare `require')
  ;; after we request `use-package' from straight.el would solve the
  ;; problem, but unfortunately it does not. As such, the hack.
  (require 'use-package)
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
        (eval-region start end)
        (message "Evaluating %s...done" name)))))

;; This keybinding is used for evaluating a buffer of Clojure code in
;; CIDER, and for evaluating a buffer of Scheme code in Geiser.
(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (bind-key "C-c C-k" #'radian-eval-buffer-or-region map))

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols.
SYMBOL is as in `xref-find-definitions'."
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
(bind-key "C-h C-f" #'find-function)
(bind-key "C-h C-v" #'find-variable)
(bind-key "C-h C-o" #'radian-find-symbol)
(bind-key "C-h C-l" #'find-library)

;; Let's establish a standard location for the Emacs source code.
(setq source-directory (expand-file-name "src" user-emacs-directory))

;; This is initialized to nil by `find-func' if the source is not
;; cloned when the library is loaded.
(setq find-function-C-source-directory
      (expand-file-name "src" source-directory))

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

;; Feature `find-func' provides the ability for you to locate the
;; definitions of Emacs Lisp functions and variables.
(use-feature find-func
  :config

  (radian-defadvice radian--advice-find-func-clone-emacs-source (&rest _)
    :before #'find-function-C-source
    "Clone Emacs source if needed to view definition."
    (radian-clone-emacs-source-maybe)))

;; Package `macrostep' provides a facility for interactively expanding
;; Elisp macros.
(use-package macrostep
  :bind (("C-c e" . #'macrostep-expand)))

;;;;; Emacs Lisp byte-compilation

;; Feature `bytecomp' handles byte-compilation of Emacs Lisp code.
(use-feature bytecomp
  :config

  ;; Eliminate two warnings that are essentially useless for me. The
  ;; `make-local' warning gets triggered every time you call
  ;; `define-minor-mode' inside of `use-package', and the `noruntime'
  ;; warning gets triggered basically all the time for everything.
  (setq byte-compile-warnings '(not make-local noruntime))

  (defun radian-batch-byte-compile ()
    "Byte-compile radian.el. For usage in batch mode."
    (byte-compile-file radian-lib-file))

  (defun radian-byte-compile (&optional report-progress)
    "Byte-compile radian.el. For interactive usage.
REPORT-PROGRESS non-nil (or interactively) means to print more
messages."
    (interactive (list 'report-progress))
    (cl-block nil
      (unless (file-newer-than-file-p
               radian-lib-file
               (concat radian-lib-file "c"))
        (when report-progress
          (message "Byte-compiled configuration already up to date"))
        (cl-return))
      (when report-progress
        (message "Byte-compiling updated configuration..."))
      (ignore-errors
        (kill-buffer " *radian-byte-compile*"))
      (let ((default-directory radian-directory))
        (make-process
         :name "radian-byte-compile"
         :buffer " *radian-byte-compile*"
         :command '("make" "compile")
         :noquery t
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (with-current-buffer (process-buffer proc)
               (if (= 0 (process-exit-status proc))
                   (progn
                     (insert "Byte-compilation completed successfully!\n")
                     (message
                      (if report-progress
                          "Byte-compiling updated configuration...done"
                        "Byte-compiled updated configuration")))
                 (save-match-data
                   (save-excursion
                     (goto-char (point-min))
                     (when (looking-at "In toplevel form:")
                       (forward-line))
                     (when (looking-at "radian\\.el:[0-9]+:[0-9]+:Warning: ")
                       (goto-char (match-end 0)))
                     (message "Failed to byte-compile%s"
                              (if (looking-at ".+")
                                  (format ": %s" (match-string 0))
                                " (no output)"))))))))))))

  :blackout (emacs-lisp-compilation-mode . "Byte-Compile"))

;;;;; Emacs Lisp linting

;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
(use-feature checkdoc
  :init

  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code. We use `with-eval-after-load' because `use-package'
;; is configured to try to `require' features during byte-compilation.
(with-eval-after-load 'elisp-lint
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
              ("C-<left>" . #'org-shiftleft)
              ("C-<right>" . #'org-shiftright)
              ("C-<up>" . #'org-shiftup)
              ("C-<down>" . #'org-shiftdown)

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
              ([remap backward-paragraph] . #'org-backward-paragraph)
              ([remap forward-paragraph] . #'org-forward-paragraph)

              ;; See discussion of this function below.
              ("C-M-RET" . #'radian-org-insert-heading-at-point)
              ("C-M-<return>" . #'radian-org-insert-heading-at-point))
  :bind* (;; Add the global keybindings for accessing Org Agenda and
          ;; Org Capture that are recommended in the Org manual.
          ("C-c a" . #'org-agenda)
          ("C-c c" . #'org-capture))
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
              ("C-<left>" . #'org-agenda-do-date-earlier)
              ("C-<right>" . #'org-agenda-do-date-later))
  :config

  (radian-defadvice radian--advice-org-agenda-default-directory
      (org-agenda &rest args)
    :around #'org-agenda
    "If `org-directory' exists, set `default-directory' to it in the agenda.
This makes the behavior of `find-file' more reasonable."
    (let ((default-directory (if (file-exists-p org-directory)
                                 org-directory
                               default-directory)))
      (apply org-agenda args)))

  (radian-defadvice radian--advice-blackout-org-agenda
      (&rest _)
    :override #'org-agenda-set-mode-name
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
          ("C-c C-x C-i" . #'org-clock-in)
          ("C-c C-x C-o" . #'org-clock-out)
          ("C-c C-x C-x" . #'org-clock-in-last)
          ("C-c C-x C-j" . #'org-clock-goto)
          ("C-c C-x C-q" . #'org-clock-cancel))

  :config

  (advice-add #'org-clock-load :around #'radian--advice-silence-messages)

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

;; Dired has some trouble parsing out filenames that have e.g. leading
;; spaces, unless the ls program used has support for Dired. GNU ls
;; has this support, so if it is available we tell Dired (and the
;; `list-directory' command, not that it sees much use) to use it.
;;
;; This is in an advice so that we can defer the PATH search until
;; necessary.
(radian-defadvice radian--use-gls-for-list-directory (&rest _)
  :before #'list-directory
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
              ("J" . #'dired-up-directory))
  :bind* (("C-x w" . radian-rename-current-file))
  :config

  (defun radian-rename-current-file (newname)
    "Rename file visited by current buffer to NEWNAME.
Interactively, prompt the user for the target filename, with
completion.

If NEWNAME is a directory then extend it with the basename of
`buffer-file-name'. Make parent directories automatically."
    (interactive
     (progn
       (unless buffer-file-name
         (user-error "Current buffer is not visiting a file"))
       (let ((newname (read-file-name "Rename to: " nil buffer-file-name)))
         (when (equal (file-truename newname)
                      (file-truename buffer-file-name))
           (user-error "%s" "Can't rename a file to itself"))
         (list newname))))
    (unless buffer-file-name
      (error "Current buffer is not visiting a file"))
    (when (equal (file-truename newname)
                 (file-truename buffer-file-name))
      (error "%s: %s" "Can't rename a file to itself" newname))
    (when (equal newname (file-name-as-directory newname))
      (setq newname
            (concat newname (file-name-nondirectory buffer-file-name))))
    (make-directory (file-name-directory newname) 'parents)
    ;; Passing integer as OK-IF-ALREADY-EXISTS means prompt for
    ;; confirmation before overwriting. Why? Who can say...
    (dired-rename-file buffer-file-name newname 0))

  (radian-defadvice radian--advice-dired-check-for-ls-dired (&rest _)
    :before #'dired-insert-directory
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
         ("C-x C-j" . #'dired-jump)
         ("C-x 4 C-j" . #'dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil)

  (radian-with-operating-system macOS
    (radian-defadvice radian--advice-dired-guess-open-on-macos
        (&rest _)
      :override #'dired-guess-default
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
         ("M-x" . #'execute-extended-command)
         ("C-h" . #'help-command)))

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

;; Package `transient' is the interface used by Magit to display
;; popups.
(use-package transient
  :config

  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . #'magit-status)
         ;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . #'magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . #'magit-file-dispatch))

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
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))


  :config

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

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; Feature `magit-diff' from package `magit' handles all the stuff
;; related to interactive Git diffs.
(use-feature magit-diff
  :config

  (radian-defadvice radian--magit-diff-revert-before-smerge (buf _pos)
    :before #'magit-diff-visit-file--setup
    "Before calling `smerge-start-session', try to revert buffer.
This is necessary because it's possible that the file being
visited has changed on disk (due to merge conflict, for example)
but it was already visited, and hasn't been autoreverted yet
(because it hasn't been visible in a window, for example). But
`smerge-start-session', which is called by Magit while jumping
you to the file, will not wait for an autorevert. It will just
see that there aren't any conflict markers in the file and
disable itself. Sad."
    (with-current-buffer buf
      (auto-revert-handler))))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(use-feature git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
(use-feature emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el. See
  ;; <https://github.com/raxod502/straight.el/issues/274> for not
  ;; having to use the internal function `straight--dir'.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

  :config

  (radian-defadvice radian--advice-emacsql-no-compile-during-compile
      (&rest _)
    :before-until #'emacsql-sqlite-ensure-binary
    "Prevent EmacSQL from trying to compile stuff during byte-compilation.
This is a problem because Forge tries to get EmacSQL to compile
its binary at load time, which is bad (you should never do
anything significant at package load time) since it breaks CI."
    byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
(use-package forge)

;; Feature `forge-core' from package `forge' implements the core
;; functionality.
(use-feature forge-core
  :config

  (radian-defadvice radian--forge-get-repository-lazily (&rest _)
    :before-while #'forge-get-repository
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents having EmacSQL try to build its binary (which may
be annoying, inconvenient, or impossible depending on the
situation) just because you tried to do literally anything with
Magit."
    (file-executable-p emacsql-sqlite-executable))

  (radian-defadvice radian--forge-build-binary-lazily (&rest _)
    :before #'forge-dispatch
    "Make `forge-dispatch' build the binary if necessary.
Normally, the binary gets built as soon as Forge is loaded, which
is terrible UX. We disable that above, so we now have to manually
make sure it does get built when we actually issue a Forge
command."
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2))))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(use-package git-gutter
  :commands (radian-git-gutter:beginning-of-hunk)
  :init

  (radian-bind-key "v p" #'git-gutter:previous-hunk)
  (radian-bind-key "v n" #'git-gutter:next-hunk)
  (radian-bind-key "v a" #'radian-git-gutter:beginning-of-hunk)
  (radian-bind-key "v e" #'git-gutter:end-of-hunk)
  (radian-bind-key "v k" #'git-gutter:revert-hunk)

  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  (radian-defhook radian--git-gutter-load ()
    find-file-hook
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'radian--git-gutter-load))

  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  (global-git-gutter-mode +1)

  (defun radian-git-gutter:beginning-of-hunk ()
    "Move to beginning of current diff hunk."
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        ;; This will move backwards since lines will be negative.
        (forward-line lines))))

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).

  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (ad-deactivate #'quit-window)
  (ad-deactivate #'switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (radian-defhook radian--git-gutter-on-buffer-or-window-change ()
    post-command-hook
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new radian--git-gutter-last-buffer-and-window)
        (setq radian--git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter))))))))

  (use-feature autorevert
    :config

    (radian-defhook radian--git-gutter-after-autorevert ()
      after-revert-hook
      "Update `git-gutter' after the buffer is autoreverted."
      (when git-gutter-mode
        (git-gutter))))

  (use-feature apheleia
    :config

    (radian-defhook radian--git-gutter-after-apheleia ()
      apheleia-post-format-hook
      "Update `git-gutter' after Apheleia formats the buffer."
      (when git-gutter-mode
        (git-gutter))))

  :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
(straight-register-package 'git-gutter-fringe)
(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (use-package git-gutter-fringe
    :demand t
    :after git-gutter
    :init

    (use-feature git-gutter
      :config

      ;; This function is only available when Emacs is built with
      ;; X/Cocoa support, see e.g.
      ;; <https://github.com/pft/mingus/issues/5>. If we try to
      ;; load/configure `git-gutter-fringe' without it, we run into
      ;; trouble.
      (when (fboundp 'define-fringe-bitmap)
        (require 'git-gutter-fringe)))

    :config

    (fringe-helper-define 'radian--git-gutter-blank nil
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (radian-defadvice radian--advice-git-gutter-remove-bitmaps
        (func &rest args)
      :around #'git-gutter-fr:view-diff-infos
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      (radian-flet ((defun fringe-helper-insert-region
                        (beg end _bitmap &rest args)
                      (apply fringe-helper-insert-region
                             beg end 'radian--git-gutter-blank args)))
        (apply func args)))))

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
    :filter-return #'compilation-start
    "Pop to compilation buffer on \\[compile]."
    (prog1 buf
      (select-window (get-buffer-window buf)))))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(use-package rg
  :straight (:host github :repo "dajva/rg.el" :branch "develop")
  :bind* (("C-c k" . #'radian-rg))
  :config

  (defun radian-rg (&optional only-current-type)
    "Search for string in current project.
With ONLY-CURRENT-TYPE non-nil, or interactively with prefix
argument, search only in files matching current type."
    (interactive "P")
    (rg-run (rg-read-pattern nil)
            (if only-current-type (car (rg-default-alias)) "*")
            (rg-project-root buffer-file-name))))

;;;; Internet applications

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(use-feature browse-url
  :init

  (defun radian--browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p
          #'markdown-mode #'org-mode #'org-agenda-mode #'magit-mode)))

  :bind* (:filter (radian--browse-url-predicate)
                  ("C-c C-o" . #'browse-url-at-point)))

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
    :after #'atomic-chrome-set-major-mode
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
    (when-let ((conn (websocket-server-conn
                      (atomic-chrome-get-websocket (current-buffer))))
               (browser
                (cond
                 ((eq conn atomic-chrome-server-ghost-text)
                  "Firefox")
                 ((eq conn atomic-chrome-server-atomic-chrome)
                  "Chromium")))
               (alt-browser
                (when (eq conn atomic-chrome-server-atomic-chrome)
                  "Google Chrome")))
      (cond
       ((radian-operating-system-p macOS)
        (call-process "open" nil nil nil "-a" browser))
       ((executable-find "wmctrl")
        (unless (or (zerop (call-process "wmctrl" nil nil nil "-a" browser))
                    (not alt-browser))
          (call-process "wmctrl" nil nil nil "-a" alt-browser))))))

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
    :around #'esup
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
              (setq custom-file
                    (expand-file-name
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
  :override #'display-startup-echo-area-message
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

  (defvar radian--restart-emacs-eager-hook-functions
    ;; This list contains hooks that I determined via profiling to be
    ;; slow (double-digit milliseconds).
    '(prescient--save
      radian--org-clock-save
      save-place-kill-emacs-hook)
    "List of functions on `kill-emacs-hook' which can be run eagerly.
If actually present on `kill-emacs-hook', then these functions
are run immediately on `save-buffers-kill-emacs'. This means that
Emacs shutdown appears to be slightly faster.

Functions can only be added here if it is okay to run them even
when shutting down Emacs is canceled. However, it is fine to put
functions here that aren't actually present on `kill-emacs-hook'.")

  (defvar radian--restart-emacs-eager-hook-functions-run nil
    "List of functions on `kill-emacs-hook' which have been run eagerly.
The global value of this variable is irrelevant; it is always
bound dynamically before being used.")

  (autoload #'restart-emacs--translate-prefix-to-args "restart-emacs")

  (radian-defadvice radian--advice-kill-emacs-dispatch
      (save-buffers-kill-emacs &optional arg)
    :around #'save-buffers-kill-emacs
    "Allow restarting Emacs or starting a new session on shutdown."
    (if radian--restart-in-progress
        (funcall save-buffers-kill-emacs arg)
      (let ((radian--restart-in-progress t)
            ;; Don't mutate the global value.
            (radian--restart-emacs-eager-hook-functions-run nil)
            (prompt (concat "Really exit (or restart, or start new, or kill) "
                            "Emacs? (y/n/r/e/k) "))
            (key nil))
        (dolist (func radian--restart-emacs-eager-hook-functions)
          ;; Run eager hook functions asynchronously while waiting for
          ;; user input. Use a separate idle timer for each function
          ;; because the order shouldn't be important, and because
          ;; that way if we don't actually restart then we can cancel
          ;; out faster (we don't have to wait for all the eager hook
          ;; functions to run).
          (run-with-idle-timer
           0 nil
           (lambda ()
             (when (and radian--restart-in-progress
                        (memq func kill-emacs-hook))
               (funcall func)
               ;; Thank goodness Elisp is single-threaded.
               (push func radian--restart-emacs-eager-hook-functions-run)))))
        (while (null key)
          (let ((cursor-in-echo-area t))
            (when minibuffer-auto-raise
              (raise-frame (window-frame (minibuffer-window))))
            (setq key
                  (read-key (propertize prompt
                                        'face 'minibuffer-prompt)))
            ;; No need to re-run the hooks that we already ran
            ;; eagerly. (This is the whole point of those
            ;; shenanigans.)
            (let ((kill-emacs-hook
                   (cl-remove-if
                    (lambda (func)
                      (memq
                       func
                       radian--restart-emacs-eager-hook-functions-run))
                    kill-emacs-hook)))
              (pcase key
                ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
                ((or ?n ?N))
                ((or ?r ?R)
                 (restart-emacs arg))
                ((or ?e ?E)
                 (radian-new-emacs
                  (restart-emacs--translate-prefix-to-args arg)))
                ((or ?k ?K) (radian-really-kill-emacs))
                (?\C-g (signal 'quit nil))
                (_ (setq key nil))))))
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
                                 ;; restart-emacs's autoloads would
                                 ;; not be present causing the the
                                 ;; --restart-emacs-desktop argument
                                 ;; to be unhandled
                                 (unless (member "-Q" translated-args)
                                   (restart-emacs--frame-restore-args))))
           (el-patch-remove
             (kill-emacs-hook
              (append kill-emacs-hook
                      (list (apply-partially
                             #'restart-emacs--launch-other-emacs
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

;; Allow you to resize frames however you want, not just in whole
;; columns. "The 80s called, they want their user interface back"
(setq frame-resize-pixelwise t)

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

;; Don't suggest shorter ways to type commands in M-x, since they
;; don't apply when using Selectrum.
(setq suggest-key-bindings nil)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

(radian-defadvice radian--advice-read-passwd-hide-char (func &rest args)
  :around #'read-passwd
  "Display passwords as **** rather than .... in the minibuffer.
This is the default behavior is Emacs 27, so this advice only has
an effect for Emacs 26 or below."
  (let ((read-hide-char (or read-hide-char ?*)))
    (apply func args)))

(setq minibuffer-message-properties '(face minibuffer-prompt))

;; Disable the contextual menu that pops up when you right-click.
(unbind-key "<C-down-mouse-1>")

;; The menu bar appears in both graphical and tty frames. Kill it.
(menu-bar-mode -1)

(when (display-graphic-p)

  ;; Disable unnecessary graphical elements.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1)

  (radian-with-operating-system macOS

    (radian-defhook radian--disable-menu-bar-again-on-macos (_)
      after-make-frame-functions
      "Disable the menu bar again, because macOS is dumb.
On macOS, for some reason you can't disable the menu bar once it
appears, and also `menu-bar-mode' doesn't prevent the menu bar
from appearing when run during early init. So we do a hack and
turn it off again after creating the first frame."
      (menu-bar-mode -1)))

  ;; Prevent the cursor from blinking. Do it two ways: using the minor
  ;; mode only works during regular init, while using the variable
  ;; only works during early init.
  (blink-cursor-mode -1)
  (setq no-blinking-cursor t)

  ;; Set the default font size.
  (when radian-font-size
    (set-face-attribute 'default nil :height radian-font-size))

  ;; Set the default font. No, I have no idea why we have to do it
  ;; this way. Using `set-face-attribute' does not have an effect,
  ;; unlike with the font size.
  (when radian-font
    (add-to-list 'default-frame-alist `(font . ,radian-font)))

  ;; Use the same font for fixed-pitch text as the rest of Emacs (you
  ;; *are* using a monospace font, right?).
  (set-face-attribute 'fixed-pitch nil :family 'unspecified)

  ;; On macOS, set the title bar to match the frame background.
  (radian-with-operating-system macOS
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

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

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

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

(radian--run-hook after-init)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(unless (bound-and-true-p radian--currently-profiling-p)
  (when (= 0 (random 100))
    (straight-prune-build-directory)))

;; We should only get here if init was successful. If we do,
;; byte-compile this file asynchronously in a subprocess using the
;; Radian Makefile. That way, the next startup will be fast(er).
(run-with-idle-timer
 1 nil
 #'radian-byte-compile)

;; Enable color theme as late as is humanly possible. This reduces
;; frame flashing and other artifacts during startup.
(when radian-color-theme-enable
  (use-feature zerodark-theme
    :demand t
    :config

    (let ((background-purple (if (true-color-p) "#48384c" "#5f5f5f"))
          (class '((class color) (min-colors 89)))
          (green (if (true-color-p) "#98be65" "#87af5f"))
          (orange (if (true-color-p) "#da8548" "#d7875f"))
          (purple (if (true-color-p) "#c678dd" "#d787d7")))
      (custom-theme-set-faces
       'zerodark
       `(selectrum-current-candidate
         ((,class (:background
                   ,background-purple
                   :weight bold
                   :foreground ,purple))))
       `(selectrum-primary-highlight ((,class (:foreground ,orange))))
       `(selectrum-secondary-highlight ((,class (:foreground ,green))))))

    (enable-theme 'zerodark)))

;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; indent-tabs-mode: nil
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:
