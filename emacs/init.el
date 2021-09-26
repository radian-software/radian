;; -*- lexical-binding: t -*-

;; This file wraps the primary Radian configuration (which lives in
;; radian.el) so that we don't have to wrap the entire file in various
;; `let' forms, etc. We put as much as possible in radian.el.

;; This allows us to instead load a different Emacs configuration by
;; exporting USER_EMACS_DIRECTORY to another .emacs.d directory.
(let ((alternate-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))

  (defvar radian--init-file-loaded-p nil
    "Non-nil if the init-file has already been loaded.
This is important for Emacs 27 and above, since our early
init-file just loads the regular init-file, which would lead to
loading the init-file twice if it were not for this variable.")

  (cond
   ;; If already loaded, do nothing. But still allow re-loading, just
   ;; do it only once during init.
   ((and (not after-init-time) radian--init-file-loaded-p))

   ;; Delegate to another Emacs configuration. (We still don't want to
   ;; load it twice.)
   (alternate-user-emacs-directory
    (setq alternate-user-emacs-directory
          (file-name-as-directory alternate-user-emacs-directory))
    (setq user-emacs-directory alternate-user-emacs-directory)
    (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
    (load user-init-file 'noerror 'nomessage))
   (t
    (setq radian--init-file-loaded-p t)

    (defvar radian-minimum-emacs-version "26.1"
      "Radian Emacs does not support any Emacs version below this.")

    (defvar radian-local-init-file
      (expand-file-name "init.local.el" user-emacs-directory)
      "File for local customizations of Radian.")

    ;; Prevent package.el from modifying this file.
    (setq package-enable-at-startup nil)

    ;; Prevent Custom from modifying this file.
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (load custom-file 'noerror 'nomessage)

    ;; Make sure we are running a modern enough Emacs, otherwise abort
    ;; init.
    (if (version< emacs-version radian-minimum-emacs-version)
        (error (concat "Radian Emacs requires at least Emacs %s, "
                       "but you are running Emacs %s")
               radian-minimum-emacs-version emacs-version)

      (let* ((this-file (or user-init-file "~/.emacs.d/init.el"))
             (link-target
              ;; This function returns the target of the link. If the
              ;; init-file is not a symlink, then we abort.
              ;;
              ;; We may be loading init.el in batch mode, in which case
              ;; `user-init-file' is nil. In that case, we should have
              ;; some backup options to try.
              (file-symlink-p this-file)))

        (unless link-target
          (error "Init-file %S is not a symlink" this-file))

        (defvar radian-lib-file (expand-file-name
                                 "radian.el"
                                 (file-name-directory link-target))
          "File containing main Radian configuration.
This file is loaded by init.el.")

        (unless (file-exists-p radian-lib-file)
          (error "Library file %S does not exist" radian-lib-file))

        (defvar radian--finalize-init-hook nil
          "Hook run unconditionally after init, even if it fails.
Unlike `after-init-hook', this hook is run every time the
init-file is loaded, not just once.")

        (unwind-protect
            ;; Load the main Radian configuration code. Disable
            ;; `file-name-handler-alist' to improve load time.
            ;;
            ;; Make sure not to load an out-of-date .elc file. Since
            ;; we byte-compile asynchronously in the background after
            ;; init succeeds, this case will happen often.
            (let ((file-name-handler-alist nil)
                  (load-prefer-newer t)
                  (stale-bytecode t))
              (catch 'stale-bytecode
                ;; We actually embed the contents of the local
                ;; init-file directly into the compiled radian.elc, so
                ;; that it can get compiled as well (and its
                ;; macroexpansion can use packages that Radian only
                ;; loads at compile-time). So that means we have to go
                ;; the slow path if the local init-file has been
                ;; updated more recently than the compiled radian.elc.
                (when (file-newer-than-file-p
                       radian-local-init-file
                       (concat radian-lib-file "c"))
                  (throw 'stale-bytecode nil))
                (load
                 (file-name-sans-extension radian-lib-file)
                 nil 'nomessage)
                (setq stale-bytecode nil))
              (when stale-bytecode
                ;; Don't bother trying to recompile, unlike in
                ;; straight.el, since we are going to handle that
                ;; later, asynchronously.
                (ignore-errors
                  (delete-file (concat radian-lib-file "c")))
                (load radian-lib-file nil 'nomessage 'nosuffix)))
          (run-hooks 'radian--finalize-init-hook)))))))

;; Local Variables:
;; no-native-compile: t
;; End:
