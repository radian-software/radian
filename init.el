;; This allows us to instead load a different Emacs configuration by
;; exporting USER_EMACS_DIRECTORY to another .emacs.d directory.
(let ((alternate-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))

  (if alternate-user-emacs-directory
      (progn
        (setq alternate-user-emacs-directory
              (file-name-as-directory alternate-user-emacs-directory))
        (setq user-emacs-directory alternate-user-emacs-directory)
        (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
        (load user-init-file 'noerror 'nomessage))

    (defvar radian-minimum-emacs-version "25.1"
      "Radian Emacs does not support any Emacs version below this.")

    (defvar radian-local-init-file "~/.emacs.d/init.local.el"
      "File for local customizations of Radian.")

    ;; Prevent package.el from modifying this file if the rest of init
    ;; fails.
    (setq package-enable-at-startup nil)

    ;; Prevent Custom from modifying this file if the rest of init
    ;; fails. We set the Custom file to the user's local configuration
    ;; file by default, but we do it early so that the user can
    ;; override it in that same file if they want.
    (setq custom-file radian-local-init-file)

    ;; Make sure we are running a modern enough Emacs, otherwise abort
    ;; init. We have to do this outside the
    ;; `condition-case-unless-debug' form, since old Emacsen do not
    ;; actually have `condition-case-unless-debug' defined.
    (if (version< emacs-version radian-minimum-emacs-version)
        (warn (concat "Radian Emacs requires at least Emacs %s, "
                      "but you are running Emacs %s")
              radian-minimum-emacs-version emacs-version)

      ;; We have a modern Emacs, proceed with init.
      (unwind-protect
          (with-demoted-errors "%S"

            ;; Require some libraries that everyone needs, just to be
            ;; explicit about it.
            (require 'cl-lib)
            (require 'subr-x)

            (defvar radian-directory
              (let ((link-target
                     ;; This function returns the target of the link.
                     ;; If the init-file is not a symlink, then we
                     ;; abort.
                     (file-symlink-p
                      (or
                       ;; If we are loading the init-file normally,
                       ;; then the filename is in `load-file-name'.
                       load-file-name
                       ;; Otherwise, that's nil and the filename is in
                       ;; `buffer-file-name' (this will happen if you
                       ;; `eval-buffer' for example).
                       buffer-file-name))))
                ;; We identify a directory as the Radian repository by
                ;; the existence of a "radian-emacs" folder inside it.
                ;; Note that the previous check does disallow copying
                ;; the init-file and radian-emacs folder into
                ;; ~/.emacs.d, which is fine as that is a ridiculous
                ;; use case.
                (when (and
                       link-target
                       (file-directory-p (expand-file-name
                                          "radian-emacs/"
                                          (file-name-directory link-target))))
                  (file-name-directory
                   (file-truename link-target))))
              "Path to the Radian repository, or nil if not found.
This is an absolute path.")

            (defvar radian-lib-directory
              (and radian-directory
                   (expand-file-name "radian-emacs/" radian-directory))
              "Path to the Radian Emacs libraries, or nil if not found.
This is an absolute path.")

            ;; Fail fast if we can't find the Radian libraries.
            (unless radian-directory
              (error "Couldn't find Radian repository"))

            ;; Tell straight.el about the profiles we are going to be
            ;; using.
            (setq straight-profiles
                  '((radian . "radian.el")
                    (radian-local . "radian-local.el")
                    (nil . "default.el")))

            ;; Use the develop branch of straight.el on Radian's
            ;; develop branch. (On master of Radian, we use the master
            ;; branch of straight.el.)
            (setq straight-repository-branch "develop")

            (defvar radian-after-init-hook nil
              "Hook run after Radian init has finished.
This is where you should probably add most of your local init
code.")

            ;; Allow to disable local customizations with a
            ;; command-line argument.
            (if (member "--no-local" command-line-args)
                (progn
                  ;; Make sure to delete --no-local from the list,
                  ;; because otherwise Emacs will issue a warning
                  ;; about the unknown argument.
                  (setq command-line-args
                        (delete "--no-local" command-line-args)))

              ;; Load local customizations. We disable eager
              ;; macroexpansion here, since otherwise bad things can
              ;; happen with e.g. `el-patch' as the package management
              ;; system has not yet been loaded. See [1] for the hack
              ;; used to disable eager macroexpansion.
              ;;
              ;; [1]: https://emacs.stackexchange.com/a/17329/12534
              (cl-letf (((symbol-function #'internal-macroexpand-for-load) nil))
                (fmakunbound 'internal-macroexpand-for-load)
                (load radian-local-init-file 'noerror 'nomessage)))

            ;; Make the Radian libraries available.
            (add-to-list 'load-path radian-lib-directory)

            ;; Load the Radian libraries.
            (let ((preloaded-features
                   '(;; Compatibility functions may be needed
                     ;; anywhere, as they modify functions outside of
                     ;; the usual `require' tree.
                     radian-compat
                     ;; no-littering changes lots of paths and needs
                     ;; to be loaded as soon as possible.
                     radian-emacsd))
                  (radian-features (mapcar
                                    (lambda (file)
                                      (intern (string-remove-suffix ".el" file)))
                                    (directory-files
                                     radian-lib-directory nil
                                     "^[a-z-]+\\.el$")))
                  ;; Any packages installed here are official Radian
                  ;; packages.
                  (straight-current-profile 'radian))

              ;; First we need to unload all the features, so that the
              ;; init-file can be reloaded to pick up changes.
              (dolist (feature radian-features)
                (setq features (remove feature features)))

              ;; Now load features that should be loaded first.
              (dolist (feature preloaded-features)
                (condition-case-unless-debug error-data
                    (require feature)
                  (error (warn "Could not load `%S': %s" feature
                               (error-message-string error-data)))))

              ;; And then the rest of the features.
              (dolist (feature radian-features)
                (unless (member feature preloaded-features)
                  (condition-case-unless-debug error-data
                      (require feature)
                    (error (warn "Could not load `%S': %s" feature
                                 (error-message-string error-data))))))

              ;; Run local customizations that are supposed to be run
              ;; after init. Any packages installed here are
              ;; user-local packages. (Packages installed
              ;; interactively do not belong to either `radian' or
              ;; `radian-local', and should not be written to either
              ;; lockfile.)
              (let ((straight-current-profile 'radian-local))
                (run-hooks 'radian-after-init-hook))))))))
