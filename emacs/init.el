;; -*- lexical-binding: t -*-

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
      (with-demoted-errors "%S"

        ;; Require some libraries that everyone needs, just to be
        ;; explicit about it.
        (require 'cl-lib)
        (require 'subr-x)

        (let* ((this-file
                (or
                 ;; If we are loading the init-file normally, then the
                 ;; filename is in `load-file-name'.
                 load-file-name
                 ;; Otherwise, that's nil and the filename is in
                 ;; `buffer-file-name' (this will happen if you
                 ;; `eval-buffer' for example).
                 buffer-file-name))
               (link-target
                ;; This function returns the target of the link. If the
                ;; init-file is not a symlink, then we abort.
                (file-symlink-p this-file)))

          (unless link-target
            (error "File %S is not a symlink" this-file))

          (let* ((lib-directory (expand-file-name
                                 "modules/"
                                 (file-name-directory link-target))))

            (unless (file-directory-p lib-directory)
              (error "Modules directory %S does not exist" lib-directory))

            (defvar radian-directory (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        link-target)))
              "Path to the Radian Git repository.")

            (defvar radian-lib-directory lib-directory
              "Path to the Radian Emacs modules directory.")))

        ;; Tell straight.el about the profiles we are going to be
        ;; using.
        (setq straight-profiles
              '((radian . "radian.el")
                (radian-local . "radian-local.el")
                (nil . "default.el")))

        ;; Use the master branch of straight.el on Radian's
        ;; master branch. (On develop of Radian, we use the develop
        ;; branch of straight.el.)
        (setq straight-repository-branch "master")

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
        (add-to-list 'load-path (directory-file-name radian-lib-directory))

        ;; Declare `straight-current-profile' as a special variable,
        ;; so we can dynamically bind it even though straight.el is
        ;; not yet loaded.
        (defvar straight-current-profile)

        ;; Load the Radian libraries.
        (let ((preloaded-features
               '(;; Compatibility functions may be needed
                 ;; anywhere, as they modify functions outside of
                 ;; the usual `require' tree.
                 radian-compat
                 ;; The package management layer is used almost
                 ;; everywhere, and now that we use the
                 ;; `use-feature' macro, it's used basically
                 ;; everywhere else too.
                 radian-package
                 ;; no-littering changes lots of paths and needs
                 ;; to be loaded as soon as possible.
                 radian-emacsd
                 ;; We want to avoid the Emacs-provided Org
                 ;; getting loaded, so we should load our Org as
                 ;; soon as possible.
                 radian-org))
              (radian-features (mapcar
                                (lambda (file)
                                  (intern (string-remove-suffix ".el" file)))
                                (directory-files
                                 radian-lib-directory nil
                                 "^[a-z-]+\\.el$")))
              ;; Any packages installed here are official Radian
              ;; packages.
              (straight-current-profile 'radian)
              (success t))

          ;; First we need to unload all the features, so that the
          ;; init-file can be reloaded to pick up changes.
          (dolist (feature radian-features)
            (setq features (remove feature features)))

          ;; Now load features that should be loaded first. If
          ;; errors occur while loading them, don't even try to
          ;; proceed with init.
          (dolist (feature preloaded-features)
            (require feature))

          ;; And then the rest of the features. If they fail to
          ;; load, maybe we can keep going, as long as we note
          ;; that there was an error.
          (dolist (feature radian-features)
            (unless (member feature preloaded-features)
              (condition-case-unless-debug error-data
                  (require feature)
                (error
                 (setq success nil)
                 (warn "Could not load `%S': %s" feature
                       (error-message-string error-data))))))

          ;; Run local customizations that are supposed to be run
          ;; after init. Any packages installed here are
          ;; user-local packages. (Packages installed
          ;; interactively do not belong to either `radian' or
          ;; `radian-local', and should not be written to either
          ;; lockfile.)
          (let ((straight-current-profile 'radian-local))
            (run-hooks 'radian-after-init-hook))

          ;; Prune the build cache for straight.el; this will
          ;; prevent it from growing too large. Only do this if we
          ;; definitely loaded all desired packages, however.
          (when success
            (straight-prune-build-cache)))))))
