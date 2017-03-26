(defvar radian-minimum-emacs-version "25.1"
  "Radian Emacs does not support any Emacs version below this.")

(defvar radian-local-init-file "~/.emacs.d/init.local.el"
  "File for local customizations of Radian.")

(defvar radian-directory "~/.emacs.d/radian/"
  "Path to Radian libraries.")

;; Prevent package.el from modifying this file if the rest of
;; init fails.
(setq package-enable-at-startup nil)

;; Prevent Custom from modifying this file if the rest of init
;; fails. We set the Custom file to the user's local
;; configuration file by default, but we do it early so that the
;; user can override it in that same file if they want.
(setq custom-file radian-local-init-file)

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((radian . "radian.el")
        (radian-local . "radian-local.el")
        (nil . "default.el")))

(condition-case-unless-debug error-data
    (progn
      ;; Make sure we are running a modern enough Emacs, otherwise
      ;; abort init.
      (when (version< emacs-version radian-minimum-emacs-version)
        (error (concat "Radian Emacs requires at least Emacs %s, "
                       "but you are running Emacs %s")
               radian-minimum-emacs-version emacs-version))

      ;; Require some libraries that everyone needs, just to be
      ;; explicit about it.
      (require 'cl-lib)
      (require 'subr-x)

      ;; Load local customizations.
      (load radian-local-init-file 'noerror 'nomessage)

      ;; Make the Radian libraries available.
      (add-to-list 'load-path radian-directory)

      ;; Load the Radian libraries.
      (let ((preloaded-features
             '(;; no-littering changes lots of paths and needs to be
               ;; loaded as soon as possible.
               radian-emacsd))
            (radian-features (mapcar
                              (lambda (file)
                                (intern (string-remove-suffix ".el" file)))
                              (directory-files
                               radian-directory nil
                               "^[a-z-]+\\.el$"
                               'nosort)))
            ;; Any packages installed here are official Radian packages.
            (straight-current-profile 'radian))
        ;; First we need to unload all the features, so that the
        ;; init-file can be reloaded to pick up changes.
        (dolist (feature radian-features)
          (setq features (remove feature features)))
        (dolist (feature preloaded-features)
          (condition-case-unless-debug error-data
              (require feature)
            (error (warn "Could not load `%S': %s" feature
                         (error-message-string error-data)))))
        (dolist (feature radian-features)
          (unless (member feature preloaded-features)
            (condition-case-unless-debug error-data
                (require feature)
              (error (warn "Could not load `%S': %s" feature
                           (error-message-string error-data)))))))

      ;; Run local customizations that are supposed to be run after
      ;; init. Any packages installed here are user-local
      ;; packages. (Packages installed interactively do not belong to
      ;; either `radian' or `radian-local', and should not be written
      ;; to either lockfile.)
      (let ((straight-current-profile 'radian-local))
        (when (fboundp 'radian-after-init)
          (radian-after-init)))

      ;; This helps out the package management system. See the
      ;; documentation on `straight-declare-init-succeeded'.
      (straight-declare-init-succeeded))

  ;; Report errors as warnings.
  (error (warn "%s" (error-message-string error-data))
         ;; This helps out the package management system. See the
         ;; documentation on `straight-declare-init-finished'.
         (when (fboundp 'straight-declare-init-finished)
           (straight-declare-init-finished))))
