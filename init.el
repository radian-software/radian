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
      (dolist (file (directory-files radian-directory nil "^[a-z-]+\\.el$" 'nosort))
        (let ((feature (intern (string-remove-suffix ".el" file))))
          (condition-case-unless-debug error-data
              (require feature)
            (error (warn "Could not load `%S': %s" feature
                         (error-message-string error-data))))))

      ;; Run local customizations that are supposed to be run after
      ;; init.
      (when (fboundp 'radian-after-init)
        (radian-after-init)))

  ;; Report errors as warnings.
  (error (warn "%s" (error-message-string error-data))))
