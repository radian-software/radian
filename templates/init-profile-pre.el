(defvar radian-minimum-emacs-version "25.1")

(defvar radian-local-init-file "~/.emacs.d/init.local.el")

(defvar radian-directory
  (file-name-directory
   (file-truename (or load-file-name
                      buffer-file-name))))

(defvar radian-lib-directory
  (expand-file-name "radian-emacs/" radian-directory))

(setq straight-profiles
      '((radian . "radian.el")
        (radian-local . "radian-local.el")
        (nil . "default.el")))

(setq straight-recipe-overrides
      '((radian . ((straight :type git :host github
                             :repo "raxod502/straight.el"
                             :branch "develop"
                             :files ("straight.el"))))))

;; This is to prevent a bug where esup tries to step into
;; byte-compiled `cl-lib', and fails horribly.
(setq esup-child-profile-require-level 0)

(require 'cl-lib)
(require 'subr-x)

(cl-letf (((symbol-function #'internal-macroexpand-for-load) nil))
  (fmakunbound 'internal-macroexpand-for-load)
  (load radian-local-init-file 'noerror 'nomessage))

(add-to-list 'load-path radian-lib-directory)
