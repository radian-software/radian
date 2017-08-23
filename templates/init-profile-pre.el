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

;; This is to prevent indentation from being lost in the profiling
;; results.
(advice-add #'esup-child-chomp :override #'string-trim)

;; This puts the esup init into a single straight.el transaction. This
;; requires some gymnastics since we haven't yet loaded straight.el,
;; and therefore can't use the transaction macro defined there.

(defun radian--advice-esup-init-as-transaction (esup-child-run &rest args)
  "Put the esup init within a single straight.el transaction.
Since we haven't loaded straight.el yet, the easiest way to do
this is to simply pretend that init has not finished yet, and
then manually end the transaction since `after-init-hook' will
not be run."
  (unwind-protect
      (let ((straight-treat-as-init t))
        (apply esup-child-run args))
    (straight-finalize-transaction)))

(advice-add #'esup-child-run :around
            #'radian--advice-esup-init-as-transaction)

(cl-letf (((symbol-function #'internal-macroexpand-for-load) nil))
  (fmakunbound 'internal-macroexpand-for-load)
  (load radian-local-init-file 'noerror 'nomessage))

(add-to-list 'load-path radian-lib-directory)
