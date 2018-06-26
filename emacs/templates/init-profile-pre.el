(defvar radian-local-init-file "~/.emacs.d/init.local.el")

(defvar radian-lib-directory
  (expand-file-name "radian-emacs/" radian-directory))

(setq straight-profiles
      '((radian . "radian.el")
        (radian-local . "radian-local.el")
        (nil . "default.el")))

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

(defun radian-advice-esup-init-as-transaction (esup-child-run &rest args)
  (unwind-protect
      (let ((straight-treat-as-init t))
        (apply esup-child-run args))
    (straight-finalize-transaction)))

(advice-add #'esup-child-run :around
            #'radian-advice-esup-init-as-transaction)

(defvar radian-after-init-hook nil)

(cl-letf (((symbol-function #'internal-macroexpand-for-load) nil))
  (fmakunbound 'internal-macroexpand-for-load)
  (load radian-local-init-file 'noerror 'nomessage))

(setq straight-current-profile 'radian)
