;;; radian-package.el --- Package management

;; We aren't using package.el, but Emacs will initialize it for us if
;; we don't tell it not to.
(setq package-enable-at-startup nil)

;; Use live modification detection. This improves startup time.
(setq straight-check-for-modifications 'live-with-find)

;; Bootstrap the package manager, straight.el. For documentation, see
;; https://github.com/raxod502/straight.el. The following code loads
;; the package manager if it is already installed, and otherwise
;; installs it first.
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features. See
;; https://github.com/jwiegley/use-package for documentation.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load packages lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

;; Make a convenient macro for using `use-package' without installing
;; a package via straight.el.
(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent 1))
  `(use-package ,name
     :straight nil
     ,@args))

;; Copied from definition of `use-package'.
(put 'use-feature 'lisp-indent-function 'defun)

(provide 'radian-package)

;;; radian-package.el ends here
