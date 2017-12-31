;;; radian-package.el --- Package management

;; We aren't using package.el, but Emacs will initialize it for us if
;; we don't tell it not to.
(setq package-enable-at-startup nil)

;; We are using a package manager called straight.el. This code, which
;; is taken from the README [1], bootstraps the system (because
;; obviously the package manager is unable to install and load itself,
;; if it is not already installed and loaded).
;;
;; [1]: https://github.com/raxod502/straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; To handle a lot of useful tasks related to package configuration,
;; we use a library called `use-package', which provides a macro by
;; the same name. This macro automates many common tasks, like
;; autoloading functions, binding keys, registering major modes, and
;; lazy-loading, through the use of keyword arguments. See the README
;; [1].
;;
;; [1]: https://github.com/jwiegley/use-package
(straight-use-package 'use-package)

;; Install packages by default.
(setq straight-use-package-by-default t)

;; Tell use-package to always load packages lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See [1].
;;
;; [1]: https://github.com/jwiegley/use-package#notes-about-lazy-loading
(setq use-package-always-defer t)

(provide 'radian-package)

;;; radian-package.el ends here
