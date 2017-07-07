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
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 1))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (delete-region (point-min) url-http-end-of-headers)
      (eval-buffer)))
  (load bootstrap-file nil 'nomessage))

;; To handle a lot of useful tasks related to package configuration,
;; we use a library called `use-package', which provides a macro by
;; the same name. This macro automates many common tasks, like
;; autoloading functions, binding keys, registering major modes, and
;; lazy-loading, through the use of keyword arguments. See the README
;; [1].
;;
;; We are using my fork until [2] is merged.
;;
;; [1]: https://github.com/jwiegley/use-package
;; [2]: https://github.com/jwiegley/use-package/pull/479
(straight-use-package '(use-package
                         :host github
                         :repo "raxod502/use-package"
                         :upstream (:host github
                                    :repo "jwiegley/use-package")))

;; Tell use-package to automatically install packages if they are
;; missing. By default, packages are installed via straight.el [1],
;; which draws package installation recipes (short lists explaining
;; where to download the package) from MELPA [2], GNU ELPA [3], and
;; EmacsMirror [4]. (But you can also specify a recipe manually by
;; putting `:recipe' in the `use-package' call, which is an extension
;; to `use-package' provided by straight.el.) Learn more about recipe
;; formatting from the MELPA README [5].
;;
;; [1]: https://github.com/raxod502/straight.el
;; [2]: http://melpa.org/#/
;; [3]: https://elpa.gnu.org/
;; [4]: https://emacsmirror.net/
;; [5]: https://github.com/melpa/melpa#recipe-format
(setq use-package-always-ensure t)

;; Tell use-package to always load packages lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See [1].
;;
;; [1]: https://github.com/jwiegley/use-package#notes-about-lazy-loading
(setq use-package-always-defer t)

(provide 'radian-package)

;;; radian-package.el ends here
