;;; radian-profile.el --- Profile Emacs startup

(require 'radian-package)

;; This is the most recent and full-featured Emacs startup profiler.
(use-package esup
  ;; My fork inhibits the behavior wherein esup descends into
  ;; `require' forms, because this causes a terrible bug with
  ;; `cl-lib', see [1].
  ;;
  ;; [1]: https://github.com/jschaf/esup/issues/40
  :recipe (:host github
           :repo "raxod502/esup"
           :upstream (:host github
                      :repo "jschaf/esup"))
  :defer-install t
  :commands (esup))

;; There's also profile-dotemacs, which intends to accomplish a
;; similar task. It's much older, and completely unmaintained (as far
;; as I know). But it can be useful sometimes.
(use-package profile-dotemacs
  ;; Package is unmaintained. No upstream.
  :recipe (:host github
           :repo "raxod502/profile-dotemacs")
  :defer-install t
  :commands (profile-dotemacs))

(provide 'radian-profile)

;;; radian-profile.el ends here
