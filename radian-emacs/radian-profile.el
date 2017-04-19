;;; radian-profile.el --- Profile Emacs startup

(require 'radian-package)

;; This is the most recent and full-featured Emacs startup profiler.
;; My fork has a small patch that works around a bug in the current
;; version.
(use-package esup
  :recipe (:fetcher github
           :repo "raxod502/esup")
  :defer-install t
  :commands (esup))

;; There's also profile-dotemacs, which intends to accomplish a
;; similar task. It's much older, and completely unmaintained (as far
;; as I know). But it can be useful sometimes. My fork (mirror,
;; actually) has a bunch of bugfixes.
(use-package profile-dotemacs
  :recipe (:fetcher github
           :repo "raxod502/profile-dotemacs")
  :defer-install t
  :commands (profile-dotemacs))

(provide 'radian-profile)

;;; radian-profile.el ends here
