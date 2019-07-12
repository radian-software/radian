;; This file is loaded before package.el is initialized, and before
;; the first graphical frame is initialized, by Emacs 27 (but not by
;; any previous version of Emacs). Trivia: I was the person to
;; implement support for early-init.el, after a protracted argument
;; between me and most of the rest of the Emacs mailing list.

;; We don't use package.el. This needs to happen here because
;; package.el gets initialized before loading the regular init-file.
(setq package-enable-at-startup nil)
