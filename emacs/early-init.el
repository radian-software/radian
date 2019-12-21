;; -*- lexical-binding: t -*-

;; This file is loaded before package.el is initialized, and before
;; the first graphical frame is initialized, by Emacs 27 (but not by
;; any previous version of Emacs). Trivia: I was the person to
;; implement support for early-init.el, after a protracted argument
;; between me and most of the rest of the Emacs mailing list.
;;
;; If the early init-file is available, we actually execute our entire
;; init process within it, by just loading the regular init-file.
;; (That file takes care of making sure it is only loaded once.)

;; Load an alternate ~/.emacs.d during regular init.
(unless (getenv "USER_EMACS_DIRECTORY")

  (defun radian--advice-fix-display-graphic-p (func &optional display)
    "Fix `display-graphic-p' so it works while loading the early init-file."
    (if display
        (funcall func display)
      ;; `display-graphic-p' lies by returning nil, but
      ;; `initial-window-system' tells the truth (it is nil only if we
      ;; are actually in a tty environment).
      initial-window-system))

  (advice-add #'display-graphic-p :around
              #'radian--advice-fix-display-graphic-p)

  (defun radian--advice-fix-xw-display-color-p (func &optional display)
    "Fix `xw-display-color-p' so it works while loading the early init-file."
    (if (or display after-init-time)
        (funcall func display)
      ;; Make an educated guess.
      initial-window-system))

  (advice-add #'xw-display-color-p :around
              #'radian--advice-fix-xw-display-color-p)

  (defun radian--advice-disable-x-resource-application ()
    "Disable `x-apply-session-resources'.
Now, `x-apply-session-resources' normally gets called before
reading the init-file. However if we do our initialization in the
early init-file, before that function gets called, then it may
override some important things like the cursor color. So we just
disable it, since there's no real reason to respect X
resources.")

  (advice-add #'x-apply-session-resources :override
              #'radian--advice-disable-x-resource-application)

  ;; Load the regular init-file.
  (load
   (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix)

  ;; Avoid messing with things more than necessary.
  (advice-remove #'display-graphic-p #'radian--advice-fix-display-graphic-p)
  (advice-remove #'xw-display-color-p #'radian--advice-fix-xw-display-color-p))
