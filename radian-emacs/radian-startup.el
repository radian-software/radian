;;; radian-startup.el --- Cleaning up Emacs startup

;; Disable the "For information about GNU Emacs..." message at
;; startup, for *all* users. (Because FSF is crazy about copyright
;; shenanigans, Emacs suggests setting
;; `inhibit-startup-echo-area-message' to your username so that other
;; people using your config will still get to see the GNU spam. This
;; is nonsense, so we override it here.)

(defalias 'radian--advice-inhibit-startup-echo-area-message #'ignore
  "Unconditionally inhibit the startup message in the echo area.
This is an `:override' advice for
`display-startup-echo-area-message'.")

(advice-add #'display-startup-echo-area-message :override
            #'radian--advice-inhibit-startup-echo-area-message)

;; Disable the *About GNU Emacs* buffer at startup, and go straight
;; for the scratch buffer. This is especially useful because the
;; startup buffer is "special" and some features that are supposed to
;; be globally available do not work in it. (E.g., Projectile.)
(setq inhibit-startup-screen t)

;; Remove the initial *scratch* message.
(setq initial-scratch-message nil)

(provide 'radian-startup)

;;; radian-startup.el ends here
