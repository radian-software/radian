;;; radian-theme.el --- Loading color themes

(require 'radian-custom)
(require 'radian-package)

;;; See also `radian-appearance'.

;; This function is useful for reformatting lispy names (like
;; `deeper-blue') into user-friendly strings (like "Deeper Blue") for
;; the Custom interface.
(defun radian--unlispify (name)
  "Converts \"deep-blue\" to \"Deep Blue\"."
  (capitalize
   (replace-regexp-in-string
    "-" " " name)))

;; The default color scheme is Wombat, for now. It's one of the few
;; reasonable-looking themes I can find that actually works in a
;; terminal.
(defcustom radian-color-theme 'wombat
  "Specifies the color theme used by Radian.

You can use anything listed by `custom-available-themes'. If you
wish to use your own color theme, you can set this to nil."
  :group 'radian
  :type `(choice ,@(mapcar (lambda (theme)
                             `(const :tag
                                ,(if theme
                                     (radian--unlispify
                                      (symbol-name theme))
                                   "None")
                                ,theme))
                           (cons
                            nil
                            (sort
                             (append
                              (custom-available-themes)
                              '(zerodark))
                             #'string-lessp)))))

;; Allow to defer color theme loading until after init, which helps to
;; avoid weirdness during the processing of the local init-file.
(defcustom radian-defer-color-theme t
  "Non-nil means defer loading the color theme until after init.
Otherwise, the color theme is loaded whenever `radian-theme' is
loaded."
  :group 'radian
  :type 'boolean)

;; This is a handy macro for conditionally enabling color theme
;; customizations.
(defmacro radian-with-color-theme (theme &rest body)
  "If the current color theme is THEME, eval BODY; else return nil.
The current color theme is determined by consulting
`radian-color-theme'."
  (declare (indent 1))
  ;; `theme' should be a symbol so we can use `eq'.
  `(when (eq ',theme radian-color-theme)
     ,@body))

;; Now we make some tweaks to various color themes to make them look
;; better.

(radian-with-color-theme leuven
  ;; Change the highlight color used by various things from yellow to
  ;; blue.
  (set-face-background 'highlight "#B1EAFC")

  ;; Get rid of the underline for the currently highlighted match in an
  ;; Isearch or query replace.
  (set-face-underline 'isearch nil)

  ;; The default highlight color for Isearches is quite dark and makes
  ;; it hard to read the highlighted text. Change it to a nice light
  ;; blue, and get rid of the distracting underline.
  (set-face-background 'lazy-highlight "#B1EAFC")
  (set-face-underline 'lazy-highlight nil)

  ;; Eliminate the underline on mismatched parens.
  (set-face-underline 'show-paren-mismatch nil))

(straight-register-package 'zerodark-theme)
(radian-with-color-theme zerodark
  (use-package zerodark-theme))

;; Load the appropriate color scheme as specified in
;; `radian-color-theme'.
(when radian-color-theme
  (if radian-defer-color-theme
      (progn
        (eval-and-compile
          (defun radian-load-color-theme ()
            "Load the Radian color theme, as given by `radian-color-theme'.
If there is an error, report it as a warning."
            (condition-case-unless-debug error-data
                (load-theme radian-color-theme 'no-confirm)
              (error (warn "Could not load color theme: %s"
                           (error-message-string error-data)))))
          (add-hook 'after-init-hook #'radian-load-color-theme)))
    (load-theme radian-color-theme 'no-confirm)))

(provide 'radian-theme)

;;; radian-theme.el ends here
