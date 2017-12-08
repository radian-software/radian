;;; radian-cc.el --- Support for C-like languages

(require 'map)

(require 'radian-autocomplete)
(require 'radian-check)
(require 'radian-eldoc)
(require 'radian-package)
(require 'radian-util)

(defvar radian-company-backend-cc nil
  "Grouped `company' backend for use in C/C++/Objective-C.")

;; Package `cc-mode' provides indentation and syntax highlighting for
;; C, C++, Objective-C, Java, and other similar languages.
(use-package cc-mode
  :ensure nil
  :config

  ;; Get rid of the submode indicators in the mode line. This
  ;; transforms e.g. "C++/l" into "C++". Since we are overriding a
  ;; function provided by `cc-mode', which is not initially loaded, we
  ;; have to make sure to do so *after* it is loaded and not before.

  (defalias 'radian-advice-inhibit-c-submode-indicators #'ignore
    "Unconditionally inhibit CC submode indicators in the mode line.
This is an `:override' advice for `c-update-modeline'.")

  (advice-add #'c-update-modeline :override
              #'radian-advice-inhibit-c-submode-indicators)

  ;; Switch to a better indentation-and-braces style. This turns the
  ;; following code:
  ;;
  ;; if (condition)
  ;;   {
  ;;     statement;
  ;;   }
  ;;
  ;; Into this:
  ;;
  ;; if (condition)
  ;; {
  ;;   statement;
  ;; }
  ;;
  ;; We do this by defining a custom style that is based on BSD, and
  ;; then overriding the indentation (which is set to 8 spaces by
  ;; default). This style is only used in C, C++, etc. and not Java.
  (c-add-style "radian-bsd"
               '("bsd"
                 (c-basic-offset . 2)))
  (map-put c-default-style 'other "radian-bsd")

  ;; Allow setting the CC style in a file-local or directory-local
  ;; variable.
  (put 'c-default-style 'safe-local-variable #'stringp))

;; Package `irony-mode' provides a framework to use libclang to get
;; semantic information about C, C++, and Objective-C code. Frontends
;; are provided by other packages.
(use-package irony
  :defer-install t
  :commands (irony-mode)
  :init

  ;; Enable `irony-mode' for C, C++, and Objective-C files.
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)

  :config

  ;; This tells `irony-mode' to discover compile options in a
  ;; .clang_complete file or another similar format automatically. See
  ;; [1] for further discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/irony-mode#configuration
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  ;; Set `company-backends' appropriately for `irony-mode'.
  (with-eval-after-load 'company
    (defun radian-company-setup-irony ()
      "Set `company-backends' locally for `irony-mode'."
      (setq-local company-backends
                  (cons radian-company-backend-cc
                        radian-company-backends-global)))

    (add-hook 'irony-mode-hook #'radian-company-setup-irony))

  :diminish irony-mode)

;; Package `company-irony' provides a `company' backend that uses
;; `irony-mode' to complete symbols in C, C++, and Objective-C.
(use-package company-irony
  :defer-install t
  :commands (company-irony)
  :after irony
  :config

  ;; Register the `company-irony' backend in
  ;; `radian-company-backend-cc', which will cause it to be used in
  ;; C/C++/Objective-C modes.
  (unless (memq 'company-irony radian-company-backend-cc)

    ;; Note that `company-irony' must be registered after
    ;; `company-irony-c-headers', if both are present. See [1].
    ;;
    ;; [1]: https://github.com/hotpxl/company-irony-c-headers
    (radian-insert-after*
     'company-irony 'company-irony-c-headers
     radian-company-backend-cc)))

;; Package `company-irony-c-headers' provides a `company' backend that
;; uses `irony-mode' to complete header file #includes in C, C++, and
;; Objective-C.
(use-package company-irony-c-headers
  :defer-install t
  :commands (company-irony-c-headers)
  :after irony
  :init

  ;; Register the `company-irony-c-headers' backend in
  ;; `radian-company-backend-cc', which will cause it to be used in
  ;; C/C++/Objective-C modes.
  (unless (memq 'company-irony-c-headers radian-company-backend-cc)

    ;; Note that `company-irony-c-headers' must be registered before
    ;; `company-irony', if both are present. See [1].
    ;;
    ;; [1]: https://github.com/hotpxl/company-irony-c-headers
    (radian-insert-before*
     'company-irony-c-headers 'company-irony
     radian-company-backend-cc)))

;; Package `irony-eldoc' provides an `eldoc' backend that uses
;; `irony-mode' to display function signatures in C, C++, and
;; Objective-C.
(use-package irony-eldoc
  :defer-install t
  :commands (irony-eldoc)
  :after irony
  :init

  ;; Enable `irony-eldoc' when `irony-mode' is active. See
  ;; `irony-eldoc' function documentation.
  (add-hook 'irony-mode-hook #'eldoc-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; Package `flycheck-irony' provides a `flycheck' checker that uses
;; `irony-mode' to display compilation errors and warnings in C, C++,
;; and Objective-C.
(use-package flycheck-irony
  :defer-install t
  :commands (flycheck-irony-setup)
  :after irony
  :config

  ;; When `irony-mode' is enabled, also set up `flycheck-irony.' This
  ;; will cause `flycheck' to be loaded, if it wasn't already.
  (add-hook 'irony-mode-hook #'flycheck-irony-setup)

  ;; Remove the default Flycheck checkers for C/C++, because they are
  ;; not as accurate and produce spurious errors.

  (defun radian-flycheck-disable-clang ()
    "Disable C/C++ checkers that are not sophisticated enough."
    (setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))

  (add-hook 'c-mode-hook #'radian-flycheck-disable-clang)
  (add-hook 'c++-mode-hook #'radian-flycheck-disable-clang)

  ;; By default, `flycheck-irony' disables the `flycheck' cppcheck
  ;; checker. This re-enables it. See [1] for discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/flycheck-irony/issues/9
  (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

(provide 'radian-cc)

;;; radian-cc.el ends here
