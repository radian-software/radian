;;; radian-cc.el --- Support for C-like languages

(require 'radian-autocomplete)
(require 'radian-check)
(require 'radian-eldoc)
(require 'radian-package)
(require 'radian-util)

;; Get rid of the submode indicators in the mode line. This transforms
;; e.g. "C++/l" into "C++". Since we are overriding a function
;; provided by `cc-mode', which is not initially loaded, we have to
;; make sure to do so *after* it is loaded and not before.

(defalias 'radian--advice-inhibit-c-submode-indicators #'ignore
  "Unconditionally inhibit CC submode indicators in the mode line.
This is an `:override' advice for `c-update-modeline'.")

(with-eval-after-load 'cc-mode
  (advice-add #'c-update-modeline :override
              #'radian--advice-inhibit-c-submode-indicators))

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
;; if (condition) {
;;   statement;
;; }
;;
;; We do this by defining a custom style that is based on K&R, and
;; then overriding the indentation (which is set to 5 spaces by
;; default -- yes, really). This style is only used in C, C++, etc.
;; and not Java.
(with-eval-after-load 'cc-mode
  (c-add-style "radian-k&r"
               '("k&r"
                 (c-basic-offset . 2)))
  (radian-alist-set* 'other "radian-k&r" c-default-style 'symbol))

;; General support for C, C++, and Objective-C based on libclang.
(use-package irony
  :defer-install t
  :commands (irony-mode)
  :init

  ;; Enable Irony for C, C++, and Objective-C files.
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)

  :config

  ;; Taken from the README of irony-mode [1]. If it's not present,
  ;; company-irony seems to only be able to work in a single buffer.
  ;;
  ;; [1]: https://github.com/Sarcasm/irony-mode
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  :diminish irony-mode)

;; Company integration for Irony.
(use-package company-irony
  :defer-install t
  :commands (company-irony)
  :init

  ;; Tell Company about company-irony. For some reason, this appears
  ;; to cause Irony to be eagerly loaded. So we only do it after Irony
  ;; has been loaded.

  (defun radian--set-up-company-irony ()
    ;; Don't add `company-irony' as a backend if we have
    ;; already added `company-irony-c-headers'. The backend
    ;; for `company-irony-c-headers' is a grouped backend,
    ;; so it accounts for both, and if we add
    ;; `company-irony' it will take precedence and inhibit
    ;; the functionality of `company-irony-c-headers'.
    (unless (member '(company-irony-c-headers
                      company-irony)
                    company-backends)
      (add-to-list 'company-backends 'company-irony)))

  (add-hook 'irony-mode-hook #'radian--set-up-company-irony))

;; Extends company-irony to work for completing #includes.
(use-package company-irony-c-headers
  :defer-install t
  :commands (company-irony-c-headers)
  :init

  ;; Tell Company about company-irony-c-headers. As per the README
  ;; [1], we must add a grouped backend for things to work properly.
  ;;
  ;; [1]: https://github.com/hotpxl/company-irony-c-headers

  (defun radian--set-up-company-irony-c-headers ()
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony)))

  (add-hook 'irony-mode-hook #'radian--set-up-company-irony-c-headers))

;; ElDoc integration for Irony.
(use-package irony-eldoc
  :defer-install t
  :commands (irony-eldoc)
  :init

  ;; Enable irony-eldoc. See `irony-eldoc' function documentation.
  (add-hook 'irony-mode-hook #'eldoc-mode)
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; Flycheck integration for Irony.
(use-package flycheck-irony
  :defer-install t
  :commands (flycheck-irony-setup)
  :init

  ;; When Irony is enabled, also set up Flycheck-Irony. This will
  ;; cause Flycheck to be loaded, if it wasn't already.
  (add-hook 'irony-mode-hook #'flycheck-irony-setup)

  :config

  ;; Once Flycheck-Irony is loaded, remove the default Flycheck
  ;; checkers for C/C++, because they are not as accurate.

  (defun radian--disable-flycheck-using-clang ()
    "Disable C/C++ checkers that are not sophisticated enough."
    (setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)))

  (add-hook 'c-mode-hook #'radian--disable-flycheck-using-clang)
  (add-hook 'c++-mode-hook #'radian--disable-flycheck-using-clang)

  ;; Also enable cppcheck, even though Flycheck-Irony is already
  ;; enabled. See [1] for discussion.
  ;;
  ;; [1]: https://github.com/Sarcasm/flycheck-irony/issues/9
  (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck)))

(provide 'radian-cc)

;;; radian-cc.el ends here
