;;; radian-cc.el --- Support for C-like languages

(require 'radian-autocomplete)
(require 'radian-eldoc)
(require 'radian-package)

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
(with-eval-after-load 'cc-mode
  (if (assoc 'other c-default-style)
      (setcdr (assoc 'other c-default-style)
              "k&r")
    (push '(other . "k&r") c-default-style))
  (setq-default c-basic-offset 2))

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

  ;; Automatically install irony-server if it is missing. irony-server
  ;; is necessary for Irony to work at all!

  (defun radian--install-irony-server-if-missing ()
    (unless (irony--locate-server-executable)
      (if (yes-or-no-p "Install irony-server? ")
          ;; The following `let' is copied from the definition of
          ;; `irony-install-server'. A better solution would be to
          ;; dynamically bind `irony--install-server-read-command' to
          ;; `identity' and then just use `call-interactively' to invoke
          ;; `irony-install-server' with no arguments, but I don't know how
          ;; to do this.
          (let ((command
                 (format
                  (concat "%s %s %s && %s --build . "
                          "--use-stderr --config Release --target install")
                  (shell-quote-argument irony-cmake-executable)
                  (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
                                                (expand-file-name
                                                 irony-server-install-prefix)))
                  (shell-quote-argument irony-server-source-dir)
                  (shell-quote-argument irony-cmake-executable))))
            (irony-install-server command))
        (message "irony-mode will not work until you M-x irony-install-server"))))

  (add-hook 'irony-mode-hook #'radian--install-irony-server-if-missing)

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

;; Eclipse for Emacs.
(use-package eclim
  :defer-install t
  :commands (eclim-mode)
  :init

  ;; Enable eclim for Java. It does other stuff too, but let's be
  ;; honest -- it's really only good for Java, at least for now.
  (add-hook 'java-mode-hook #'eclim-mode)

  ;; Boilerplate for `el-patch'.

  (defun radian--enable-eclim-patches ()
    (and (use-package-install-deferred-package 'eclim :el-patch)
         (require 'eclim)))

  (add-hook 'el-patch-pre-validate-hook #'radian--enable-eclim-patches)

  :config

  ;; If we didn't find `eclim-executable'...
  (unless eclim-executable
    ;; Add the 'brew cask install eclipse-java' installation directory
    ;; to the list of paths that are searched for the eclim
    ;; executable.
    (add-to-list 'eclim-eclipse-dirs
                 "/Applications/Eclipse Java.app/Contents/Eclipse/")

    ;; Then search again.
    (custom-reevaluate-setting 'eclim-executable))

  ;; Regrettably, eclim does not properly handle spaces in paths.
  ;; Let's fix it for them.
  (el-patch-defun eclim--make-command (args)
    "Create a command string that can be executed from the shell.
The first element in ARGS is the name of the eclim
operation.  The rest are flags/values to be passed on to
eclimd."
    (when (not eclim-executable)
      (error "Eclim installation not found. Please set eclim-executable."))
    (cl-reduce (lambda (a b) (format "%s %s" a b))
               (append (list (el-patch-wrap 1
                               (shell-quote-argument eclim-executable))
                             "-command" ((el-patch-swap first cl-first) args))
                       (cl-loop for a = (cdr args) then (cdr (cdr a))
                                for arg = ((el-patch-swap first cl-first) a)
                                for val = ((el-patch-swap second cl-second) a)
                                while arg append (if val (list arg (shell-quote-argument val)) (list arg))))))

  ;; Start the eclim server automatically when necessary.
  (setq eclimd-autostart t))

(provide 'radian-cc)

;;; radian-cc.el ends here
