;;; radian-langs.el --- Support for miscellaneous languages

(require 'radian-autocomplete)
(require 'radian-bind-key)
(require 'radian-indent)
(require 'radian-os)
(require 'radian-package)
(require 'radian-patch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AppleScript

;;  https://developer.apple.com/library/content/documentation/AppleScript/Conceptual/AppleScriptLangGuide/introduction/ASLR_intro.html

(use-package apples-mode
  :defer-install t
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

;; https://golang.org/

(use-package go-mode
  :defer-install t
  :mode "\\.go\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HTML

;; https://www.w3.org/TR/html5/

;; This package provides for editing HTML and HTML-like
;; languages (e.g. templating engines like PHP, ASP, Handlebars,
;; etc.).
(use-package web-mode
  :defer-install t
  :commands (web-mode)
  :init

  ;; Enable `web-mode' when editing HTML documents.
  (add-hook 'html-mode-hook #'web-mode)

  :config

  ;; Indent by two spaces by default.
  (setq web-mode-markup-indent-offset 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JavaScript

(with-eval-after-load 'js
  ;; The standard JavaScript indent width is two spaces, not four.
  (setq js-indent-level 2))

;; Improved JavaScript support.
(use-package js2-mode
  :defer-install t
  :commands (js2-minor-mode)
  ;; This is slightly different than what is recommended in the README
  ;; [1]. It seems to make the most sense though. See also [2].
  ;;
  ;; [1]: http://elpa.gnu.org/packages/js2-mode.html
  ;; [2]: https://github.com/PythonNut/emacs-config/blob/41c132ed89d85b96c3cd5267cb86b6bb30ac45f3/modules/config-modes.el#L220-L222
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter ("node" . js2-mode)
  :config

  ;; Change the mode lighters. (They are originally Javascript-IDE and
  ;; JSX-IDE, which are wordy.)

  (defun radian--set-js2-mode-lighter ()
    "Change the `js2-mode' lighter from Javascript-IDE to JavaScript."
    (setq-local mode-name "JavaScript"))

  (defun radian--set-js2-jsx-mode-lighter ()
    "Change the `js2-jsx-mode' lighter from JSX-IDE to JSX."
    (setq-local mode-name "JSX"))

  (add-hook 'js2-mode-hook #'radian--set-js2-mode-lighter)
  (add-hook 'js2-jsx-mode-hook #'radian--set-js2-jsx-mode-lighter)

  ;; Treat shebang lines (e.g. for node) correctly.
  (setq js2-skip-preprocessor-directives t))

;; Live web development with Emacs.

(use-package skewer-mode
  :defer-install t
  :commands (list-skewer-clients
             run-skewer
             skewer-mode
             skewer-run-phantomjs)
  :init

  ;; Enable the features of Skewer. Since this package uses deferred
  ;; installation, we need to `el-patch'.

  (el-patch-defun skewer-setup ()
    "Fully integrate Skewer into js2-mode, css-mode, and html-mode buffers."
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode))

  (skewer-setup)

  (defun radian--enable-skewer-setup-patches ()
    "Enable `el-patch' patches for `skewer-setup'."
    (require 'skewer-setup))

  (add-hook 'el-patch-pre-validate-hook
            #'radian--enable-skewer-setup-patches)

  :diminish skewer-mode)

(use-package skewer-css
  :recipe skewer-mode
  :defer-install t
  :commands (skewer-css-mode)
  :diminish skewer-css-mode)

(use-package skewer-html
  :recipe skewer-mode
  :defer-install t
  :commands (skewer-html-mode)
  :diminish skewer-html-mode)

;; Contrary to the name of the package, this actually provides Company
;; support for JavaScript, using js2 parsing and Skewer.
(use-package ac-js2
  :defer-install t
  :after skewer-repl
  :config

  ;; Ostensibly this provides more intelligent completions, by
  ;; allowing `ac-js2' to evaluate your JavaScript code to generate
  ;; completions.
  (setq ac-js2-evaluate-calls t)

  ;; Enable the Company integration for `ac-js2', but only in the
  ;; Skewer REPL.

  (defun radian--enable-ac-js2-company ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'ac-js2-company))

  (add-hook 'skewer-repl-mode-hook #'radian--enable-ac-js2-company))

;; This package provides a separate auto-completion backend for
;; JavaScript that is more suitable for general code.
(use-package tern
  :defer-install t
  :after js2-mode
  :config

  ;; We want Tern to be active whenever we're editing a JavaScript
  ;; file. But not when we're in the Skewer REPL, since there we use
  ;; `ac-js2' instead.
  (add-hook 'js2-mode-hook #'tern-mode)

  :diminish tern-mode)

;; Company backend that uses Tern.
(use-package company-tern
  :defer-install t
  :after (:all tern company)
  :config

  ;; This allows Company to use suggestions from Tern.
  (add-to-list 'company-backends 'company-tern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown

;; https://daringfireball.net/projects/markdown/

(use-package markdown-mode
  :defer-install t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

;; Provides the `markdown-toc-generate-toc' command to generate a
;; table of contents for a Markdown file.
(use-package markdown-toc
  :defer-install t
  :commands (markdown-toc-generate-toc
             markdown-toc-version)
  :config

  ;; Remove the header inserted before the table of contents. If you
  ;; want a header, just add one before the "markdown-toc start"
  ;; comment -- this way, you can have different header styles in
  ;; different documents.
  (setq markdown-toc-header-toc-title ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python

;; https://www.python.org/

;; Integrated development environment for Python.
(use-package anaconda-mode
  :defer-install t
  :commands (anaconda-mode)
  :init

  ;; Enable the functionality of anaconda-mode in Python buffers, as
  ;; suggested in the README [1].
  ;;
  ;; [1]: https://github.com/proofit404/anaconda-mode
  (add-hook 'python-mode-hook #'anaconda-mode)

  :diminish anaconda-mode)

;; Company integration for anaconda-mode.
(use-package company-anaconda
  :defer-install t
  :commands (company-anaconda)
  :init

  (with-eval-after-load 'company
    ;; Enable the functionality of company-anaconda in Python buffers,
    ;; as suggested in the README [1].
    ;;
    ;; [1]: https://github.com/proofit404/company-anaconda

    (defun radian--enable-company-anaconda ()
      (add-to-list 'company-backends #'company-anaconda))

    (add-hook 'python-mode-hook #'radian--enable-company-anaconda)

    ;; Add a space between the completion candidates and the
    ;; chevron-enclosed description, so the completions menu doesn't
    ;; look so crowded.
    (setq company-anaconda-annotation-function
          (lambda (candidate)
            (concat
             " "
             (company-anaconda-description-in-chevrons
              candidate))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Ruby

;; https://www.ruby-lang.org/

(with-eval-after-load 'ruby-mode
  ;; Indent aggressively in Ruby.
  (add-hook 'ruby-mode-hook #'aggressive-indent-mode))

;; Autocompletion for Ruby.
(use-package robe
  :defer-install t
  :commands (robe-mode)
  :init

  ;; Enable Robe in Ruby files.
  (add-hook 'ruby-mode-hook #'robe-mode)

  :diminish robe-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rust

;; https://www.rust-lang.org/

(use-package rust-mode
  :defer-install t
  :mode "\\.rs\\'")

;; Autocompletion for Rust.
(use-package racer
  :defer-install t
  :commands (racer-mode)
  :init

  (add-hook 'rust-mode-hook #'racer-mode)

  :config

  (add-hook 'racer-mode-hook #'eldoc-mode)
  (with-eval-after-load 'company
    (setq-local company-tooltip-align-annotations t))

  (defun radian--reduce-racer-lag ()
    ;; increased from 0:
    (setq-local company-idle-delay 0.5)
    ;; increased from 0:
    (setq-local eldoc-idle-delay 0.5))

  (add-hook 'racer-mode-hook #'radian--reduce-racer-lag)

  :diminish racer-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Shell

;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

;; Inhibit the "Indentation setup for shell type *sh" message.

(defun radian--enable-sh-script-patches ()
  "Enable `el-patch' patches for `sh-script'."
  (require 'sh-script))

(add-hook 'el-patch-pre-validate-hook #'radian--enable-sh-script-patches)

(with-eval-after-load 'sh-script
  (el-patch-defun sh-set-shell (shell &optional no-query-flag insert-flag)
    "Set this buffer's shell to SHELL (a string).
When used interactively, insert the proper starting #!-line,
and make the visited file executable via `executable-set-magic',
perhaps querying depending on the value of `executable-query'.

When this function is called noninteractively, INSERT-FLAG (the third
argument) controls whether to insert a #!-line and think about making
the visited file executable, and NO-QUERY-FLAG (the second argument)
controls whether to query about making the visited file executable.

Calls the value of `sh-set-shell-hook' if set.

Shell script files can cause this function be called automatically
when the file is visited by having a `sh-shell' file-local variable
whose value is the shell name (don't quote it)."
    (interactive (list (completing-read
                        (format "Shell (default %s): "
                                sh-shell-file)
                        ;; This used to use interpreter-mode-alist, but that is
                        ;; no longer appropriate now that uses regexps.
                        ;; Maybe there could be a separate variable that lists
                        ;; the shells, used here and to construct i-mode-alist.
                        ;; But the following is probably good enough:
                        (append (mapcar (lambda (e) (symbol-name (car e)))
                                        sh-ancestor-alist)
                                '("csh" "rc" "sh"))
                        nil nil nil nil sh-shell-file)
                       (eq executable-query 'function)
                       t))
    (if (string-match "\\.exe\\'" shell)
        (setq shell (substring shell 0 (match-beginning 0))))
    (setq sh-shell (sh-canonicalize-shell shell))
    (if insert-flag
        (setq sh-shell-file
              (executable-set-magic shell (sh-feature sh-shell-arg)
                                    no-query-flag insert-flag)))
    (setq mode-line-process (format "[%s]" sh-shell))
    (setq-local sh-shell-variables nil)
    (setq-local sh-shell-variables-initialized nil)
    (setq-local imenu-generic-expression
                (sh-feature sh-imenu-generic-expression))
    (let ((tem (sh-feature sh-mode-syntax-table-input)))
      (when tem
        (setq-local sh-mode-syntax-table
                    (apply 'sh-mode-syntax-table tem))
        (set-syntax-table sh-mode-syntax-table)))
    (dolist (var (sh-feature sh-variables))
      (sh-remember-variable var))
    (if (setq-local sh-indent-supported-here
                    (sh-feature sh-indent-supported))
        (progn
          (el-patch-remove
            (message "Setting up indent for shell type %s" sh-shell))
          (let ((mksym (lambda (name)
                         (intern (format "sh-smie-%s-%s"
                                         sh-indent-supported-here name)))))
            (add-function :around (local 'smie--hanging-eolp-function)
              (lambda (orig)
                (if (looking-at "[ \t]*\\\\\n")
                    (goto-char (match-end 0))
                  (funcall orig))))
            (add-hook 'smie-indent-functions #'sh-smie--indent-continuation nil t)
            (smie-setup (symbol-value (funcall mksym "grammar"))
                        (funcall mksym "rules")
                        :forward-token  (funcall mksym "forward-token")
                        :backward-token (funcall mksym "backward-token")))
          (unless sh-use-smie
            (setq-local parse-sexp-lookup-properties t)
            (setq-local sh-kw-alist (sh-feature sh-kw))
            (let ((regexp (sh-feature sh-kws-for-done)))
              (if regexp
                  (setq-local sh-regexp-for-done
                              (sh-mkword-regexpr (regexp-opt regexp t)))))
            (el-patch-remove
              (message "setting up indent stuff"))
            ;; sh-mode has already made indent-line-function local
            ;; but do it in case this is called before that.
            (setq-local indent-line-function 'sh-indent-line))
          (if sh-make-vars-local
              (sh-make-vars-local))
          (el-patch-remove
            (message "Indentation setup for shell type %s" sh-shell)))
      (el-patch-remove
        (message "No indentation for this shell type."))
      (setq-local indent-line-function 'sh-basic-indent-line))
    (when font-lock-mode
      (setq font-lock-set-defaults nil)
      (font-lock-set-defaults)
      (font-lock-flush))
    (setq sh-shell-process nil)
    (run-hooks 'sh-set-shell-hook)))

;; Inhibit the "Indentation variables are now local" message.

(with-eval-after-load 'sh-script
  (el-patch-defun sh-make-vars-local ()
    "Make the indentation variables local to this buffer.
Normally they already are local.  This command is provided in case
variable `sh-make-vars-local' has been set to nil.

To revert all these variables to the global values, use
command `sh-reset-indent-vars-to-global-values'."
    (interactive)
    (mapc 'make-local-variable sh-var-list)
    (el-patch-remove
      (message "Indentation variables are now local."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Swift

;; https://developer.apple.com/swift/

(use-package swift-mode
  :defer-install t
  :mode "\\.swift\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TeX

;; https://www.tug.org/begin.html

;; AUCTeX, integrated development environment for LaTeX and friends.
;; Unfortunately because AUCTeX is weird, we need to do a little dance
;; with the `use-package' declarations, see [1].
;;
;; [1]: https://github.com/jwiegley/use-package/issues/379#issuecomment-258217014

(use-package tex-site
  :recipe (auctex :fetcher github
                  :repo "emacsmirror/auctex"
                  :files (:defaults (:exclude "*.el.in")))
  :demand t)

(use-package tex
  :recipe auctex
  :config

  ;; The following configuration is recommended in the manual [1].
  ;;
  ;; [1]: https://www.gnu.org/software/auctex/manual/auctex/Quick-Start.html
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  (radian-with-operating-system macOS
    ;; Use TeXShop for previewing LaTeX, rather than Preview. This
    ;; means we have to define the command to run TeXShop as a "viewer
    ;; program", and then tell AUCTeX to use the TeXShop viewer when
    ;; opening PDFs.

    (add-to-list 'TeX-view-program-list
                 '("TeXShop" "/usr/bin/open -a TeXShop.app %s.pdf"))
    (setf (alist-get 'output-pdf TeX-view-program-selection)
          '("TeXShop"))))

(use-package latex
  :recipe auctex
  :config

  ;; Don't be afraid to break inline math between lines.
  (setq LaTeX-fill-break-at-separators nil))

;; Company integration for AUCTeX.
(use-package company-auctex
  :defer-install t
  :after tex
  :config

  ;; Enable the functionality of `company-auctex'.
  (company-auctex-init))

(provide 'radian-langs)

;;; radian-langs.el ends here
