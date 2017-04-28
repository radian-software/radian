;;; radian-langs.el --- Support for miscellaneous languages

(require 'radian-autocomplete)
(require 'radian-bind-key)
(require 'radian-indent)
(require 'radian-os)
(require 'radian-package)

;; AppleScript, see
(use-package apples-mode
  :defer-install t
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;; Go, see https://golang.org/
(use-package go-mode
  :defer-install t
  :mode "\\.go\\'")

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

  ;; Bind C-M-j to `js-indent-line', not just M-j. This is temporary
  ;; until [1] is merged.
  ;;
  ;; [1]: https://github.com/mooz/js2-mode/pull/421
  (bind-key [remap indent-new-comment-line] #'js2-line-break js2-mode-map))

;; Live web development with Emacs.
(use-package skewer-mode
  :defer-install t
  :commands (list-skewer-clients skewer-mode run-skewer skewer-run-phantomjs)
  :init

  ;; Enable the features of Skewer. Since the author was kind enough
  ;; to put `skewer-setup' in a separate file, we don't have to worry
  ;; about lazy-loading!
  (skewer-setup)

  :diminish skewer-mode)

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

;; Markdown, see https://daringfireball.net/projects/markdown/
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

;; Ruby, see https://www.ruby-lang.org/
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

;; Rust, see https://www.rust-lang.org/
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

;; Swift, see https://developer.apple.com/swift/
(use-package swift-mode
  :defer-install t
  :mode "\\.swift\\'")

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

(provide 'radian-langs)

;;; radian-langs.el ends here
