;;; radian-langs.el --- Support for miscellaneous languages

(require 'radian-autocomplete)
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

;; Markdown, see https://daringfireball.net/projects/markdown/syntax
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
  (setq web-mode-markup-indent-offset 2))

(provide 'radian-langs)

;;; radian-langs.el ends here
