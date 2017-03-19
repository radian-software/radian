;;; radian-tex.el --- Support for TeX

(require 'radian-autocomplete)
(require 'radian-os)
(require 'radian-package)

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

(provide 'radian-tex)

;;; radian-tex.el ends here
