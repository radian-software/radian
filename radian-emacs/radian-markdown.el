;;; radian-markdown.el --- Support for Markdown

(require 'radian-package)

;; Provides syntax highlighting, indentation, and editing commands for
;; Markdown files.
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

(provide 'radian-markdown)

;;; radian-markdown.el ends here
