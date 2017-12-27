;;; radian-pairs.el --- Paired delimiter handling

(require 'radian-package)
(require 'radian-windowed)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; `smartparens'.
(setq blink-matching-paren nil)

;; Insert and manipulate paired delimiters.
(use-package smartparens
  :init

  ;; Load the default configuration, including `with-eval-after-load'
  ;; forms for more specific configurations in other modes.
  (require 'smartparens-config)

  :config

  ;; Enable the functionality of Smartparens everywhere.
  (smartparens-global-mode +1)

  ;; Smartparens' Paredit emulation is missing some bindings, so we
  ;; re-add them here.
  (radian-alist-set* "M-?" #'sp-convolute-sexp sp-paredit-bindings)
  (radian-alist-set* "M-j" #'sp-join-sexp sp-paredit-bindings)

  (defun radian-sp-wrap-round ()
    "Wrap following sexp in round parentheses."
    (interactive)
    (sp-wrap-with-pair "("))

  (defun radian-sp-wrap-square ()
    "Wrap following sexp in square brackets."
    (interactive)
    (sp-wrap-with-pair "["))

  (defun radian-sp-wrap-curly ()
    "Wrap following sexp in curly braces."
    (interactive)
    (sp-wrap-with-pair "{"))

  ;; M-( is uncontroversial, and bound by Paredit. In windowed mode,
  ;; we can also bind M-[ (which Paredit doesn't do), but not in the
  ;; terminal since that messed up the escape sequences sent for the
  ;; arrows and other keys. M-{ is a no-go because it's bound to
  ;; `backward-paragraph'.
  (radian-alist-set* "M-(" #'radian-sp-wrap-round sp-paredit-bindings)
  (radian-with-windowed-emacs
    (radian-alist-set* "M-[" #'radian-sp-wrap-square sp-paredit-bindings))

  ;; Enable some default keybindings for Smartparens.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent paired delimiters from ever becoming unpaired, in Lisp
  ;; modes.
  (dolist (mode sp-lisp-modes)
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook #'smartparens-strict-mode)))

  ;; Prevent all highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.

  (with-eval-after-load 'org
    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (with-eval-after-load 'org-agenda
    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; When pressing RET after a newly entered curly-brace pair, add an
  ;; extra newline and indent. See [1].
  ;;
  ;; [1]: https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312

  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (dolist (mode '(c-mode c++-mode objc-mode java-mode python-mode))
    (sp-local-pair mode "{" nil :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>")))))

(provide 'radian-pairs)

;;; radian-pairs.el ends here
