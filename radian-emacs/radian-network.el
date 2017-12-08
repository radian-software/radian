;;; radian-browse.el --- Browsing the web

(require 'radian-os)
(require 'radian-package)

;; In recent versions of macOS, there is an annoying configuration
;; problem wherein TLS verification does not work out of the box,
;; since macOS does not provide certificates in a place that can be
;; read by `gnutls', or something silly like that. The user impact is
;; that anytime Emacs tries to make an HTTPS connection, the user is
;; presented with a warning saying that there was a verification
;; error. See [1] for further discussion of this issue, and for the
;; original source of the solution given below. In order to take
;; advantage of it, you must install the 'libressl' package from
;; Homebrew.
;;
;; [1]: https://emacs.stackexchange.com/a/18070/12534
(radian-with-operating-system macOS
  (with-eval-after-load 'gnutls
    (setq gnutls-verify-error t)
    (setq gnutls-min-prime-bits 3072)
    (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")))

;; Package `sx' allows you to browse Stack Overflow from within Emacs.
;; First, run `sx-authenticate' in order to provide your username and
;; password. After that, you can use any of the entry points listened
;; below. Navigation is keyboard-centric.
(use-package sx
  :defer-install t
  :commands (sx-ask
             sx-authenticate
             sx-bug-report
             sx-inbox
             sx-inbox-notifications
             sx-open-link
             sx-search
             sx-search-tag-at-point
             sx-tab-all-questions
             sx-tab-featured
             sx-tab-frontpage
             sx-tab-hot
             sx-tab-month
             sx-tab-newest
             sx-tab-starred
             sx-tab-topvoted
             sx-tab-unanswered
             sx-tab-unanswered-my-tags
             sx-tab-week))

;; Allow setting the regexp for bug references from file-local or
;; directory-local variables. CIDER does this in its files, for
;; example.
(put 'bug-reference-bug-regexp 'safe-local-variable #'stringp)

;; Package `ix' provides Emacs support for a fantastic command-line
;; pastebin tool.
(use-package ix
  :defer-install t
  :commands (ix ix-browse ix-delete))

(provide 'radian-network)

;;; radian-browse.el ends here
