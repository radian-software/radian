;;; radian-browse.el --- Browsing the web

(require 'radian-package)

;; Browse Stack Overflow from Emacs!
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

(provide 'radian-browse)

;;; radian-browse.el ends here
