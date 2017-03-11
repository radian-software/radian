;;; radian-org.el -- Org mode customizations

(require 'radian-bind-key)

;; Add a global keybinding for accessing the Org Agenda.
(bind-key "C-c a" #'org-agenda)

(with-eval-after-load 'org
  ;; Prevent Org from overriding the bindings for windmove.
  (unbind-key "S-<left>" #'org-mode-map)
  (unbind-key "S-<right>" #'org-mode-map)
  (unbind-key "S-<up>" #'org-mode-map)
  (unbind-key "S-<down>" #'org-mode-map)

  ;; Add replacements for the some of keybindings we just removed.
  ;; It looks like Org already binds C-up and C-down separately from
  ;; M-{ and M-}, so we can't use those. Users will just have to
  ;; make do with C-c <up> and C-c <down> for now.
  (bind-keys :map org-mode-map
    ("C-<left>" . org-shiftleft)
    ("C-<right>" . org-shiftright)))

(with-eval-after-load 'org-agenda
  ;; Prevent Org Agenda from overriding the bindings for windmove.
  (unbind-key "S-<up>" #'org-agenda-mode-map)
  (unbind-key "S-<down>" #'org-agenda-mode-map)
  (unbind-key "S-<left>" #'org-agenda-mode-map)
  (unbind-key "S-<right>" #'org-agenda-mode-map)

  ;; Same routine as above. Now for Org Agenda, we could use C-up
  ;; and C-down because M-{ and M-} are bound to the same commands.
  ;; But I think it's best to take the same approach as before, for
  ;; consistency.
  (bind-keys :map org-agenda-mode-map
    ("C-<left>" . org-agenda-do-date-earlier)
    ("C-<right>" . org-agenda-do-date-later)))

(provide 'radian-org)

;;; radian-org.el ends here
