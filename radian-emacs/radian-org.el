;;; radian-org.el -- Org mode customizations

(require 'radian-bind-key)

;; Add a global keybinding for accessing the Org Agenda.
(bind-key "C-c a" #'org-agenda)

;; Make the Org Agenda split the window horizontally (with two tall
;; windows) rather than vertically (with two wide windows) by default.

(defun radian--advice-org-agenda-split-horizontally (org-agenda &rest args)
  "Make `org-agenda' split horizontally, not vertically, by default.
This is an `:around' advice for `org-agenda'."
  (let ((split-height-threshold nil))
    (apply org-agenda args)))

(advice-add #'org-agenda :around
            #'radian--advice-org-agenda-split-horizontally)

;; If the `org-directory' exists, set `default-directory' to it when
;; opening the Org agenda. This makes the behavior of things like
;; `find-file' more reasonable.

(defun radian--advice-org-agenda-default-directory
    (org-agenda &rest args)
  (let ((default-directory (if (file-exists-p org-directory)
                               org-directory
                             default-directory)))
    (apply org-agenda args)))

(advice-add #'org-agenda :around
            #'radian--advice-org-agenda-default-directory)

(with-eval-after-load 'org
  ;; Prevent Org from overriding the bindings for windmove.
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)

  ;; Add replacements for the some of keybindings we just removed.
  ;; It looks like Org already binds C-up and C-down separately from
  ;; M-{ and M-}, so we can't use those. Users will just have to
  ;; make do with C-c <up> and C-c <down> for now.
  (bind-keys :map org-mode-map
    ("C-<left>" . org-shiftleft)
    ("C-<right>" . org-shiftright)))

(with-eval-after-load 'org-agenda
  ;; Prevent Org Agenda from overriding the bindings for windmove.
  (unbind-key "S-<up>" org-agenda-mode-map)
  (unbind-key "S-<down>" org-agenda-mode-map)
  (unbind-key "S-<left>" org-agenda-mode-map)
  (unbind-key "S-<right>" org-agenda-mode-map)

  ;; Same routine as above. Now for Org Agenda, we could use C-up
  ;; and C-down because M-{ and M-} are bound to the same commands.
  ;; But I think it's best to take the same approach as before, for
  ;; consistency.
  (bind-keys :map org-agenda-mode-map
    ("C-<left>" . org-agenda-do-date-earlier)
    ("C-<right>" . org-agenda-do-date-later)))

(provide 'radian-org)

;;; radian-org.el ends here
