;;; radian-org.el -- Org mode customizations

;; Org is a hugely expansive framework (a.k.a. collection of hacks)
;; for organizing information, notes, tasks, calendars, and anything
;; else related to Org-anization.

(require 'radian-bind-key)
(require 'radian-git)

(define-globalized-minor-mode global-outline-minor-mode
  outline-minor-mode outline-minor-mode)

(global-outline-minor-mode +1)
(diminish 'outline-minor-mode)

(use-package org
  :bind (;; Add the global keybindings for accessing Org Agenda and
         ;; Org Capture that are recommended in the Org manual.
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)

         :map org-mode-map

         ;; Prevent Org from overriding the bindings for windmove. By
         ;; default, these keys are mapped to `org-shiftleft', etc.
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil)

         ;; Add replacements for the keybindings we just removed.
         ;; C-<left> and C-<right> are unused by Org. C-<up> and
         ;; C-<down> are bound to `org-backward-paragraph', etc. (but
         ;; see below).
         ("C-<left>" . org-shiftleft)
         ("C-<right>" . org-shiftright)
         ("C-<up>" . org-shiftup)
         ("C-<down>" . org-shiftdown)

         ;; By default, Org maps C-<up> to `org-backward-paragraph'
         ;; instead of `backward-paragraph' (and analogously for
         ;; C-<down>). However, it doesn't do the same remapping for
         ;; the other bindings of `backward-paragraph' (e.g. M-{).
         ;; Here we establish that remapping. (This is important since
         ;; we remap C-<up> and C-<down> to other things, above. So
         ;; otherwise there would be no easy way to invoke
         ;; `org-backward-paragraph' and `org-forward-paragraph'.)
         ([remap backward-paragraph] . org-backward-paragraph)
         ([remap forward-paragraph] . org-forward-paragraph))
  :init

  ;; The following is a temporary hack until straight.el supports
  ;; building Org, see:
  ;;
  ;; * https://github.com/raxod502/straight.el/issues/211
  ;; * https://github.com/raxod502/radian/issues/410
  ;;
  ;; There are three things missing from our version of Org: the
  ;; functions `org-git-version' and `org-release', and the feature
  ;; `org-version'. We provide all three of those ourself, therefore.

  (defun org-git-version ()
    "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version)

  :bind (:map org-mode-map
              ("C-M-RET" . radian-org-insert-heading-at-point)
              ("C-M-<return>" . radian-org-insert-heading-at-point))
  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)

  ;; But add a new keybinding for recovering the old behavior.

  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil)

  ;; Indent subsections.
  (add-hook 'org-mode-hook #'org-indent-mode)

  ;; Utility functions.

  (defun radian-org-sort-buffer ()
    "Sort all entries in the Org buffer recursively in alphabetical order."
    (interactive)
    (org-map-entries (lambda ()
                       (condition-case x
                           (org-sort-entries nil ?a)
                         ;; Ignore any errors signalled by Org.
                         (user-error)))))

  (defun radian-org-archive-past ()
    "Archive DONE items with deadlines either missing or in the past."
    (interactive)
    (org-map-entries
     (lambda ()
       (when (and (string= (org-get-todo-state) "DONE")
                  (let ((deadline (org-entry-get (point) "DEADLINE")))
                    (or (null deadline)
                        (time-less-p (org-time-string-to-time deadline)
                                     (current-time)))))
         (org-archive-subtree)
         (setq org-map-continue-from (line-beginning-position)))))))

;; Org Agenda is for generating a more useful consolidated summary of
;; all or some of your tasks, according to their metadata.
(use-feature org-agenda
  :bind (:map org-agenda-mode-map

              ;; Prevent Org Agenda from overriding the bindings for
              ;; windmove.
              ("S-<up>" . nil)
              ("S-<down>" . nil)
              ("S-<left>" . nil)
              ("S-<right>" . nil)

              ;; Same routine as above. Now for Org Agenda, we could use
              ;; C-up and C-down because M-{ and M-} are bound to the same
              ;; commands. But I think it's best to take the same approach
              ;; as before, for consistency.
              ("C-<left>" . org-agenda-do-date-earlier)
              ("C-<right>" . org-agenda-do-date-later))
  :config

  ;; Make the Org Agenda split the window horizontally (with two tall
  ;; windows) rather than vertically (with two wide windows) by
  ;; default.

  (defun radian--advice-org-agenda-split-horizontally (org-agenda &rest args)
    "Make `org-agenda' split horizontally, not vertically, by default.
This is an `:around' advice for `org-agenda'. It commutes with
`radian--advice-org-agenda-default-directory'."
    (let ((split-height-threshold nil))
      (apply org-agenda args)))

  (advice-add #'org-agenda :around
              #'radian--advice-org-agenda-split-horizontally)

  ;; If the `org-directory' exists, set `default-directory' to it when
  ;; opening the Org agenda. This makes the behavior of things like
  ;; `find-file' more reasonable.

  (defun radian--advice-org-agenda-default-directory
      (org-agenda &rest args)
    "If `org-directory' exists, set `default-directory' to it in the agenda.
This is an `:around' advice for `org-agenda'. It commutes with
`radian--advice-org-agenda-split-horizontally'."
    (let ((default-directory (if (file-exists-p org-directory)
                                 org-directory
                               default-directory)))
      (apply org-agenda args)))

  (advice-add #'org-agenda :around
              #'radian--advice-org-agenda-default-directory))

(provide 'radian-org)

;;; radian-org.el ends here
