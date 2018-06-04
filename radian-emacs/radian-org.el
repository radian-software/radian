;;; radian-org.el -- Org mode customizations

;; Org is a hugely expansive framework (a.k.a. collection of hacks)
;; for organizing information, notes, tasks, calendars, and anything
;; else related to Org-anization.

(require 'radian-bind-key)
(require 'radian-git)
(require 'radian-patch)
(require 'radian-util)

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

  ;; But add a new keybinding for recovering the old behavior (see
  ;; `:bind' above).
  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))

  ;; Indent text according to outline structure.
  (add-hook 'org-mode-hook #'org-indent-mode)

  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil)

  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)

  (defun radian-org-sort-buffer ()
    "Sort all entries in the Org buffer recursively in alphabetical order."
    (interactive)
    (org-map-entries (lambda ()
                       (condition-case x
                           (org-sort-entries nil ?a)
                         ;; Ignore any errors signalled by Org.
                         (user-error)))))

  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian-list-of-strings-p)

  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t))

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
              #'radian--advice-org-agenda-default-directory)

  ;; Hide blocked tasks in the agenda view.
  (setq org-agenda-dim-blocked-tasks 'invisible))

(use-feature org-clock
  ;; We have to autoload these functions in order for the below code
  ;; that enables clock persistence without slowing down startup to
  ;; work.
  :commands (org-clock-load org-clock-save)
  :init

  ;; Allow clock data to be saved persistently.
  (setq org-clock-persist t)

  ;; Actually enable clock persistence. This is taken from
  ;; `org-clock-persistence-insinuate', but we can't use that function
  ;; since it causes both `org' and `org-clock' to be loaded for no
  ;; good reason.
  (add-hook 'org-mode-hook 'org-clock-load)
  (add-hook 'kill-emacs-hook 'org-clock-save)

  :config

  ;; Silence the messages that are usually printed when the clock data
  ;; is loaded from disk.
  (el-patch-defun org-clock-load ()
    "Load clock-related data from disk, maybe resuming a stored clock."
    (when (and org-clock-persist (not org-clock-loaded))
      (if (not (file-readable-p org-clock-persist-file))
	  (el-patch-swap
            (message "Not restoring clock data; %S not found" org-clock-persist-file)
            nil)
        (el-patch-remove
          (message "Restoring clock data"))
        ;; Load history.
        (el-patch-wrap 1
          (radian-with-silent-load
           (load-file org-clock-persist-file)))
        (setq org-clock-loaded t)
        (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
		       org-clock-stored-history)
	  (org-clock-history-push position (find-file-noselect file)))
        ;; Resume clock.
        (pcase org-clock-stored-resume-clock
	  (`(,(and file (pred file-exists-p)) . ,position)
	   (with-current-buffer (find-file-noselect file)
	     (when (or (not org-clock-persist-query-resume)
		       (y-or-n-p (format "Resume clock (%s) "
				         (save-excursion
					   (goto-char position)
					   (org-get-heading t t)))))
	       (goto-char position)
	       (let ((org-clock-in-resume 'auto-restart)
		     (org-clock-auto-clock-resolution nil))
	         (org-clock-in)
	         (when (org-invisible-p) (org-show-context))))))
	  (_ nil)))))

  ;; Don't record a clock entry if you clocked out in less than one
  ;; minute.
  (setq org-clock-out-remove-zero-time-clocks t))

(provide 'radian-org)

;;; radian-org.el ends here
