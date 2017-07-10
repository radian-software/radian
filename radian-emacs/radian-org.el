;;; radian-org.el -- Org mode customizations

;; Org is a hugely expansive framework (a.k.a. collection of hacks)
;; for organizing information, notes, tasks, calendars, and anything
;; else related to Org-anization.

(require 'radian-bind-key)
(require 'radian-package)

(use-package org
  :recipe (:host github
           :repo "emacsmirror/org"
           :files ("lisp/*.el"))
  :defer-install t
  :commands (org-version)
  :bind (;; Add a global keybinding for accessing the Org Agenda.
         ("C-c a" . org-agenda)

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

  ;; This section is devoted to fixing the asinine version-check
  ;; handling in Org (it's not designed to handle the case where you
  ;; run straight from the Git repo, apparently). This is one of the
  ;; worse hacks I've ever had the misfortune to create in Emacs.

  ;; First we define a function to return a proper version string
  ;; based on the Git repo. (This is somewhat similar to what happens
  ;; in org-fixup.el.) We should really define a function that will
  ;; return the latest tag, as well, but this remains a FIXME for now.
  (defun radian--org-git-version ()
    "Return the abbreviated SHA for the Org Git repo."
    (let ((default-directory (concat user-emacs-directory
                                     "straight/repos/org/")))
      (if (executable-find "git")
          (with-temp-buffer
            ;; Returns the shortest prefix of the SHA for HEAD that is
            ;; unique, down to a minimum of 4 characters (see
            ;; git-rev-parse(1)).
            (call-process "git" nil '(t nil) nil
                          "rev-parse" "--short" "HEAD")
            (if (> (buffer-size) 0)
                (string-trim (buffer-string))
              ;; This shouldn't happen, unless somehow Org is not
              ;; actually a Git repo.
              "revision unknown"))
        ;; This also shouldn't happen, because how would you have
        ;; gotten Org in the first place, then? But the real world
        ;; sucks and we have to account for stuff like this.
        "git not available")))

  ;; Here we're defining `org-git-version' and `org-release' eagerly.
  ;; Pay close attention here, since we actually do this multiple
  ;; times. The control flow is really weird. The reason we define the
  ;; functions here is that Emacs includes its own copy of Org, and
  ;; these functions are autoloaded by Emacs. Now, normally the
  ;; built-in autoloads are overridden by the version of Org
  ;; downloaded from EmacsMirror, but since we're running straight
  ;; from the Git repo, `org-git-version' and `org-release' are not
  ;; generated and autoloaded. So in order to avoid the original
  ;; autoloads from being triggered under any circumstances, we have
  ;; to overwrite them here.
  (defalias #'org-git-version #'radian--org-git-version)
  (defun org-release () "N/A") ; FIXME: replace with a real function

  ;; Now, the culprit function is `org-check-version', which is
  ;; defined in org-compat.el and called from org.el. The problem with
  ;; this function is that if the version of Org in use is not a
  ;; release version (i.e. it's running straight from the repo, as we
  ;; are doing), then it prints a warning. We don't want this. The
  ;; natural thought is to override `org-check-version'.
  ;; Unfortunately, this is completely impossible since
  ;; `org-check-version' is a macro, and org.el (which is where the
  ;; macro is used) is byte-compiled, so the code of
  ;; `org-check-version' is hardcoded into org.elc. The easiest way
  ;; around the problem, other than doing something even more
  ;; horrifying like suppressing warnings while loading Org, seems to
  ;; be to *pretend* that org-version.el is available, even though it
  ;; doesn't exist. Then `org-check-version' happily defines
  ;; `org-git-version' and `org-release' as autoloads pointing to
  ;; org-version.el. Of course, then after Org is loaded, we have to
  ;; override those autoloads to make the functions point back to what
  ;; we want. Right now, the definition of `org-release' generated by
  ;; `org-check-version' is the same as the one used above, so we
  ;; don't bother to change it. That should change, FIXME.
  (provide 'org-version)
  (with-eval-after-load 'org
    (defalias #'org-git-version #'radian--org-git-version))

  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t))

;; Org Agenda is for generating a more useful consolidated summary of
;; all or some of your tasks, according to their metadata.
(use-package org-agenda
  :recipe org
  :defer-install t
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
