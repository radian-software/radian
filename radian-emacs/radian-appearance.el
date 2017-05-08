;;; radian-appearance.el --- Non-color-theme appearance tweaks

(require 'radian-bind-key)
(require 'radian-package)
(require 'radian-windowed)

;;; This file has appearance tweaks that are unrelated to the color
;;; theme. Menus, scroll bars, bells, cursors, and so on. See also
;;; `radian-theme'.

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the contextual menu that pops up when you right-click.
(bind-keys ("<C-mouse-1>" . ignore)
           ("<C-down-mouse-1>" . ignore))

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

;; When point is on a paren, highlight the matching paren, even if it
;; wasn't just typed. Also, do it immediately, instead of after 1/8 of
;; a second. Note that `show-paren-delay' must be changed *before*
;; turning on `show-paren-mode' in order for the change to take
;; effect.
(setq show-paren-delay 0)
(show-paren-mode 1)

(radian-with-windowed-emacs
  ;; Disable the scroll bars.
  (scroll-bar-mode -1)

  ;; Disable the tool bar.
  (tool-bar-mode -1)

  ;; Prevent the cursor from blinking.
  (blink-cursor-mode -1))

;; This package provides an easy way to change the display of minor
;; modes in the mode line.
(use-package diminish
  :demand t)

;;; The following code customizes the mode bar to something like:
;;; [*] init.el        96% (2410,30)  [radian:master*]  (Emacs-Lisp Paredit AggrIndent)

(defun radian--mode-line-buffer-modified ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize (if (and (buffer-modified-p)
                       (buffer-file-name))
                  "[*]" "   ")
              ;; Make sure to show it in the same color as the buffer
              ;; name.
              'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defvar-local radian--mode-line-project-and-branch nil
  "Mode line construct showing Projectile project and Git status.
The format is [project:branch*], where the * is shown if the
working directory is dirty. Either component can be missing; this
might happen if Projectile is not installed or if the project is
not version-controlled with Git. If nothing should be displayed,
this variable is set to nil.

This variable is actually only a cached value; it is set by
`radian--compute-mode-line-project-and-branch' for performance
reasons.")

;; Don't clear the cache when switching major modes (or using M-x
;; normal-mode).
(put 'radian--mode-line-project-and-branch 'permanent-local t)

(defun radian--compute-mode-line-project-and-branch ()
  (let ((old radian--mode-line-project-and-branch)
        (new
         (let* (;; Don't insist on having Projectile loaded.
                (project-name (when (featurep 'projectile)
                                (projectile-project-name)))
                ;; Projectile returns "-" to mean "no project". I'm
                ;; still wondering what happens if someone makes a
                ;; project named "-".
                (project-name (unless (equal project-name "-")
                                project-name))
                ;; Check if we are actually in a Git repo, and Git is
                ;; available.
                (git (and (executable-find "git")
                          (locate-dominating-file default-directory ".git")))
                (branch-name
                 (when git
                   ;; Determine a reasonable string to show for the
                   ;; current branch. This is actually more or less
                   ;; the same logic as we use for the Radian zsh
                   ;; prompt.
                   (with-temp-buffer
                     ;; First attempt uses symbolic-ref, which returns
                     ;; the branch name if it exists.
                     (call-process "git" nil '(t nil) nil
                                   "symbolic-ref" "HEAD")
                     (if (> (buffer-size) 0)
                         ;; It actually returns something like
                         ;; refs/heads/master, though, so let's try to
                         ;; trim it if possible.
                         (let ((regex "^\\(refs/heads/\\)?\\(.+\\)$")
                               (str (string-trim (buffer-string))))
                           (if (string-match regex str)
                               (match-string 2 str)
                             ;; If it's something weird then just show
                             ;; it literally.
                             str))
                       ;; If symbolic-ref didn't return anything on
                       ;; stdout (we discarded stderr), we probably
                       ;; have a detached head and we should show the
                       ;; abbreviated commit hash (e.g. b007692).
                       (erase-buffer)
                       (call-process "git" nil '(t nil) nil
                                     "rev-parse" "--short" "HEAD")
                       (if (> (buffer-size) 0)
                           (string-trim (buffer-string))
                         ;; We shouldn't get here. Unfortunately, it
                         ;; turns out that we do every once in a
                         ;; while. (I have no idea why.)
                         "???")))))
                (dirty (when git
                         (with-temp-buffer
                           (call-process "git" nil t nil
                                         "status" "--porcelain"
                                         "--ignore-submodules=none")
                           (if (> (buffer-size) 0)
                               "*" "")))))
           (cond
            ((and project-name git)
             (format "  [%s:%s%s]" project-name branch-name dirty))
            (project-name
             (format "  [%s]" project-name))
            ;; This should never happen unless you do something
            ;; perverse like create a version-controlled Projectile
            ;; project whose name is a hyphen, but we want to handle
            ;; it anyway.
            (git
             (format "  [%s%s]" branch-name dirty))))))
    (unless (equal old new)
      (setq radian--mode-line-project-and-branch new)
      (force-mode-line-update))))

;; We will make sure this information is updated after one second of
;; inactivity, for the current buffer.

(defvar radian--mode-line-timer-primary nil)
(defvar radian--mode-line-timer-secondary nil)

(defun radian--compute-mode-line-and-reschedule ()
  (when radian--mode-line-timer-secondary
    (cancel-timer radian--mode-line-timer-secondary))
  (radian--compute-mode-line-project-and-branch)
  (setq radian--mode-line-timer-secondary
        (run-with-idle-timer
         (time-add 1 (current-idle-time)) nil
         #'radian--compute-mode-line-and-reschedule)))

(when radian--mode-line-timer-primary
  (cancel-timer radian--mode-line-timer-primary))

(when radian--mode-line-timer-secondary
  (cancel-timer radian--mode-line-timer-secondary))

(setq radian--mode-line-timer-primary
      (run-with-idle-timer
       1 'repeat #'radian--compute-mode-line-and-reschedule))

(setq-default mode-line-format
              '(;; Show [*] if the buffer is modified.
                (:eval (radian--mode-line-buffer-modified))
                " "
                ;; Show the name of the current buffer.
                mode-line-buffer-identification
                "   "
                ;; Show the row and column of point.
                mode-line-position
                ;; Show the current Projectile project.
                radian--mode-line-project-and-branch
                ;; Show the active major and minor modes.
                "  "
                mode-line-modes))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

(provide 'radian-appearance)

;;; radian-appearance.el ends here
