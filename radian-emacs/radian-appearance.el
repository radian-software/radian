;;; radian-appearance.el --- Non-color-theme appearance tweaks

(require 'radian-bind-key)
(require 'radian-custom)
(require 'radian-package)
(require 'radian-windowed)

;;; This file has appearance tweaks that are unrelated to the color
;;; theme. Menus, scroll bars, bells, cursors, and so on. See also
;;; `radian-theme', which customizes the color theme specifically.

(defcustom radian-window-maximize-on-startup t
  "Non-nil means maximize frames when they are created."
  :type #'boolean
  :group #'radian)

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the contextual menu that pops up when you right-click.
(bind-keys ("<C-mouse-1>" . nil)
           ("<C-down-mouse-1>" . nil))

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

(defcustom radian-font-size 140
  "Default font size, in pixels."
  :type 'integer
  :group 'radian)

(radian-with-windowed-emacs
  ;; Disable the scroll bars.
  (scroll-bar-mode -1)

  ;; Disable the tool bar.
  (tool-bar-mode -1)

  ;; Prevent the cursor from blinking.
  (blink-cursor-mode -1)

  ;; Increase the default font size.
  (set-face-attribute 'default nil :height radian-font-size)

  (when radian-window-maximize-on-startup
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

  ;; Use the same font for fixed-pitch text as the rest of Emacs (you
  ;; *are* using a monospace font, right?).
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))

;; Package `diminish' provides an easy way to change the display of
;; minor modes in the mode line. It provides a single function,
;; `diminish', which can be given a mode name and optionally a
;; replacement string for the mode name, if you do not want to
;; diminish it entirely. Package `use-package' has built-in support
;; for `diminish' via the `:diminish' keyword, so we rarely use
;; `diminish' directly. Note that `diminish' cannot be used to change
;; the appearance of major modes; for that, we have to add a function
;; to the mode hook that sets `mode-name' appropriately. It may be
;; desirable to eventually switch to package `delight', which
;; allegedly allows for customizing the appearance of both minor modes
;; and major modes.
(use-package diminish
  :demand t)

;;; The following code customizes the mode line to something like:
;;; [*] init.el        96% (2410,30)  [radian:develop*]  (Emacs-Lisp AggrIndent SP/s)

(defun radian-mode-line-buffer-modified ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "[*]"
     "   ")
   'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(defvar-local radian-mode-line-project-and-branch nil
  "Mode line construct showing Projectile project and Git status.
The format is [project:branch*], where the * is shown if the
working directory is dirty. Either component can be missing; this
might happen if Projectile is not available or if the project is
not version-controlled with Git. If nothing should be displayed,
this variable is set to nil.

This variable is actually only a cached value; it is set by
`radian-mode-line-compute-project-and-branch' for performance
reasons.")

;; Don't clear the cache when switching major modes (or using M-x
;; normal-mode).
(put 'radian-mode-line-project-and-branch 'permanent-local t)

(defun radian-mode-line-compute-project-and-branch ()
  "Recalculate and set `radian-mode-line-project-and-branch'.
Force a redisplay of the mode line if necessary. This is buffer-local."
  (condition-case-unless-debug err
      (let ((old radian-mode-line-project-and-branch)
            (new
             (let* (;; Don't insist on having Projectile loaded.
                    (project-name (when (featurep 'projectile)
                                    (projectile-project-name)))
                    ;; Projectile returns "-" to mean "no project".
                    ;; I'm still wondering what happens if someone
                    ;; makes a project named "-".
                    (project-name (unless (equal project-name "-")
                                    project-name))
                    ;; Check if we are actually in a Git repo, and Git
                    ;; is available.
                    (git (and
                          (executable-find "git")
                          (locate-dominating-file default-directory ".git")))
                    (branch-name
                     (when git
                       ;; Determine a reasonable string to show for
                       ;; the current branch. This is actually more or
                       ;; less the same logic as we use for the Radian
                       ;; Zsh prompt.
                       (with-temp-buffer
                         ;; First attempt uses symbolic-ref, which
                         ;; returns the branch name if it exists.
                         (call-process "git" nil '(t nil) nil
                                       "symbolic-ref" "HEAD")
                         (if (> (buffer-size) 0)
                             ;; It actually returns something like
                             ;; refs/heads/master, though, so let's
                             ;; try to trim it if possible.
                             (let ((regex "^\\(refs/heads/\\)?\\(.+\\)$")
                                   (str (string-trim (buffer-string))))
                               (if (string-match regex str)
                                   (match-string 2 str)
                                 ;; If it's something weird then just
                                 ;; show it literally.
                                 str))
                           ;; If symbolic-ref didn't return anything
                           ;; on stdout (we discarded stderr), we
                           ;; probably have a detached head and we
                           ;; should show the abbreviated commit hash
                           ;; (e.g. b007692).
                           (erase-buffer)
                           (call-process "git" nil '(t nil) nil
                                         "rev-parse" "--short" "HEAD")
                           (if (> (buffer-size) 0)
                               (string-trim (buffer-string))
                             ;; We shouldn't get here. Unfortunately,
                             ;; it turns out that we do every once in
                             ;; a while. (I have no idea why.)
                             "???")))))
                    (dirty (when git
                             (with-temp-buffer
                               (call-process "git" nil t nil
                                             "status" "--porcelain")
                               (if (> (buffer-size) 0)
                                   "*" "")))))
               (cond
                ((and project-name git)
                 (format "  [%s:%s%s]" project-name branch-name dirty))
                (project-name
                 (format "  [%s]" project-name))
                ;; This should never happen unless you do something
                ;; perverse like create a version-controlled
                ;; Projectile project whose name is a hyphen, but we
                ;; want to handle it anyway.
                (git
                 (format "  [%s%s]" branch-name dirty))))))
        (unless (equal old new)
          (setq radian-mode-line-project-and-branch new)
          (force-mode-line-update)))
    (error
     ;; We should not usually get an error here. In the case that we
     ;; do, however, let's try to avoid displaying garbage data, and
     ;; instead delete the construct entirely from the mode line.
     (unless (null radian-mode-line-project-and-branch)
       (setq radian-mode-line-project-and-branch nil)
       (force-mode-line-update)))))

;;; We will make sure this information is updated after some time of
;;; inactivity, for the current buffer.

(defcustom radian-mode-line-update-delay 1
  "Seconds of inactivity before updating the mode line.
Specifically, this entails updating the Projectile project, Git
branch, and dirty status, which are the most computationally
taxing elements."
  :type '(choice
          (number :tag "Seconds")
          (list :tag "Full time specification"
            (integer :tag "Highest bits of seconds")
            (integer :tag "Lowest bits of seconds")
            (integer :tag "Microseconds")
            (integer :tag "Picoseconds")))
  :group 'radian)

;; We only need one global timer pair for all the buffers, since we
;; will only be updating the cached mode line value for the current
;; buffer.
;;
;; The way this is set up, the main idle timer runs each time that
;; Emacs is idle for exactly one second. That triggers a recomputation
;; of the mode line, and also schedules the repeat timer, which
;; reschedules itself repeatedly. Why do we need two timers? If we
;; tried to use just the idle timer, then the recomputation would only
;; get scheduled once per idle session, one second in, instead of
;; going once per second after one second of initial idleness. If we
;; tried to use just the repeat timer, then we would get
;; ever-increasing delays before it would fire, in each new idle
;; session. Why? Because the pattern for scheduling an idle timer
;; repeatedly is to increase the idle delay, since the idle time is
;; not re-set just because a timer fired. And if the idle session ends
;; between timer fires, then the repeat timer will be stuck with a
;; really long idle delay, and won't fire again.

(defun radian-mode-line-compute-and-reschedule ()
  "Compute mode line data and re-set timers.
The delay is `radian-mode-line-update-delay'. The timers are
`radian-mode-line-idle-timer' and
`radian-mode-line-repeat-timer'."

  ;; Cancel any existing timer (we wouldn't want to introduce
  ;; duplicate timers!), and do it early in a half-hearted attempt to
  ;; avoid race conditions.
  (when radian-mode-line-repeat-timer
    (cancel-timer radian-mode-line-repeat-timer))

  ;; Do the computation.
  (radian-mode-line-compute-project-and-branch)

  ;; If Emacs is already idle (meaning that the main idle timer has
  ;; already been triggered, and won't go again), then we need to
  ;; schedule the repeat timer. Otherwise, the main idle timer will be
  ;; triggered when Emacs does become idle, and we don't need to
  ;; schedule anything. There's no need to clear an old repeat timer,
  ;; since the idle timer will always get called before the repeat
  ;; timer and that will cause the repeat timer to be re-set as below.
  (when (current-idle-time)
    (setq radian-mode-line-repeat-timer
          (run-with-idle-timer
           (time-add (current-idle-time) radian-mode-line-update-delay)
           nil #'radian-mode-line-compute-and-reschedule))))

(defvar radian-mode-line-idle-timer
  (run-with-idle-timer
   radian-mode-line-update-delay 'repeat
   #'radian-mode-line-compute-and-reschedule)
  "Timer that recomputes information for the mode line, or nil.
This runs once each time Emacs is idle.
Future recomputations are scheduled under
`radian-mode-line-repeat-timer'. See also
`radian-mode-line-compute-and-reschedule' and
`radian-mode-line-compute-project-and-branch'.")

(defvar radian-mode-line-repeat-timer nil
  "Timer that recomputes information for the mode line, or nil.
This is scheduled repeatedly at intervals after
`radian-mode-line-idle-timer' runs once. See also
`radian-mode-line-compute-and-reschedule' and
`radian-mode-line-compute-project-and-branch'.")

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode 1)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(;; Show [*] if the buffer is modified.
                (:eval (radian-mode-line-buffer-modified))
                " "
                ;; Show the name of the current buffer.
                mode-line-buffer-identification
                "   "
                ;; Show the row and column of point.
                mode-line-position
                ;; Show the current Projectile project and Git branch.
                radian-mode-line-project-and-branch
                ;; Show the active major and minor modes.
                "  "
                mode-line-modes))

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

(provide 'radian-appearance)

;;; radian-appearance.el ends here
