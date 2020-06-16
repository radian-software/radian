;; -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Editing
;;;; Text wrap

;; Doom sets this to 80, an increase from the default of 70. I
;; personally find that a bridge too far, because everybody else's
;; Emacs is configured to a default of 70 and it's perfectly
;; serviceable. We should encourage a standard.
(setq-default fill-column 70)

;;;; Delimiter manipulation

;; Bind M-(, M-", C-<left>, C-<right>, M-<up>, M-<down>, M-r, M-s,
;; M-j, M-S, M-?. 
(sp-use-paredit-bindings)

;;;; Undo/redo

(map! ("M-/" #'undo-tree-redo))

;;; Navigation
;;;; Scrolling

;; Doom configures this variable to a large value, which has the
;; effect of making it so that when you do a search and the result is
;; off-screen, Emacs only scrolls just enough to bring it to the edge
;; of the screen (where, of course, you can barely see it). Reset the
;; option so that Emacs will instead scroll point to the center of the
;; window.
(setq scroll-conservatively 0)

;;;; Search

;; By default Doom binds C-s to `counsel-minibuffer-history', which
;; conflicts with CTRLF.
(map! (:map minibuffer-local-map "C-s" nil))

;; Enable globally. This remaps C-s, C-r, C-M-s, C-M-r as well as some
;; additional Isearch bindings.
(ctrlf-mode +1)

;;;; Projects

;; Do not use separate workspaces for each project. We do not want our
;; window configuration to keep changing unexpectedly when we switch
;; projects.
(setq +workspaces-on-switch-project-behavior nil)

;;; Interface
;;;; Margins

;; We have no need of line numbers.
(setq display-line-numbers-type nil)

;;;; Buffers

;; By default this is bound to `+ivy/switch-workspace-buffer'. We
;; don't use workspaces, however.
(map! ("C-x b" #'ivy-switch-buffer))

;;;; Windows

;; Bind S-<left>, S-<right>, S-<up>, S-<down>.
(windmove-default-keybindings)

;;;; Frames

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;;;; Color theme

(setq doom-theme 'doom-vibrant)

;;; Code intelligence
;;;; Autocompletion

;; By default, while a Company popup is active it steals most of your
;; keyboard inputs. This is not good user experience as it prevents
;; you (for example) from opening a new line by pressing RET. We bind
;; all of those problematic keys back to nil in the
;; `company-active-map', and establish two canonical ways to interact
;; with Company:
;;
;; (1) Accept the first autocompletion by pressing TAB.
;; (2) Accept a different one by pressing M-1 through M-9.
;;
;; If neither of these suffice, you should just type more to narrow
;; the candidates.
(map! (:map company-active-map
       "<down>" nil
       "<return>" nil
       "<up>" nil
       "C-M-s" nil
       "C-SPC" nil
       "C-d" nil
       "C-j" nil
       "C-k" nil
       "C-n" nil
       "C-p" nil
       "C-s" nil
       "C-u" nil
       "M-n" nil
       "M-p" nil
       "RET" nil
       "<tab>" #'company-complete-selection
       "TAB" #'company-complete-selection))

;;; Introspection
;;;; Help

;; Establish binding for `describe-keymap', which otherwise is not
;; accessible.
(map! (:map help-map "M-k" #'describe-keymap))

;;; Applications
;;;; Version control

;; Add some missing flags to various Magit commands.
(after! magit

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow"))

  (transient-append-suffix 'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;;; Local configuration

(load (expand-file-name "config.local.el") 'noerror 'nomessage)

;; Local Variables:
;; fill-column: 70
;; indent-tabs-mode: nil
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:
