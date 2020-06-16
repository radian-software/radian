;; -*- lexical-binding: t -*-

;;; help

(map! (:map help-map "M-k" #'describe-keymap))

;;; completions

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

;;; editor
;;;; editor/ctrlf

(map! (:map minibuffer-local-map "C-s" nil))

(ctrlf-mode +1)

;;;; editor/smartparens

(sp-use-paredit-bindings)

;;; emacs
;;;; emacs/buffers

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;; emacs/undo

(map! ("M-/" #'undo-tree-redo))

;;; tools
;;;; tools/magit

(after! magit

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;;; ui
;;;; ui/graphical

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;;;; ui/line-numbers

(setq display-line-numbers-type 'relative)

;;;; ui/scroll

(setq scroll-conservatively 0)

;;;; ui/theme

(setq doom-theme 'doom-vibrant)

;;;; ui/windmove

(windmove-default-keybindings)

;;;; ui/workspaces

(setq +workspaces-on-switch-project-behavior nil)

;;; local config

(load (expand-file-name "config.local.el") 'noerror 'nomessage)
