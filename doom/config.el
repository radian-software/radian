;; -*- lexical-binding: t -*-

;;; help

(map! (:map help-map "K" #'describe-keymap))

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
       ))

;;; editor
;;;; editor/ctrlf

(map! (:map minibuffer-local-map "C-s" nil))
(map! (:map evil-normal-state-map "C-r" nil))

(ctrlf-mode +1)

;;; emacs
;;;; emacs/undo

(map! ("M-/" #'redo)
      (:n "U" #'redo))

;;; ui

(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type 'relative)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;;;; ui/workspaces

(setq +workspaces-on-switch-project-behavior nil)

;;; local config

(load (expand-file-name "config.local.el") 'noerror 'nomessage)
