;; -*- lexical-binding: t -*-

(map! (:map help-map "K" #'describe-keymap))

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

(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type 'relative)
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(load (expand-file-name "config.local.el") 'noerror 'nomessage)
