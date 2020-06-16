;; -*- lexical-binding: t -*-

(map! :leader
      :desc "describe-keymap" "hK" #'describe-keymap) h

(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type 'relative)
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(load (expand-file-name "config.local.el") 'noerror 'nomessage)
