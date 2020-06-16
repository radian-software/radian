;; -*- lexical-binding: t -*-

(setq doom-theme 'doom-vibrant)
(setq display-line-numbers-type 'relative)

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(load (expand-file-name "config.local.el") 'noerror 'nomessage)
