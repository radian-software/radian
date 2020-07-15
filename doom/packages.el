;; -*- lexical-binding: t; no-byte-compile: t -*-

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia")
  :pin "6bd69671796c3d232ffae42df6eecba4eb1f7cd2")

(package! buffer-move
  :recipe (:fork (:repo "raxod502/buffer-move"))
  :pin "25b7a989cf43414559717f87279654305b13e7c9")

(package! company-quickhelp
  :pin "2dda13403c49221cc98e87b4bbf8168436f27560")

(package! crontab-mode
  :pin "9625228cbfce29ac3b443c6eff893ff828268f7d")

(package! ctrlf
  :pin "79377a55df2e39ec0559bd6ec9f2567e8978402e")

(package! pkgbuild-mode
  :pin "bc22301198b3f581d89352510e847454fb1cb9ff")

(package! ssh-config-mode
  :pin "8fda737131fef2e55c4965a938cf907acbf03372")

(package! transpose-frame
  :pin "12e523d70ff78cc8868097b56120848befab5dbc")

(package! visual-regexp
  :pin "3e3ed81a3cbadef1f1f4cb16f9112a58641d70ca")

(load (expand-file-name "packages.local.el" doom-private-dir)
      'noerror 'nomessage)
