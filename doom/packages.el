;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Bump to get https://github.com/jrblevin/markdown-mode/pull/544.
(package! markdown-mode
  :pin "152eae2415258141043e559af97d37a72de6c4e5")

;; Fork to get https://github.com/iqbalansari/restart-emacs/pull/19.
(package! restart-emacs
  :recipe (:fork (:repo "raxod502/restart-emacs" :branch "fork/1"))
  :pin "174fee595af82d1fb860a44ab5d30bfc1c48adbf")

;; New packages

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia")
  :pin "6bd69671796c3d232ffae42df6eecba4eb1f7cd2")

(package! atomic-chrome
  :pin "a505f638866f9e7b913784be0dc84f338e9ad449")

(package! buffer-move
  :recipe (:fork "raxod502")
  :pin "25b7a989cf43414559717f87279654305b13e7c9")

(package! company-quickhelp
  :pin "2dda13403c49221cc98e87b4bbf8168436f27560")

(package! crontab-mode
  :pin "9625228cbfce29ac3b443c6eff893ff828268f7d")

(package! ctrlf
  :pin "79377a55df2e39ec0559bd6ec9f2567e8978402e")

(package! git-link
  :pin "1dbabfed4c5c3c5ac6ffa9035a9c3d4c6cc7f885")

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
