;; -*- lexical-binding: t; no-byte-compile: t -*-

(package! apheleia
  :recipe (:host github :repo "raxod502/apheleia")
  :pin "6bd69671796c3d232ffae42df6eecba4eb1f7cd2")

(package! buffer-move
  :recipe (:fork (:repo "raxod502/buffer-move"))
  :pin "25b7a989cf43414559717f87279654305b13e7c9")

(package! ctrlf
  :pin "79377a55df2e39ec0559bd6ec9f2567e8978402e")

(package! transpose-frame
  :pin "12e523d70ff78cc8868097b56120848befab5dbc")

(package! visual-regexp
  :pin "3e3ed81a3cbadef1f1f4cb16f9112a58641d70ca")

(load (expand-file-name "packages.local.el" doom-private-dir)
      'noerror 'nomessage)
