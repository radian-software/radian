;; -*- lexical-binding: t -*-

(doom! :completion
       company
       (ivy +prescient)
       :emacs
       dired
       electric
       (undo +tree)
       vc
       :lang
       (cc +lsp)
       data
       emacs-lisp
       (go +lsp)
       (haskell +lsp)
       (java +lsp)
       (javascript +lsp)
       (json +lsp)
       (latex +lsp)
       (markdown +grip)
       org
       (python +lsp +poetry)
       (ruby +lsp)
       (rust +lsp)
       (sh +lsp)
       (web +css +html)
       (yaml +lsp)
       :tools
       debugger
       docker
       editorconfig
       (eval +overlay)
       gist
       lookup
       lsp
       macos
       magit
       make
       terraform
       tmux
       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       modeline
       ophints
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe
       workspaces
       :config
       (default +bindings +smartparens))

(load (expand-file-name "init.local.el" doom-private-dir)
      'noerror 'nomessage)
