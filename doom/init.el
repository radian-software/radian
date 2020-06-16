;; -*- lexical-binding: t -*-

(doom! :completion
       company
       (ivy +prescient)
       :editor
       (evil +everywhere)
       snippets
       :emacs
       dired
       electric
       undo
       vc
       :lang
       cc
       data
       emacs-lisp
       go
       haskell
       java
       javascript
       json
       latex
       markdown
       org
       python
       ruby
       rust
       sh
       web
       yaml
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

(load (expand-file-name "init.local.el") 'noerror 'nomessage)
