""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""" Keyboard

" Prefix all of Radian's custom keybindings with <space>, because it's
" very easy to type.
let mapleader=" "

" When a prefix or leader key is pressed, wait indefinitely for further input,
" instead of timing out after one second. Taken from [1].
"
" [1]: http://stackoverflow.com/a/26657284/3538165
set notimeout
set ttimeout

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""" Appearance

" Show line numbers relative to the cursor in the left-hand column.
set relativenumber

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""" Finding files

""" Create keybindings for visiting Radian's dotfiles.

"" Emacs
nnoremap <leader>eei :edit ~/.emacs.d/init.el<cr>
nnoremap <leader>eeb :edit ~/.emacs.d/init.before.local.el<cr>
nnoremap <leader>eel :edit ~/.emacs.d/init.local.el<cr>

"" Emacs (deprecated)
nnoremap <leader>eepr :edit ~/.emacs.d/init.pre.local.el<cr>
nnoremap <leader>eepo :edit ~/.emacs.d/init.post.local.el<cr>

"" Git
nnoremap <leader>egc :edit ~/.gitconfig<cr>
nnoremap <leader>ege :edit ~/.gitexclude<cr>
nnoremap <leader>egl :edit ~/.gitconfig.local<cr>

"" Leiningen
nnoremap <leader>elp :edit ~/.lein/profiles.clj<cr>

"" Tmux
nnoremap <leader>etc :edit ~/.tmux.conf<cr>
nnoremap <leader>etl :edit ~/.tmux.local.conf<cr>

"" Vim
nnoremap <leader>evi :edit ~/.config/nvim/init.vim<cr>

"" Zsh
nnoremap <leader>ezr :edit ~/.zshrc<cr>
nnoremap <leader>eza :edit ~/.zshrc.antigen.local<cr>
nnoremap <leader>ezb :edit ~/.zshrc.before.local<cr>
nnoremap <leader>ezl :edit ~/.zshrc.local<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""" VimScript

" Add a keybinding for reloading this file (init.vim).
nnoremap <leader>r :source ~/.config/nvim/init.vim<cr>
