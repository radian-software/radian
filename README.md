# Radian

> Not everyone wants to use radians, but one cannot deny that they are
> the most elegant choice...

This repository contains configurations for [Emacs], [Zsh], [Tmux],
[Leiningen], and [Git] based on the philosophy that good
configurations should:

- be easily customizable and extensible, so that people with a wide
  range of preferences can use them easily;
- be highly modular, so that components can be added or removed
  easily;
- contain high-quality, well-documented code, so that every part of
  them can be easily modified; and perhaps most importantly,
- Just Work&trade;.

## Contents

- Information on how to set up Radian (see below)
- Information on how to use Emacs
  (see [`reference/EMACS.md`](reference/EMACS.md))
- Information on how to contribute to Radian
  (see [`CONTRIBUTING.md`](CONTRIBUTING.md))

## Setup on OS X

Radian includes a setup script that will take you 95% of the way from
a fresh install of OS X to a fully working setup. Simply download or
clone this repository and run `scripts/setup.sh`. By default, this
will set up everything: Emacs, Zsh, Tmux, Leiningen, Git, and other
miscellaneous tools. If you are not interested in one or more of these
items, you can run the setup script as—for instance—`scripts/setup.sh
exclude leiningen git`. Alternatively, if you are only interested in a
few of the items, you can include only a few of them: for instance,
`scripts/setup.sh only emacs tmux`.

Depending on your system, the setup script may take a while to do its
thing the first time you run it. (For instance, it may have to
download the JDK for Leiningen.) However, it is designed to only do
things that need to be done. In other words, it will not attempt to
install a tool if an appropriate version of the tool is already
available. This means that, to some extent, you can use the setup
script as an "arbitrary problem fixer"—if you have accidentally
deleted a program or broken a symlink, you can run `setup.sh` to
automatically detect and fix what is wrong.

Depending on your system and what tools you want to use, you may—for
now—have to do the following steps manually:

- If you want to use Tmux, you will need to install
  a [powerline-patched font][fonts] and select it in your terminal
  emulator (Terminal.app or [iTerm2]). I
  use [Ubuntu Mono derivative Powerline Bold][font].
- In Emacs, some of the more unusual keyboard shortcuts (such as
  `C-)`) may not work correctly. This is a general problem with
  terminal emulators, unfortunately—getting non-alphabetic keyboard
  combinations to be passed through to programs is very difficult.
  Currently there is no real solution to this problem
  (see [open issue][keys issue]), but you might be able to improve
  your experience somewhat by copying
  the [iTerm2 preferences file from `rally-emacs`][plist], which has
  some patches to improve keybinding support.
- To get copy/paste to integrate with the system clipboard in iTerm2,
  you will need to enable `Applications in terminal may access
  clipboard`
  (see [open issue][settings issue]).
- To get the Meta key to work in iTerm2, you will need to select
  `Left/Right option key acts as +Esc`
  (see [open issue][settings issue]).

Although the setup script is extremely good at fixing whatever mess
your system might be in, there are a few things to watch out for:

- It's not easy to correctly reset the shell environment, so the setup
  script doesn't try. This means that if your environment variables
  are configured oddly, they will stay that way and you will have to
  open a new shell if you want them to be cleared.
- If you already have Emacs packages installed, the setup script has
  no way of checking if they are outdated. You might want to run `M-x
  list-packages` to check if there are any updates available.

## Setup on Linux

A setup script for Linux is coming soon. In the meantime, you can
symlink any of the dotfiles you want into your home directory. Simply
look at `setup.sh` to see what versions of what programs are expected
to be installed, and where symlinks are created.

## Setup on Windows

There is currently no support for Windows and no plan to add it, but
because of the modular design of Radian, this would probably not be
too difficult a task. We are accepting pull requests!

[emacs]: https://www.gnu.org/software/emacs/
[zsh]: http://zsh.sourceforge.net/
[tmux]: https://tmux.github.io/
[leiningen]: http://leiningen.org/
[git]: https://git-scm.com/
[fonts]: https://github.com/powerline/fonts
[iterm2]: https://www.iterm2.com/
[font]: https://github.com/powerline/fonts/tree/master/UbuntuMono
[keys issue]: https://github.com/raxod502/radian/issues/101
[plist]: https://github.com/RallySoftware/rally-emacs/blob/53a7448fb70b1c1b184e78145b0781a19b65300a/pristine/com.googlecode.iterm2.plist
[settings issue]: https://github.com/raxod502/radian/issues/113
