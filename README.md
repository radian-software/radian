# Radian

> Not everyone wants to use radians, but one cannot deny that they are
> the most elegant choice...

This repository contains configurations for [Emacs], [Zsh], [Tmux],
[Leiningen], and [Git] based on the philosophy that good
configurations should:

* be easily customizable and extensible, so that people with a wide
  range of preferences can use them easily;
* be highly modular, so that components can be added or removed
  easily;
* contain high-quality, well-documented code, so that every part of
  them can be easily modified; and perhaps most importantly,
* Just Work&trade;.

## Contents

* How to set up Radian (see below)
* [How to use Radian Emacs](docs/emacs/summary.md) -- slightly
  outdated but still useful
* [How to contribute to Radian](CONTRIBUTING.md)

## Setup

There is currently a Python setup script by the name of `setup.py` in
the root of this repository. It does a reasonable job of installing
necessary dependencies; you can choose interactively what you want to
install, and it will ask you before running each command. However, I
consider the current setup script a bit clunky, and I'm in the
progress of developing a new, general-purpose tool called [Dotman] to
handle setup in a much more elegant and Unixy way. Radian's Dotman
configuration is in progress on the `wip/dotman` branch.

In the meantime, you might be able to use the following random hints,
if you want to try using `setup.py`:

* In Emacs, some of the more unusual keyboard shortcuts (such as
  `C-)`) may not work correctly. This is a general problem with
  terminal emulators, unfortunately—getting non-alphabetic keyboard
  combinations to be passed through to programs is very difficult.
  Currently there is no real solution to this problem
  (see [open issue][keys issue]), but you might be able to improve
  your experience somewhat by copying
  the [iTerm2 preferences file from `rally-emacs`][plist], which has
  some patches to improve keybinding support.
* To get copy/paste to integrate with the system clipboard in iTerm2,
  you will need to enable `Applications in terminal may access
  clipboard`
  (see [open issue][settings issue]).
* To get the Meta key to work in iTerm2, you will need to select
  `Left/Right option key acts as +Esc`
  (see [open issue][settings issue]).

[dotman]: https://github.com/raxod502/dotman
[emacs]: https://www.gnu.org/software/emacs/
[git]: https://git-scm.com/
[iterm2]: https://www.iterm2.com/
[keys issue]: https://github.com/raxod502/radian/issues/101
[leiningen]: http://leiningen.org/
[plist]: https://github.com/RallySoftware/rally-emacs/blob/53a7448fb70b1c1b184e78145b0781a19b65300a/pristine/com.googlecode.iterm2.plist
[settings issue]: https://github.com/raxod502/radian/issues/113
[tmux]: https://tmux.github.io/
[zsh]: http://zsh.sourceforge.net/
