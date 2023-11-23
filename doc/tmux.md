## Radian tmux configuration

This doc outlines the special features that are added to tmux by
Radian's `.tmux.conf`.

### Setup

* Run `ln -sT /path/to/radian/tmux/.tmux.conf ~/.tmux.conf`
* If you want to add further overrides or configuration, create
  `~/.tmux.local.conf`; this file is loaded at the end of Radian's
  `.tmux.conf`
* On macOS, install
  [reattach-to-user-namespace](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)
  for clipboard to work properly

### Standard fixes

* Compatibility improvements for terminal keybindings and colors
* Mouse integration
* Fix clipboard integration on macOS
* Decrease lag passing the ESC key to child processes

### Behavior changes

* Default prefix key is backtick (`` ` ``) instead of control-B. This
  makes switching windows much faster.
* Number windows and panes from 1 instead of from 0, because with the
  new prefix key switching to window 0 is unergonomic.
* Automatically renumber windows when removing one. Windows will
  therefore always be numbered 1 through (number of windows), and
  won't have gaps.
* Open new windows and panes in the same working directory as the
  current one, instead of always in the default.

### New keybindings

* `prefix R` - reload `.tmux.conf`
* `prefix \` - swap the current and marked windows (use `prefix m` to
  mark or unmark a window)
* `prefix <` and `prefix >` - swap the current window with the one to
  the left or right, respectively
* `prefix I` - prompt for an index (1 or greater) and insert a window
  before that numbered window

### Appearance tweaks

* New status bar. It uses nicer colors and different styling to make
  it a lot more obvious what window is currently selected.
* Status bar is updated once per second, instead of once per 15
  seconds. This means that new-activity indicators will show up in a
  more timely manner.
