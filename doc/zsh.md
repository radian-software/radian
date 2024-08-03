## Radian zsh configuration

This doc outlines the special features that are added to zsh by
Radian's `.zshrc`.

### Setup

* Run `ln -sT /path/to/radian/*/.* ~/`
* If you want to add further overrides or configuration, create
  `~/.zshrc.local` and/or `~/.profile.local`; these are evaluated
  after Radian's `~/.zshrc` and `~/.profile` respectively
    * Note: `~/.zshrc.local` is loaded early during init, but you can
      provide hooks to influence the rest of the load -
      `radian_after_init_hook` for overriding things at the end, or
      `radian_zinit_hook` for adding plugins
* Suggested usage is to set environment variables in
  `~/.profile.local` and only define things needed for interactive
  usage (e.g. aliases) in `~/.zshrc.local`

### Dependencies

* zsh (obviously)
* git (to clone zinit)
* eza (optional - colorized ls aliases)

### Plugin management

Radian uses
[zdharma-continuum/zinit](https://github.com/zdharma-continuum/zinit)
as a unified plugin manager for Zsh. The plugin manager is installed
user-locally to `~/.local/share/zinit` on first shell startup as long
as Git is available.

The following plugins are installed by default:

* [radian-software/wdx](https://github.com/radian-software/wdx)
* [marlonrichert/zsh-autocomplete](https://github.com/marlonrichert/zsh-autocomplete)
* [zsh-users/zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
* [zsh-users/zsh-completions](https://github.com/zsh-users/zsh-completions)

If you want to add further plugins, you can define a
`radian_zinit_hook` function in your `~/.zshrc.local` and include
`zplugin light` lines in there.

Completions setup is handled automatically, including for user-added
plugins.

### Behavior changes - command line

* Force usage of Emacs keybindings in Zsh line editor regardless of
  the setting of `$EDITOR`. Note that Radian does not configure
  `$EDITOR`, you can set this in `~/.profile.local` if desired
* Allow comments in the interactive shell (start with `#`)
* Allow escaping single quotes in single-quoted strings, `echo 'It''s
  me!'` is shorthand for `echo 'It'"'"'s me!'` and both print `It's
  me!`
* Disable flow control (`ctrl+s` and `ctrl+q` have their normal Emacs
  meaning rather than freezing and unfreezing command output)

### Behavior changes - completion

* Completions (`TAB`) are displayed in a menu below the command line,
  you can cycle through them with `TAB` and `Shift+TAB` or exit the
  menu with `ctrl+g`
* Completions are based on path-aware substring matching, so
  `~/foo/bar<tab>` will expand to any file in `~/foo` that includes
  `bar` as a substring in its name; non-path arguments or custom
  completions also use substring matching
* Globs (`*`) are case-insensitive, match dotfiles by default, and
  sort numerically instead of lexicographically
* History expansion (`!`) is disabled, use `ctrl+r` provided by
  zsh-autocomplete

### Behavior changes - command history

* History is effectively unlimited both within a shell session and in
  `~/.zsh_history`
* History is shared between all sessions with some limitations for
  performance. If you ran a command in one session and you want to get
  it in your history for another session, reload zsh config (period +
  enter) to get it there.
* Don't save commands to history if they start with a space, use this
  for sensitive commands. For other commands, remove superfluous
  spaces before saving to history file.

### Behavior changes - navigation

* See [wdx](https://github.com/radian-software/wdx) for an easy way to
  navigate to well-known directories on your system. Warp point file
  is at `~/.config/wdx/points`, not backed up or replicated by
  default. Radian aliases `wd` for wdx.
* When you cd to a directory, its name is resolved to the canonical
  form (so, symlinks are expanded). This differs from the traditional
  behavior.
* You can cd to a directory (absolute or relative) by entering it as a
  command name. This also works for `-`, `-2`, ... `-9`. The latter
  are adjusted to index from the most recent end of the directory
  stack rather than the beginning, and the directory stack is updated
  on every cd by default. Print directory stack with `ds`

### Commands and aliases

* Type `.` and press enter to reload zsh config (`exec zsh`)
* Use `help` as a replacement for `man` that can also look up shell
  functions and specific subsections of certain man pages. Man pages
  are colored by default
* ls aliases: `l` for `ls -lAhF` and `lt` for `tree -a`, but using eza
  or gnu ls if installed. `lt <n>` for only recursing to the nth
  level (ignore `.git`, `.svn`, `node_modules` by default), and `lti
  <glob>` for ignoring more patterns. Use `ltli <n> <glob>` to combine
  both modes. `lg` for getting a grid instead of a list
* wd aliases: `wd` for `wdx`, `ws` for `wdx set`, `wsf` for `wdx set
  -f`
* filesystem clipboard: `copy <path>...` to record a list of
  filenames, then cd somewhere else and `paste [<path>]` or `move
  [<path>]` to execute `cp` or `mv` respectively on the "copied" paths
  and/or the path passed to `paste` or `move`. Note that `paste` and
  `move` shadow builtin commands, use `command paste` or `command
  move` if they are needed. `pasteln` is the same but uses `ln -s`
* `delink <path>` to replace a symlink with a copy of what it points
  to, or `transpose <path1> <path2>` to swap two files
  (non-atomically)
* `md` is `mkdir -p`, `rd` is `rmdir`, `t` is `trash` (must be
  installed separately, there are different distributions)
* emacs aliases: `e`/`ew` for terminal and graphical Emacs. `eq`/`eqw`
  for `emacs -Q`. `ue`/`uew` for setting `USER_EMACS_DIRECTORY` to
  current directory, for a clean environment. `ec`/`ecw` for
  emacsclient
* git aliases: many, covering most of the existing commands and
  commonly used flags, especially for `git log` - check the source for
  a list
* tmux aliases - `ts` for `tmux new-session`, `ta` for `tmux attach`
* vim aliases - `v` for nvim or vim or vi

### Appearance tweaks

* Modified prompt - includes username and hostname, abbreviated
  working directory, shows git info if installed (branch + dirty
  status), and is colorized based on the last command's return code
