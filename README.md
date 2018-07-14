**Radian**: dotfiles that marry elegance and practicality.

## Summary

I write my dotfiles with an eye to:

* extensive documentation and commentary within and outside of the
  code
* harnessing the full power of software while staying true to its
  original spirit (and keybindings!)
* first-class support for local customization without the need for
  forking

## Software configured, features

* [Emacs]
    * Next-generation package manager, [`straight.el`][straight.el]
    * Future-proof customizations using [`el-patch`][el-patch]
    * Sorting by frecency and usage on all commands using
      [`prescient.el`][prescient.el]
    * Informative but minimal mode-line showing file modification
      status, buffer name, point position, current project, Git
      branch, and active modes
    * Extremely clean mode lighters
    * Delightful color scheme that works in the terminal
    * Clipboard, mouse, and PATH integration with macOS
    * Automatic creation and interactive removal of parent directories
      when finding files
    * Extensible system for defining mnemonic key sequences to jump to
      dotfiles
    * Emacsclient as a Git commit message editor
    * Lots of packages for pushing around windows and buffers in
      interesting ways
    * Configured packages: [aggressive-indent-mode], [Autorevert],
      [Company], [Counsel], [delete-selection-mode], [EasyPG],
      [ElDoc], [Flycheck], [Ivy], [Magit], [no-littering], [Org],
      [Projectile], [Smartparens], [Swiper], [undo-tree],
      [use-package], [visual-regexp], [YASnippet] (... and dozens
      more, but these are the biggest ones)
    * Supported languages: C/C++, [Clojure], JavaScript, [LaTeX],
      [Markdown], [Python], [Racket], [Ruby], [Rust], [TypeScript]
      (... and dozens more, but these are the biggest ones with the
      most sophisticated support)
* [Zsh]
    * Next-generation package manager, [zplug]
    * No-nonsense prompt showing working directory and Git status,
      colored by exit code
    * Substring completion everywhere
    * Aliases for [Exa], a modern replacement for `ls`
    * GUI-like copy/paste functions on the command line
    * Extensive library of clean and consistent [Git] aliases
    * Colored man pages
* [Tmux]
    * Keybindings for inserting new windows and shifting them left and
      right
    * No-nonsense but stylish status bar à la [powerline] but without
      the dependencies
    * Spectacular hack to leverage [reattach-to-user-namespace] on
      macOS with minimal side effects
* [Git]
    * Create a repository and a root commit all at once
    * Rename stashes
    * Reword a commit while preserving the index
    * Alias and unalias without messing with `git config`
    * More helpful output from `git status`, submodules, and more
* [Leiningen]
    * Modular profile system allowing maximal performance through
      selecting exactly the features you want
    * Inject useful utility functions using [Vinyasa]
    * Pull JARs from Maven and hotload them into your REPL with
      [Alembic]
    * Refresh a dirty REPL without a restart, using [tools.namespace]
    * Colorization and pretty-printing of output and stack traces in
      the REPL

## Installation

Setup is in three parts: installing the software, installing the
configuration, and optionally installing local configuration.

### Installing software
#### macOS

* Emacs: `brew install emacs --with-cocoa`, version 26.1 minimum.
* Zsh: `brew install zsh`; `brew install zplug`
* Tmux: `brew install tmux`
* Git: `brew install git`
* Leiningen: `brew cask install java`; `brew install leiningen`

#### Arch Linux

* Emacs: `pacman -S emacs`
* Zsh: `pacman -S zsh`
* Tmux: `pacman -S tmux`
* Git: `pacman -S git`
* Leiningen: `pacman -S jdk8-openjdk`; install `leiningen` from AUR

### Installing configuration

Use symbolic links:

    ./emacs/init.el => ~/.emacs.d/init.el
    ./emacs/versions.el => ~/.emacs.d/straight/versions/radian.el
    ./git/.gitconfig => ~/.gitconfig
    ./git/.gitexclude => ~/.gitexclude
    ./leiningen/profiles.clj => ~/.lein/profiles.clj
    ./shell/bash/.bashrc => ~/.bashrc
    ./shell/shared/.profile => ~/.profile
    ./shell/zsh/.zshrc => ~/.zshrc
    ./tmux/.tmux.conf => ~/.tmux.conf

### Installing local configuration

* Emacs: `~/.emacs.d/init.local.el`,
  `~/.emacs.d/straight/versions/radian-local.el`
* All shells: `~/.profile.local`
* Zsh: `~/.zshrc.local`
* Tmux: `~/.tmux.local.conf`
* Git: `~/.gitconfig.local`

### Tips and tricks

In order to get `$PATH`, `ssh-agent`, and `gpg-agent` working
correctly in graphical applications on macOS, use
`scripts/patch-macos-app.zsh`.

## Contributing

Please feel free to contribute in any way that you would like. If you
find a bug or have a question about how to use Radian, [report
it][issues]. If you want to contribute code, [please do][prs]. (See
the [style guide][style] and [design pattern cheatsheet][patterns].)

### Reading the source code

Please do! It will probably be informative in one way or another. The
goal is that *absolutely everything* has a comment, no exceptions.
There are a couple of things that are done often enough that it would
be silly to repeat the same comment over and over again, so these
patterns are instead documented in the [design pattern
cheatsheet][patterns].

[patterns]: docs/patterns.md
[style]: docs/style.md

[1.0]: https://github.com/raxod502/radian/milestone/1
[aggressive-indent-mode]: https://github.com/Malabarba/aggressive-indent-mode
[alembic]: https://github.com/pallet/alembic
[autorevert]: https://www.emacswiki.org/emacs/AutoRevertMode
[clojure]: https://clojure.org/
[company-statistics]: https://github.com/company-mode/company-statistics
[company]: http://company-mode.github.io/
[counsel]: https://github.com/abo-abo/swiper#counsel
[delete-selection-mode]: https://www.emacswiki.org/emacs/DeleteSelectionMode
[dotman]: https://github.com/raxod502/dotman
[easypg]: https://www.gnu.org/software/emacs/manual/epa.html
[el-patch]: https://github.com/raxod502/el-patch
[eldoc]: https://www.emacswiki.org/emacs/ElDoc
[emacs]: https://www.gnu.org/software/emacs/
[exa]: https://the.exa.website/
[flx]: https://github.com/lewang/flx
[flycheck]: http://www.flycheck.org/
[git]: https://git-scm.com/
[historian]: https://github.com/PythonNut/historian.el
[issues]: https://github.com/raxod502/radian/issues
[ivy]: https://github.com/abo-abo/swiper#ivy
[latex]: https://www.latex-project.org/
[leiningen]: http://leiningen.org/
[magit]: https://magit.vc/
[markdown-mode]: http://jblevins.org/projects/markdown-mode/
[markdown]: https://daringfireball.net/projects/markdown/syntax
[no-littering]: https://github.com/tarsius/no-littering
[org]: http://orgmode.org/
[powerline]: https://github.com/powerline/powerline
[prescient.el]: https://github.com/raxod502/prescient.el
[projectile]: http://batsov.com/projectile/
[prs]: https://github.com/raxod502/radian/pulls
[python]: https://www.python.org/
[racket]: https://racket-lang.org/
[reattach-to-user-namespace]: https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard
[ruby]: https://www.ruby-lang.org/
[rust]: https://www.rust-lang.org/
[smartparens]: https://github.com/Fuco1/smartparens
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
[swiper]: https://github.com/abo-abo/swiper#swiper
[tmux]: https://tmux.github.io/
[tools.namespace]: https://github.com/clojure/tools.namespace
[typescript]: https://www.typescriptlang.org/
[undo-tree]: http://www.dr-qubit.org/undo-tree.html
[use-package]: https://github.com/jwiegley/use-package
[vinyasa]: http://docs.caudate.me/lucidity/
[visual-regexp]: https://github.com/benma/visual-regexp.el
[yasnippet]: https://github.com/joaotavora/yasnippet
[zplug]: https://github.com/zplug/zplug
[zsh]: http://zsh.sourceforge.net/
