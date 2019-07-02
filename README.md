**Radian**: dotfiles that marry elegance and practicality.

## Summary

These dotfiles attempt to achieve the following goals:

* aggressively using best practices, or creating them if none exist
  already
* extensively documenting and commenting all code
* remaining as simple as possible while maximizing usability (in
  particular, not rebinding keys unnecessarily)
* supporting local configuration without the need to fork this
  repository (my local Emacs configuration is almost 750 lines of
  code)

## Software configured, features

* [Emacs]
    * Next-generation package manager, [`straight.el`][straight.el]
    * Clean and DRY package customizations using
      [`use-package`][use-package]
    * Future-proof customizations using [`el-patch`][el-patch]
    * Sorting by frecency and usage on all commands using
      [`prescient.el`][prescient.el]
    * IDE features for expanding library of programming languages with
      [LSP] via [`lsp-mode`][lsp-mode]
    * Informative but minimal mode-line showing file modification
      status, buffer name, point position, current project, Git
      branch, and active modes (with optional right-alignment support)
    * Extremely clean mode lighters thanks to [Blackout]
    * Aggressive startup optimization: 0.7s or less for a fully
      configured graphical frame, from cold boot
    * Aggressively consistent coding style and documentation,
      including heavy use of macros to automate and foolproof common
      operations
    * Delightful color scheme that works in the terminal ([Zerodark])
    * Clipboard, mouse, and PATH integration for macOS
    * Automatic creation and interactive removal of parent directories
      when finding files
    * Extensible system for defining mnemonic key sequences to jump to
      dotfiles
    * Choose to kill, restart, or spawn new Emacs on `C-x C-c`, based
      partly on [`restart-emacs`][restart-emacs]
    * Automatic insertion of whitespace and indentation when pressing
      newline after inserting a pair of delimiters
    * Configured packages: [Atomic Chrome][atomic-chrome] (with
      [Firefox] support), [Autorevert], [buffer-move], [Company],
      [Counsel], [delete-selection-mode], [Dired], [dumb-jump],
      [ElDoc], [ESUP], [Flycheck], [Forge], [git-link], [Helpful],
      [Ivy], [Macrostep], [Magit], [no-littering], [Org],
      [Projectile], [pyvenv], [Smartparens], [Swiper],
      [transpose-frame], [undo-tree], [use-package], [visual-regexp],
      [webpaste.el], and more
    * Supported languages: C/C++, [Clojure], [Haskell], [JavaScript],
      [LaTeX], [Markdown], [Python], [Ruby], [Rust], [TypeScript], and
      more
    * Major modes for editing many configuration file types
* [Zsh]
    * Extremely fast and flexible package manager, [zplugin]
    * No-nonsense prompt showing username, hostname, working
      directory, and Git status, colored by exit code
    * Substring completion everywhere
    * GUI-like file/directory copy/paste functions on the command line
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
##### Emacs

**Minimum version supported: Emacs 25.2**

    $ brew cask install emacs

Install support tools. Python is required for environment variable
setting; watchexec is optional for improved startup time.

    $ brew install python watchexec

##### Zsh

    $ brew install zsh
    $ mkdir ~/.zplugin
    $ git clone git@github.com:zdharma/zplugin.git ~/.zplugin/bin

##### Remaining tools

* Tmux: `brew install tmux`
* Git: `brew install git`
* Leiningen: `brew cask install java`; `brew install leiningen`

#### Arch Linux

I use [Yay](https://github.com/Jguer/yay) to install AUR packages. If
you prefer something different, substitute to taste.

* Emacs: `pacman -S emacs python`; (optional for improved startup
  time) `yay -S watchexec`
  * LSP
    * Bash: `yarn global add bash-language-server`
    * C/C++: `pacman -S clang`
    * Flow: `yarn global add flow-bin`
    * Go: `go get -u golang.org/x/tools/gopls` and add `$GOPATH/bin`
      to your `$PATH`
    * Javascript/TypeScript:

          $ git clone git@github.com:sourcegraph/javascript-typescript-langserver.git
          $ cd javascript-typescript-langserver
          $ npm install
          $ npm run build
          $ chmod +x lib/language-server-stdio.js
          $ ln -s $PWD/lib/language-server-stdio.js ~/.local/bin/javascript-typescript-stdio

    * Python: `yay -S microsoft-python-language-server`
* Zsh:

      $ pacman -S zsh
      $ mkdir ~/.zplugin
      $ git clone git@github.com:zdharma/zplugin.git ~/.zplugin/bin

* Tmux: `pacman -S tmux`
* Git: `pacman -S git`
* Leiningen:

      $ pacman -S jdk8-openjdk
      $ yay -S leiningen

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
    ./shell/zsh/.zprofile => ~/.zprofile
    ./tmux/.tmux.conf => ~/.tmux.conf

Do not attempt to use the `emacs` subdirectory of this repository as
`user-emacs-directory`; it won't work.

### Installing local configuration

* Emacs: `~/.emacs.d/init.local.el`,
  `~/.emacs.d/straight/versions/radian-local.el`
* All shells: `~/.profile.local`
* Zsh: `~/.zshrc.local`
* Tmux: `~/.tmux.local.conf`
* Git: `~/.gitconfig.local`

I suggest versioning your local dotfiles in a separate repository, and
symlinking them to the appropriate locations. This is what I do.

### Tips and tricks

In order to get `$PATH`, `ssh-agent`, and `gpg-agent` working
correctly in graphical applications on macOS, use
`scripts/patch-macos-app.zsh`. Note however that Emacs already sources
`~/.profile` (and thereby `~/.profile.local`) during startup.

## Contributing

Please feel free to contribute in any way that you would like. If you
find a bug or have a question about how to use Radian, [report
it][issues]. If you want to contribute code, [please do][prs]. (Try to
follow the style of the surrounding code.)

### Reading the source code

Please do! It will probably be informative in one way or another. The
goal is that *absolutely everything* should be either obvious or
commented.

[alembic]: https://github.com/pallet/alembic
[atomic-chrome]: https://github.com/alpha22jp/atomic-chrome
[autorevert]: https://www.emacswiki.org/emacs/AutoRevertMode
[blackout]: https://github.com/raxod502/blackout
[buffer-move]: https://github.com/lukhas/buffer-move
[clojure]: https://clojure.org/
[company-statistics]: https://github.com/company-mode/company-statistics
[company]: http://company-mode.github.io/
[counsel]: https://github.com/abo-abo/swiper#counsel
[delete-selection-mode]: https://www.emacswiki.org/emacs/DeleteSelectionMode
[dired]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
[dotman]: https://github.com/raxod502/dotman
[dumb-jump]: https://github.com/jacktasia/dumb-jump
[easypg]: https://www.gnu.org/software/emacs/manual/epa.html
[el-patch]: https://github.com/raxod502/el-patch
[eldoc]: https://www.emacswiki.org/emacs/ElDoc
[emacs]: https://www.gnu.org/software/emacs/
[esup]: https://github.com/jschaf/esup
[exa]: https://the.exa.website/
[firefox]: https://www.mozilla.org/en-US/firefox/
[flx]: https://github.com/lewang/flx
[flycheck]: http://www.flycheck.org/
[forge]: https://github.com/magit/forge
[git]: https://git-scm.com/
[git-link]: https://github.com/sshaw/git-link
[haskell]: https://www.haskell.org/
[helpful]: https://github.com/Wilfred/helpful
[historian]: https://github.com/PythonNut/historian.el
[issues]: https://github.com/raxod502/radian/issues
[ivy]: https://github.com/abo-abo/swiper#ivy
[javascript]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[latex]: https://www.latex-project.org/
[leiningen]: http://leiningen.org/
[lsp]: https://langserver.org/
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[macrostep]: https://github.com/joddie/macrostep
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
[pyvenv]: https://github.com/jorgenschaefer/pyvenv
[racket]: https://racket-lang.org/
[reattach-to-user-namespace]: https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard
[restart-emacs]: https://github.com/iqbalansari/restart-emacs
[ruby]: https://www.ruby-lang.org/
[rust]: https://www.rust-lang.org/
[smartparens]: https://github.com/Fuco1/smartparens
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
[swiper]: https://github.com/abo-abo/swiper#swiper
[tmux]: https://tmux.github.io/
[tools.namespace]: https://github.com/clojure/tools.namespace
[transpose-frame]: https://www.emacswiki.org/emacs/TransposeFrame
[typescript]: https://www.typescriptlang.org/
[undo-tree]: http://www.dr-qubit.org/undo-tree.html
[use-package]: https://github.com/jwiegley/use-package
[vinyasa]: http://docs.caudate.me/lucidity/
[visual-regexp]: https://github.com/benma/visual-regexp.el
[webpaste.el]: https://github.com/etu/webpaste.el
[yasnippet]: https://github.com/joaotavora/yasnippet
[zerodark]: https://github.com/NicolasPetton/zerodark-theme
[zplugin]: https://github.com/zdharma/zplugin
[zsh]: http://zsh.sourceforge.net/
