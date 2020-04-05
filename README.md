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

If you are a fan of my Emacs packages (such as
[`straight.el`][straight.el], [Selectrum], [CTRLF],
[`prescient.el`][prescient.el], [Apheleia], [Blackout]) then you will
find all of them configured here.

Note that there is a `master` branch which is not updated as
frequently. You may be interested in running this branch if you desire
more stability.

## Software configured, features

* [Emacs] (**minimum version supported: 25.2**)
  * Next-generation package manager, [`straight.el`][straight.el]
  * Clean and DRY package customizations using
    [`use-package`][use-package]
  * Simpler and less buggy (than [Ivy], [Counsel], [Helm]) file and
    command selection using [Selectrum]
  * More robust and streamlined single-buffer text search (than
    [Isearch], [Swiper]) using [CTRLF]
  * Sorting by frecency and usage on all commands using
    [`prescient.el`][prescient.el]
  * IDE features for expanding library of programming languages with
    [LSP] via [`lsp-mode`][lsp-mode] (Bash, C, C++, CSS, [Flow], [Go],
    [Haskell], HTML, [JavaScript], [TypeScript], [JSX]/[TSX], [Flow],
    [LaTeX], [Python] with [Poetry] and [Pipenv] virtualenvs
    autodetected)
  * Automatic asynchronous code reformatting without moving point
    using [Black], [Brittany], [Gofmt], and [Prettier] via [Apheleia]
  * Informative but minimal mode-line showing file modification
    status, buffer name, point position, and active modes (with
    optional right-alignment support)
  * Extremely clean mode lighters with prettier names thanks to
    [Blackout]
  * *All* the needless messages have been suppressed; no errors mean
    no messages
  * Aggressive startup optimization: as fast as 0.33s for a fully
    configured graphical frame (by aggressive lazy-loading of
    everything; using [`el-patch`][el-patch] to lazy-load packages
    that weren't designed to be lazy-loaded; by extensive use of idle
    timers; by disabling of heavy autoloads; by asynchronous
    byte-compilation of the init-file in a subprocess on successful
    init, with the local init-file macroexpanded and embedded directly
    into Radian during compilation; and by running all customizations
    before the first graphical frame is initialized)
  * Aggressively consistent coding style and documentation (init-file
    is 37% comments and docstrings), including heavy use of macros to
    automate and foolproof common operations
  * Future-proof customizations using [`el-patch`][el-patch]
  * Delightful color scheme that works in the terminal ([Zerodark])
  * Clipboard, mouse, and PATH integration for macOS and Linux
  * Automatic creation and interactive removal of parent directories
    when finding and renaming files
  * Automatically clone Emacs source when needed by `find-function`
  * Extensible system for defining mnemonic key sequences to jump to
    dotfiles
  * Choose to kill, restart, or spawn new Emacs on `C-x C-c`, based
    partly on [`restart-emacs`][restart-emacs]
  * Automatic insertion of whitespace and indentation when pressing
    newline after inserting a pair of delimiters
  * Global auto-fill configured to activate only in comments,
    docstrings, and text
  * Configured packages: [Atomic Chrome][atomic-chrome] (with
    [Firefox] support), [Autorevert], [buffer-move], [Company],
    [delete-selection-mode], [Dired], [dumb-jump], [ElDoc], [ESUP],
    [Flycheck], [Forge],
    [`git-gutter-fringe.el`][git-gutter-fringe.el], [git-link],
    [Helpful], [Macrostep], [Magit], [no-littering], [Org],
    [Projectile], [pyvenv], [`rg.el`][rg.el], [Smartparens],
    [transpose-frame], [undo-tree], [use-package], [visual-regexp],
    [webpaste.el], and more
  * Major modes for editing many languages and configuration file
    types
  * [Tested on CircleCI](https://circleci.com/gh/raxod502/radian) with
    [Docker] configuration included for all supported Emacs versions
* [Zsh]
  * Extremely fast and flexible package manager, [zplugin]
  * No-nonsense prompt showing username, hostname, working
    directory, and Git status, colored by exit code
  * Substring completion everywhere
  * GUI-like file/directory copy/paste functions on the command line
  * Extensive library of clean and consistent [Git] aliases
  * Colored man pages
  * Configured plugins: [wdx], [zsh-autosuggestions],
    [zsh-history-substring-search], [zsh-completions]
* [Tmux]
  * Keybindings for inserting new windows and shifting them left and
    right
  * No-nonsense but stylish status bar à la [powerline] but without
    the dependencies
  * Spectacular hack to leverage [reattach-to-user-namespace] on
    macOS with minimal side effects
* [Git]
  * Create a repository and a root commit all at once
  * Alias and unalias without messing with `git config`
  * More helpful output from `git status`, submodules, and more

## Installation

Setup is in three parts: installing the software, installing the
configuration, and optionally installing local configuration.

### Installing software
#### macOS

* Emacs: `brew install bash python`; `brew cask install emacs`;
  (optional for improved startup time) `brew install watchexec`
  * Code intelligence
    * Bash: `yarn global add bash-language-server`, but see [this
      issue](https://github.com/mads-hartmann/bash-language-server/issues/131).
    * C/C++: `brew install llvm`
    * Flow: `brew install flow`
    * Go: `go get -u golang.org/x/tools/gopls` and add `$GOPATH/bin`
      to your `$PATH`
    * Haskell: (please help with documentation!)
    * HTML: `yarn global add vscode-html-languageserver-bin`
    * JavaScript/TypeScript: `yarn global add typescript
      typescript-language-server`; `brew install prettier`
    * LaTeX: `yay -S digestif`
    * Python: the language server is downloaded automatically courtesy
      of [`lsp-python-ms`](https://github.com/emacs-lsp/lsp-python-ms).

          $ brew install black

* Zsh:

      $ brew install zsh
      $ mkdir ~/.zplugin
      $ git clone git@github.com:zdharma/zplugin.git ~/.zplugin/bin

* Tmux: `brew install tmux`
* Git: `brew install git`

#### Arch/Manjaro Linux

I use [Yay](https://github.com/Jguer/yay) to install AUR packages. If
you prefer something different, substitute to taste.

* Emacs: `pacman -S emacs python`; (optional for improved startup
  time) `yay -S watchexec`
  * Code intelligence
    * Bash: `yarn global add bash-language-server`
    * C/C++: `pacman -S clang`
    * Flow: `yarn global add flow-bin`
    * Go: `go get -u golang.org/x/tools/gopls` and add `$GOPATH/bin`
      to your `$PATH`
    * Haskell: `yay -S haskell-ide-engine`
    * HTML: `yay -S vscode-html-languageserver-bin`
    * JavaScript/TypeScript: `pacman -S prettier`; `yay -S
      typescript-language-server-bin`
    * LaTeX:

          $ wget -O ~/.local/bin/texlab.jar
              https://github.com/latex-lsp/texlab/releases/download/v0.4.2/texlab.jar

    * Python: the language server is downloaded automatically courtesy
      of [`lsp-python-ms`](https://github.com/emacs-lsp/lsp-python-ms).

          $ pacman -S python-black

* Zsh:

      $ pacman -S zsh
      $ mkdir ~/.zplugin
      $ git clone git@github.com:zdharma/zplugin.git ~/.zplugin/bin

* Tmux: `pacman -S tmux`
* Git: `pacman -S git`

### Installing configuration

Use symbolic links:

    ./emacs/init.el => ~/.emacs.d/init.el
    ./emacs/early-init.el => ~/.emacs.d/early-init.el
    ./emacs/versions.el => ~/.emacs.d/straight/versions/radian.el
    ./git/.gitconfig => ~/.gitconfig
    ./git/.gitexclude => ~/.gitexclude
    ./shell/bash/.bashrc => ~/.bashrc
    ./shell/shared/.profile => ~/.profile
    ./shell/zsh/.zshrc => ~/.zshrc
    ./shell/zsh/.zprofile => ~/.zprofile
    ./tmux/.tmux.conf => ~/.tmux.conf

Do not attempt to use the `emacs` subdirectory of this repository as
`user-emacs-directory`; it won't work.

### Installing local configuration

* Emacs: `~/.emacs.d/init.local.el` (local configuration) and
  `~/.emacs.d/straight/versions/radian-local.el` (optional, local
  lockfile for `straight.el`; will be created when you run `M-x
  straight-freeze-versions`)
* All shells: `~/.profile.local`
* Zsh: `~/.zshrc.local`
* Tmux: `~/.tmux.local.conf`
* Git: `~/.gitconfig.local`

I suggest versioning your local dotfiles in a separate repository, and
symlinking them to the appropriate locations. This is what I do.

Here is what your `init.local.el` should probably look like:

    ;; code that should be run at the very beginning of init, e.g.

    (setq radian-font ...)
    (setq radian-font-size ...)

    (radian-local-on-hook before-straight

      ;; code that should be run right before straight.el is bootstrapped,
      ;; e.g.

      (setq straight-vc-git-default-protocol ...)
      (setq straight-check-for-modifications ...))

    (radian-local-on-hook after-init

      ;; code that should be run at the end of init, e.g.

      (use-package ...))

    ;; see M-x customize-group RET radian-hooks RET for which hooks you
    ;; can use with `radian-local-on-hook'

You don't have to worry about byte-compiling your local init-file;
Radian actually macroexpands it and embeds it directly into the
byte-compiled Radian init-file. Using the macro `radian-local-on-hook`
instead of defining functions and adding them to Radian's hooks
manually enables some magic that makes this actually work properly.

## Contributing

Please feel free to contribute in any way that you would like. If you
find a bug or have a question about how to use Radian, [report
it][issues]. If you want to contribute code, [please do][prs]. (Try to
follow the style of the surrounding code.)

### Reading the source code

Please do! It will probably be informative in one way or another. The
goal is that *absolutely everything* should be either obvious or
commented.

[apheleia]: https://github.com/raxod502/apheleia
[atomic-chrome]: https://github.com/alpha22jp/atomic-chrome
[autorevert]: https://www.emacswiki.org/emacs/AutoRevertMode
[black]: https://github.com/python/black
[blackout]: https://github.com/raxod502/blackout
[buffer-move]: https://github.com/lukhas/buffer-move
[company-statistics]: https://github.com/company-mode/company-statistics
[company]: http://company-mode.github.io/
[counsel]: https://github.com/abo-abo/swiper#counsel
[ctrlf]: https://github.com/raxod502/ctrlf
[delete-selection-mode]: https://www.emacswiki.org/emacs/DeleteSelectionMode
[dired]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
[docker]: https://www.docker.com/
[dotman]: https://github.com/raxod502/dotman
[dumb-jump]: https://github.com/jacktasia/dumb-jump
[easypg]: https://www.gnu.org/software/emacs/manual/epa.html
[el-patch]: https://github.com/raxod502/el-patch
[eldoc]: https://www.emacswiki.org/emacs/ElDoc
[emacs]: https://www.gnu.org/software/emacs/
[esup]: https://github.com/jschaf/esup
[exa]: https://the.exa.website/
[firefox]: https://www.mozilla.org/en-US/firefox/
[flow]: https://flow.org/
[flow]: https://flow.org/
[flx]: https://github.com/lewang/flx
[flycheck]: http://www.flycheck.org/
[forge]: https://github.com/magit/forge
[git-gutter-fringe.el]: https://github.com/syohex/emacs-git-gutter-fringe
[git-link]: https://github.com/sshaw/git-link
[git]: https://git-scm.com/
[go]: https://golang.org/
[gofmt]: https://golang.org/cmd/gofmt/
[haskell]: https://www.haskell.org/
[helm]: https://github.com/emacs-helm/helm
[helpful]: https://github.com/Wilfred/helpful
[historian]: https://github.com/PythonNut/historian.el
[isearch]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html
[issues]: https://github.com/raxod502/radian/issues
[ivy]: https://github.com/abo-abo/swiper#ivy
[javascript]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[jsx]: https://reactjs.org/docs/introducing-jsx.html
[latex]: https://www.latex-project.org/
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[lsp]: https://langserver.org/
[macrostep]: https://github.com/joddie/macrostep
[magit]: https://magit.vc/
[no-littering]: https://github.com/tarsius/no-littering
[org]: http://orgmode.org/
[pipenv]: https://docs.pipenv.org/en/latest/
[poetry]: https://poetry.eustace.io/
[powerline]: https://github.com/powerline/powerline
[prescient.el]: https://github.com/raxod502/prescient.el
[prettier]: https://github.com/prettier/prettier
[projectile]: http://batsov.com/projectile/
[prs]: https://github.com/raxod502/radian/pulls
[python]: https://www.python.org/
[pyvenv]: https://github.com/jorgenschaefer/pyvenv
[racket]: https://racket-lang.org/
[reattach-to-user-namespace]: https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard
[restart-emacs]: https://github.com/iqbalansari/restart-emacs
[rg.el]: https://github.com/dajva/rg.el
[selectrum]: https://github.com/raxod502/selectrum
[smartparens]: https://github.com/Fuco1/smartparens
[smex]: https://github.com/nonsequitur/smex
[straight.el]: https://github.com/raxod502/straight.el
[swiper]: https://github.com/abo-abo/swiper#swiper
[tmux]: https://tmux.github.io/
[transpose-frame]: https://www.emacswiki.org/emacs/TransposeFrame
[tsx]: https://www.typescriptlang.org/docs/handbook/jsx.html
[typescript]: https://www.typescriptlang.org/
[undo-tree]: http://www.dr-qubit.org/undo-tree.html
[use-package]: https://github.com/jwiegley/use-package
[visual-regexp]: https://github.com/benma/visual-regexp.el
[wdx]: https://github.com/raxod502/wdx
[webpaste.el]: https://github.com/etu/webpaste.el
[yasnippet]: https://github.com/joaotavora/yasnippet
[zerodark]: https://github.com/NicolasPetton/zerodark-theme
[zplugin]: https://github.com/zdharma/zplugin
[zsh-autosuggestions]: https://github.com/zsh-users/zsh-autosuggestions
[zsh-completions]: https://github.com/zsh-users/zsh-completions
[zsh-history-substring-search]: https://github.com/zsh-users/zsh-history-substring-search
[zsh]: http://zsh.sourceforge.net/
