## Radian git configuration

This doc outlines the special features that are added to git by
Radian's `.gitconfig`.

### Setup

* Run `ln -sT /path/to/radian/git/.* ~/`
* If you want to add further overrides or configuration, create
  `~/.gitconfig.local`; this file is appended to Radian's `.gitconfig`
  and can override values
    * You should put your `user.name` and `user.email` in
      `~/.gitconfig.local`
* To use your own global gitignore file instead of Radian's, set
  `core.excludesfile` in `~/.gitconfig.local` to
  `~/.gitexclude.local`. By default `~/.gitexclude` from Radian is
  used.

### Extra commands

* `git exec CMD...`: Run `CMD` at the root of the current Git
  repository. Just for convenience. The alternative is `(cd "$(git
  rev-parse --show-toplevel)" && CMD...)` which is much more annoying.
* `git root`: Show the hash of the oldest revision in the repository
  (or at least one of them). Faster than paging through all of `git
  log`.

### Significant behavior changes

* Disable most command-line usage hints.
* Disable the macOS Keychain credential caching on macOS. Use the
  built-in `git-credential-cache` instead.
* Configure diffs to always be shown using less, even if they fit on a
  single page. You must press `q` to exit. Adds consistency.
* Show conflicts using three-way diff style, where you can compare
  both parents to the common ancestor (merge base). Improves
  comprehensibility of diffs.
* When pushing, default to pushing a matching branch of the same name
  to remote. Also when pushing, configure upstream so that pulling
  will pull from that same origin branch. Do the same when checking
  out a remote branch. Overall, reduces typing.
* Refuse to pull except as fast-forward; require `--rebase` or merge.

### Enabled features

* Rebase autosquash capability
* Diff mnemonic prefixes
* Diff copy detection
* Submodule commit logs in diffs and status
* Mercurial remote protocol
* Slightly more refspec information on pulls
