# Clojure

There is support for syntax highlighting and indentation of Clojure
code, but most of the commands only work once you have a REPL running.
Start a REPL with `C-c M-j`. For ClojureScript projects, you can start
Clojure and ClojureScript REPLs simultaneously with `C-c M-J`.

Once you have a REPL open, the following commands are useful:

| Command       | Functionality                                               |
| ------------- | ----------------------------------------------------------- |
| `C-c C-d C-d` | Show documentation for a symbol                             |
| `C-c C-d C-j` | Show Javadoc documentation for a symbol                     |
| `C-c C-d C-r` | Show Grimoire documentation for a symbol                    |
| `C-c C-d C-w` | Show online Grimoire documentation for a symbol             |
| `M-.`         | Show the definition for a symbol                            |
| `C-x e`       | Evaluate the form before the cursor                         |
| `C-M-x`       | Evaluate the current top-level form                         |
| `C-c C-v C-r` | Evaluate the current selection                              |
| `C-c C-v C-n` | Evaluate the `ns` form for the current namespace            |
| `C-c C-k`     | Evaluate the current buffer                                 |
| `C-c M-n`     | Switch namespaces                                           |
| `C-c M-z`     | Evaluate the current buffer and move the cursor to the REPL |
| `C-u C-c M-z` | Equivalent to `C-c M-n C-c M-z`                             |
| `C-c C-t C-t` | Run the current test                                        |
| `C-c C-t C-n` | Run all tests in the current namespace                      |
| `C-c C-t C-p` | Run all tests in the project                                |

These commands are only relevant in the REPL:

| Command   | Functionality                                      |
| --------- | -------------------------------------------------- |
| `C-c C-c` | Interrupt evaluation                               |
| `C-c C-o` | Clear the last output (long outputs can lag Emacs) |
| `C-c C-q` | Kill the REPL                                      |

Finally, you can use `M-p` and `M-n` to bring up past entries at the
REPL. If you have already typed part of a query, then they will only
show you entries that begin with what you have already typed.
